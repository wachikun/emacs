;;; emoji.el --- Inserting emojis  -*- lexical-binding:t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Lars Ingebrigtsen <larsi@gnus.org>
;; Keywords: fun

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'transient)

(defgroup emoji nil
  "Inserting Emojist."
  :version "29.1"
  :group 'play)

(defface emoji-list-header-face
  '((default :weight bold :inherit variable-pitch))
  "Face for web pages with invalid certificates."
  :version "29.1")

(defvar emoji--labels nil)
(defvar emoji--derived nil)
(defvar emoji--names (make-hash-table :test #'equal))

;;;###autoload
(defun emoji-insert (&optional text)
  "Choose and insert an emoji glyph.
If TEXT (interactively, the prefix), use a textual search instead
of a visual interface."
  (interactive "P")
  (emoji--init)
  (if text
      (emoji--choose-emoji)
    (funcall (intern "emoji-command-Emoji"))))

(defvar emoji--insert-buffer)

;;;###autoload
(defun list-emojis ()
  "List emojis and insert the one that's selected.
The character will be inserted into the buffer that was selected
when the command was issued."
  (interactive)
  (let ((buf (current-buffer)))
    (emoji--init)
    (pop-to-buffer (get-buffer-create "*Emoji*"))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (emoji-list-mode)
      (setq-local emoji--insert-buffer buf)
      (emoji--list-generate nil (cons nil emoji--labels))
      (goto-char (point-min)))))

(defun emoji--list-generate (name alist)
  (let ((width (/ (window-width) 3))
        (mname (pop alist)))
    (if (consp (car alist))
        ;; Recurse.
        (mapcar (lambda (elem)
                  (emoji--list-generate (if name
                                            (concat name " " mname)
                                          mname)
                                        elem))
                alist)
      ;; Output this block of emojis.
      (insert (propertize (concat name " " mname)
                          'face 'emoji-list-header-face)
              "\n\n")
      (cl-loop for i from 1
               for char in alist
               do (insert
                   (propertize
                    char
                    'emoji-glyph char
                    'help-echo
                    (or (gethash char emoji--names)
                        (get-char-code-property (aref char 0) 'name))))
               (when (zerop (mod i width))
                 (insert "\n")))
      (insert "\n\n"))))

(defvar-keymap emoji-list-mode-map
  ["RET"] #'emoji-list-select
  ["<mouse-2>"] #'emoji-list-select
  [follow-link] 'mouse-face)

(define-derived-mode emoji-list-mode special-mode "Emoji"
  "Mode to display emojis."
  :interactive nil
  (setq-local truncate-lines t))

(defun emoji-list-select (event)
  "Select the emoji under point."
  (interactive (list last-nonmenu-event) emoji-list-mode)
  (mouse-set-point event)
  (let ((glyph (get-text-property (point) 'emoji-glyph)))
    (unless glyph
      (error "No emoji under point"))
    (let ((derived (gethash glyph emoji--derived))
          (end-func
           (lambda ()
             (let ((buf emoji--insert-buffer))
               (quit-window)
               (if (buffer-live-p buf)
                   (switch-to-buffer buf)
                 (error "Buffer disappeared"))))))
      (if (not derived)
          (progn
            (funcall end-func)
            (insert glyph))
        (funcall
         (emoji--define-transient (cons "Choose Emoji" (cons glyph derived))
                                  nil end-func))))))

(defun emoji--init ()
  (setq transient-use-variable-pitch t)
  ;; Remove debugging.
  (unless (and nil emoji--labels)
    (setq emoji--derived (make-hash-table :test #'equal))
    (emoji--parse-labels)
    (emoji--parse-normal-derived)
    (emoji--parse-zwj-derived)
    (emoji--define-transient)))

(defun emoji--parse-labels ()
  (setq emoji--labels nil)
  (with-temp-buffer
    (insert-file-contents (expand-file-name "../admin/unidata/labels.txt"
                                            data-directory))
    ;; The format is "[...] ; Main ; sub".
    (while (re-search-forward "^\\[\\([^]]+\\)\\][ \t]*;[ \t]*\\(.*?\\)[ \t]*;[ \t]*\\(.*\\)$" nil t)
      (let ((ranges (match-string 1))
            (main (match-string 2))
            (sub (match-string 3)))
        (emoji--add-characters
         (cl-loop with range-start
                  and set
                  and prev
                  for char across ranges
                  ;; a-z
                  if (eql char ?-)
                  do (setq range-start (1+ prev))
                  else if (and set (eql char ?}))
                  collect (prog1
                              (apply #'string (cdr (nreverse set)))
                            (setq set nil))
                  ;; {set}
                  else if (or (eql char ?{) set)
                  do (push char set)
                  else
                  append (if range-start
                             (prog1
                                 (mapcar #'string
                                         (number-sequence range-start char))
                               (setq range-start nil))
                           (list (string char)))
                  do (setq prev char))
         main sub)))
    ;; Finally split up the too-long lists.
    (emoji--split-long-lists emoji--labels)))

(defun emoji--parse-zwj-derived ()
  (with-temp-buffer
    (let ((table (make-hash-table :test #'equal)))
      (insert-file-contents (expand-file-name
                             "../admin/unidata/emoji-zwj-sequences.txt"
                             data-directory))
      ;; The format is "[...] ; Main ; sub".
      (while (re-search-forward "RGI_Emoji_ZWJ_Sequence[ \t]+;[ \t]+\\(.*?\\)[ \t]+#.*(\\([^)]+\\))"
                                nil t)
        (let* ((name (match-string 1))
               (glyph (match-string 2))
               (base (replace-regexp-in-string ":.*" "" name)))
          (if (equal base name)
              ;; New base.
              (setf (gethash base table) (list glyph)
                    (gethash glyph emoji--names) name)
            ;; Add derived to the base.
            (unless (gethash base table)
              (let ((char (gethash (upcase base) (ucs-names))))
                ;; FIXME -- These are things like "man lifting weights".
                ;;(unless char (message "No %s in `ucs-names'" base))
                (setf (gethash base table) (list char))))
            (setf (gethash base table)
                  (nconc (gethash base table) (list glyph))))
          ;; Map "woman police officer: light skin tone" to "police
          ;; officer", too.
          (setf (gethash (substring glyph 0 1) emoji--derived)
                (append (gethash (substring glyph 0 1) emoji--derived)
                        (list glyph)))))
      ;; Finally create the mapping from the base glyphs to derived ones.
      (maphash (lambda (_k v)
                 (setf (gethash (car v) emoji--derived)
                       (cdr v)))
               table))))

(defun emoji--parse-normal-derived ()
  (with-temp-buffer
    (let ((case-fold-search t))
      (insert-file-contents (expand-file-name
                             "../admin/unidata/emoji-sequences.txt"
                             data-directory))
      (unless (re-search-forward "^# RGI_Emoji_Modifier_Sequence" nil t)
        (error "Can't find RGI_Emoji_Modifier_Sequence"))
      (forward-line 2)
      (while (looking-at "\\([[:xdigit:]]+\\) +\\([[:xdigit:]]+\\)")
        (let ((parent (string (string-to-number (match-string 1) 16)))
              (modifier (string (string-to-number (match-string 2) 16))))
          (setf (gethash parent emoji--derived)
                (append (gethash parent emoji--derived)
                        (list (concat parent modifier)))))
        (forward-line 1)))))

(defun emoji--add-characters (chars main sub)
  (let ((subs (if (member sub '( "cat-face" "monkey-face" "skin-tone"
                                 "country-flag" "subdivision-flag"
                                 "award-medal" "musical-instrument"
                                 "book-paper" "other-object"
                                 "transport-sign" "av-symbol"
                                 "other-symbol"))
                  (list sub)
                (split-string sub "-")))
        parent elem)
    ;; This category is way too big; split it up.
    (when (equal main "Smileys & People")
      (setq main
            (if (member (car subs) '("face" "cat-face" "monkey-face"))
                "Smileys"
              (capitalize (car subs))))
      (when (and (equal (car subs) "person")
                 (= (length subs) 1))
        (setq subs (list "person" "age")))
      (when (and (= (length subs) 1)
                 (not (string-search "-" (car subs))))
        (setq subs nil)))
    (when (equal (car subs) "person")
      (pop subs))
    ;; Useless category.
    (unless (member main '("Skin-Tone"))
      (unless (setq parent (assoc main emoji--labels))
        (setq emoji--labels (append emoji--labels
                                    (list (setq parent (list main))))))
      (setq elem parent)
      (while subs
        (unless (setq elem (assoc (car subs) parent))
          (nconc parent (list (setq elem (list (car subs))))))
        (pop subs)
        (setq parent elem))
      (nconc elem
             (cl-loop for char in chars
                      collect (if (and (= (length char) 1)
                                       (eq (aref char-script-table (elt char 0))
                                           'symbol))
                                  ;; If itʼs not in the 'emoji script you need
                                  ;; the VS-16. Itʼs an emoji, but it
                                  ;; has Emoji_Presentation = No.
                                  ;; Donʼt ask.  Add VARIATION
                                  ;; SELECTOR-16.
                                  (concat char (string  #xfe0f))
                                char))))))

(defun emoji--define-transient (&optional alist inhibit-derived
                                          end-function)
  (unless alist
    (setq alist (cons "Emoji" emoji--labels)))
  (let* ((mname (pop alist))
         (name (intern (format "emoji-command-%s" mname)))
         (has-subs (consp (cadr alist)))
         (layout
          (if has-subs
              ;; Define sub-maps.
              (cl-loop for entry in (emoji--compute-prefix alist)
                       collect (list
                                (car entry)
                                (emoji--compute-name (cdr entry))
                                (emoji--define-transient
                                 (cons (concat mname " " (cadr entry))
                                       (cddr entry)))))
            ;; Insert an emoji.
            (cl-loop for char in alist
                     for i in (append (number-sequence ?a ?z)
                                      (number-sequence ?A ?Z)
                                      (number-sequence ?0 ?9)
                                      (number-sequence ?! ?/))
                     collect (let ((this-char char))
                               (list
                                (string i)
                                char
                                (let ((derived
                                       (and (not inhibit-derived)
                                            (gethash char emoji--derived))))
                                  (if derived
                                      ;; We have a derived glyph, so add
                                      ;; another level.
                                      (emoji--define-transient
                                       (cons (concat mname " " char)
                                             (cons char derived))
                                       'no-derived)
                                    ;; Insert the emoji.
                                    (lambda ()
                                      (interactive)
                                      (when end-function
                                        (funcall end-function))
                                      (insert this-char)))))))))
         (args (apply #'vector mname
                      (emoji--columnize layout
                                        (if has-subs 2 10)))))
    ;; There's probably a better way to do this...
    (setf (symbol-function name)
          (lambda ()
            (interactive)
            (transient-setup name)))
    (pcase-let ((`(,class ,slots ,suffixes ,docstr ,_body)
                 (transient--expand-define-args (list args))))
       (put name 'interactive-only t)
       (put name 'function-documentation docstr)
       (put name 'transient--prefix
            (apply (or class 'transient-prefix) :command name
                   slots))
       (put name 'transient--layout
            (cl-mapcan (lambda (s) (transient--parse-child name s))
                       suffixes)))
    name))

(defun emoji--columnize (list columns)
  (cl-loop with length = (ceiling (/ (float (length list)) columns))
           for i upto columns
           for part on list by (lambda (l) (nthcdr length l))
           collect (apply #'vector (seq-take part length))))

(defun emoji--compute-prefix (alist)
  "Compute characters to use for entries in ALIST.
We prefer the earliest unique letter."
  (cl-loop with taken = (make-hash-table)
           for entry in alist
           for name = (car entry)
           collect (cons (cl-loop for char across name
                                  do (setq char (downcase char))
                                  while (gethash char taken)
                                  finally (progn
                                            (setf (gethash char taken) t)
                                            (cl-return (string char))))
                         entry)))

(defun emoji--compute-name (entry)
  "Add example emojis to the name."
  (let ((name (concat (car entry) " "))
        (children (emoji--flatten entry)))
    (cl-loop for i from 0 upto 20
             while (< (length name) 18)
             do (cl-loop for child in children
                         for char = (elt child i)
                         while (< (length name) 18)
                         when char
                         do (setq name (concat name char))))
    (if (= (length name) 20)
        (concat name "…")
      name)))

(defun emoji--flatten (alist)
  (pop alist)
  (if (consp (cadr alist))
      (cl-loop for child in alist
               append (emoji--flatten child))
    (list alist)))

(defun emoji--split-long-lists (alist)
  (let ((whole alist))
    (pop alist)
    (if (consp (cadr alist))
        ;; Descend.
        (cl-loop for child in alist
                 do (emoji--split-long-lists child))
      ;; We have a list.
      (when (length> alist 77)
        (setcdr whole
                (cl-loop for prefix from ?a
                         for bit on alist by (lambda (l) (nthcdr 77 l))
                         collect (cons (concat (string prefix) "-group")
                                       (seq-take bit 77))))))))

(defun emoji--choose-emoji ()
  (let ((names (make-hash-table :test #'equal)))
    (dolist (section (emoji--flatten (cons "Emoji" emoji--labels)))
      (dolist (char section)
        (when-let ((name (or (gethash char emoji--names)
                             (get-char-code-property (aref char 0) 'name))))
          (setf (gethash (downcase name) names) char))))
    (let* ((name (completing-read "Emoji: " names nil t))
           (glyph (gethash name names))
           (derived (gethash glyph emoji--derived)))
      (if (not derived)
          (insert glyph)
        (funcall
         (emoji--define-transient
          (cons "Choose Emoji" (cons glyph derived))))))))

(provide 'emoji)

;;; emoji.el ends here
