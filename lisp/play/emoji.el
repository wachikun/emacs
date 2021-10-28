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

(require 'cl-lib)
(require 'cl-extra)
(require 'transient)

(defgroup emoji nil
  "Inserting Emojist."
  :version "29.1"
  :group 'play)

(defface emoji-list-header
  '((default :weight bold :inherit variable-pitch))
  "Face for emoji list headers."
  :version "29.1")

(defface emoji
  '((t :height 2.0))
  "Face used when displaying an emoji."
  :version "29.1")

(defface emoji-with-derivations
  '((((background dark))
     (:background "#202020" :inherit emoji))
    (((background light))
     (:background "#e0e0e0" :inherit emoji)))
  "Face for emojis that have derivations."
  :version "29.1")

(defvar emoji--labels nil)
(defvar emoji--derived nil)
(defvar emoji--names (make-hash-table :test #'equal))
(defvar emoji--done-derived nil)
(defvar emoji--recent (list "😀" "😖"))
(defvar emoji--insert-buffer)

;;;###autoload
(defun emoji-insert (&optional text)
  "Choose and insert an emoji glyph.
If TEXT (interactively, the prefix), use a textual search instead
of a visual interface."
  (interactive "*P")
  (emoji--init)
  (if text
      (emoji--choose-emoji)
    (unless (fboundp 'emoji--command-Emoji)
      (emoji--define-transient))
    (funcall (intern "emoji--command-Emoji"))))

;;;###autoload
(defun emoji-recent ()
  "Choose and insert a recently used emoji glyph."
  (interactive "*")
  (emoji--init)
  (unless (fboundp 'emoji--command-Emoji)
    (emoji--define-transient))
  (funcall (emoji--define-transient
            (cons "Recent" emoji--recent) t)))

;;;###autoload
(defun emoji-search ()
  "Choose and insert an emoji glyph by searching for an emoji name."
  (interactive "*")
  (emoji--init)
  (emoji--choose-emoji))

;;;###autoload
(defun emoji-list ()
  "List emojis and insert the one that's selected.
The character will be inserted into the buffer that was selected
when the command was issued."
  (interactive "*")
  (let ((buf (current-buffer)))
    (emoji--init)
    (switch-to-buffer (get-buffer-create "*Emoji*"))
    ;; Don't regenerate the buffer if it already exists -- this will
    ;; leave point where it was the last time it was used.
    (when (zerop (buffer-size))
      (let ((inhibit-read-only t))
        (emoji-list-mode)
        (setq-local emoji--insert-buffer buf)
        (emoji--list-generate nil (cons nil emoji--labels))
        (goto-char (point-min))))))

(defun emoji--list-generate (name alist)
  (let ((width (/ (window-width) 5))
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
                          'face 'emoji-list-header)
              "\n\n")
      (cl-loop for i from 1
               for char in alist
               do (insert
                   (propertize
                    (emoji--fontify-char char)
                    'emoji-glyph char
                    'help-echo (emoji--name char)))
               (when (zerop (mod i width))
                 (insert "\n")))
      (insert "\n\n"))))

(defun emoji--fontify-char (char &optional inhibit-derived)
  (propertize char 'face
              (if (and (not inhibit-derived)
                       (or (null emoji--done-derived)
                           (not (gethash char emoji--done-derived)))
                       (gethash char emoji--derived))
                  'emoji-with-derivations
                'emoji)))

(defun emoji--name (char)
  (or (gethash char emoji--names)
      (get-char-code-property (aref char 0) 'name)))

(defvar-keymap emoji-list-mode-map
  ["RET"] #'emoji-list-select
  ["<mouse-2>"] #'emoji-list-select
  "h" #'emoji-list-help
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
            (emoji--add-recent glyph)
            (funcall end-func)
            (insert glyph))
        (let ((emoji--done-derived (make-hash-table :test #'equal)))
          (setf (gethash glyph emoji--done-derived) t)
          (funcall
           (emoji--define-transient (cons "Choose Emoji" (cons glyph derived))
                                    nil end-func)))))))

(defun emoji-list-help ()
  "Say what the emoji under point is."
  (interactive nil emoji-list-mode)
  (let ((glyph (get-text-property (point) 'emoji-glyph)))
    (unless glyph
      (error "No emoji under point"))
    (let ((name (emoji--name glyph)))
      (if (not name)
          (error "Unknown name")
        (message "%s" name)))))

(defun emoji--init (&optional force inhibit-adjust)
  ;; Remove debugging.
  (when (or (not emoji--labels)
            force)
    (unless force
      (ignore-errors (require 'emoji-labels)))
    ;; The require should define the variable, but in case the .el
    ;; file doesn't exist (yet), parse the file now.
    (when (or force
              (not emoji--labels))
      (setq emoji--derived (make-hash-table :test #'equal))
      (emoji--parse-emoji-test))
    (unless inhibit-adjust
      (emoji--adjust-displayable (cons "Emoji" emoji--labels)))))

(defun emoji--adjust-displayable (alist)
  "Remove glyphs we don't have fonts for."
  (if (consp (caddr alist))
      (dolist (child (cdr alist))
        (emoji--adjust-displayable child))
    (setcdr alist (seq-filter (lambda (glyph)
                                (not (symbolp (char-displayable-p
                                               (elt glyph 0)))))
                              (cdr alist)))))

(defun emoji--parse-emoji-test ()
  (setq emoji--labels nil)
  (with-temp-buffer
    (insert-file-contents (expand-file-name "../admin/unidata/emoji-test.txt"
                                            data-directory))
    (unless (re-search-forward "^# +group:" nil t)
      (error "Can't find start of data"))
    (beginning-of-line)
    (setq emoji--names (make-hash-table :test #'equal))
    (let ((derivations (make-hash-table :test #'equal))
          (case-fold-search t)
          group subgroup)
      (while (not (eobp))
        (cond
         ((looking-at "# +group: \\(.*\\)")
          (setq group (match-string 1)
                subgroup nil))
         ((looking-at "# +subgroup: \\(.*\\)")
          (setq subgroup (match-string 1)))
         ((looking-at
           "\\([[:xdigit:] \t]+\\); *\\([^ \t]+\\)[ \t]+#.*?E[.0-9]+ +\\(.*\\)")
          (let* ((codes (match-string 1))
                 (qualification (match-string 2))
                 (name (match-string 3))
                 (base (emoji--base-name name derivations))
                 (glyph (mapconcat
                         (lambda (code)
                           (string (string-to-number code 16)))
                         (split-string codes))))
            ;; Special-case flags.
            (when (equal base "flag")
              (setq base name))
            ;; Register all glyphs to that we can look up their names
            ;; later.
            (setf (gethash glyph emoji--names) name)
            ;; For the interface, we only care about the fully qualified
            ;; emojis.
            (when (equal qualification "fully-qualified")
              (when (equal base name)
                ;; "People & Body" is very large; split it up.
                (if (equal group "People & Body")
                    (if (or (string-match "\\`person" subgroup)
                            (equal subgroup "family"))
                        (emoji--add-character
                         glyph "People"
                         (if (equal subgroup "family")
                             (list subgroup)
                           (cdr (emoji--split-subgroup subgroup))))
                      (emoji--add-character
                       glyph "Body" (emoji--split-subgroup subgroup)))
                  ;; Other groups.
                  (emoji--add-character
                   glyph group (emoji--split-subgroup subgroup))))
              (setf (gethash base derivations)
                    (nconc (gethash base derivations) (list glyph)))))))
        (forward-line 1))
      ;; Finally create the mapping from the base glyphs to derived ones.
      (setq emoji--derived (make-hash-table :test #'equal))
      (maphash (lambda (_k v)
                 (setf (gethash (car v) emoji--derived)
                       (cdr v)))
               derivations))))

(defun emoji--generate-file (&optional file)
  "Generate an .el file with emoji mapping data and write it to FILE."
  ;; Running from Makefile.
  (unless file
    (setq file (pop command-line-args-left)))
  (emoji--init t t)
  (with-temp-buffer
    (insert ";; Generated file -- do not edit.   -*- lexical-binding:t -*-
;; Copyright © 1991-2021 Unicode, Inc.
;; Generated from Unicode data files by unidata-gen.el.
;; The sources for this file are found in the admin/unidata/ directory in
;; the Emacs sources.  The Unicode data files are used under the
;; Unicode Terms of Use, as contained in the file copyright.html in that
;; same directory.\n\n")
    (dolist (var '(emoji--labels emoji--derived emoji--names))
      (insert (format "(defconst %s '" var))
      (pp (symbol-value var) (current-buffer))
      (insert (format "\n) ;; End %s\n\n" var)))
    (insert ";; Local" " Variables:
;; coding: utf-8
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:

(provide 'emoji-labels)

;;; uni-comment.el ends here\n")
    (write-region (point-min) (point-max) file)))

(defun emoji--base-name (name derivations)
  (let* ((base (replace-regexp-in-string ":.*" "" name))
         (non-binary (replace-regexp-in-string "\\`\\(man\\|woman\\) " ""
                                               base)))
    ;; If we have (for instance) "person golfing", and we're adding
    ;; "man golfing", make the latter a derivation of the former.
    (if (or (gethash (concat "person " non-binary) derivations)
            (gethash non-binary derivations))
        non-binary
      base)))

(defun emoji--split-subgroup (subgroup)
  (let ((prefixes '("face" "hand" "person" "animal" "plant"
                    "food" "place")))
    (cond
     ((string-match (concat "\\`" (regexp-opt prefixes) "-") subgroup)
      ;; Split these subgroups into hierarchies.
      (list (substring subgroup 0 (1- (match-end 0)))
            (substring subgroup (match-end 0))))
     ((equal subgroup "person")
      (list "person" "age"))
     (t
      (list subgroup)))))

(defun emoji--add-character (char main subs)
  (let (parent elem)
    ;; Useless category.
    (unless (member main '("Component"))
      (unless (setq parent (assoc main emoji--labels))
        (setq emoji--labels (append emoji--labels
                                    (list (setq parent (list main))))))
      (setq elem parent)
      (while subs
        (unless (setq elem (assoc (car subs) parent))
          (nconc parent (list (setq elem (list (car subs))))))
        (pop subs)
        (setq parent elem))
      (nconc elem (list char)))))

(defun emoji--define-transient (&optional alist inhibit-derived
                                          end-function)
  (unless alist
    (setq alist (cons "Emoji" emoji--labels)))
  (let* ((mname (pop alist))
         (name (intern (format "emoji--command-%s" mname)))
         (emoji--done-derived (or emoji--done-derived
                                  (make-hash-table :test #'equal)))
         (has-subs (consp (cadr alist)))
         (layout
          (if has-subs
              ;; Define sub-maps.
              (cl-loop for entry in
                       (emoji--compute-prefix
                        (if (equal mname "Emoji")
                            (cons (list "Recent") alist)
                          alist))
                       collect (list
                                (car entry)
                                (emoji--compute-name (cdr entry))
                                (if (equal (cadr entry) "Recent")
                                    (emoji--recent-transient end-function)
                                  (emoji--define-transient
                                   (cons (concat mname " " (cadr entry))
                                         (cddr entry))))))
            ;; Insert an emoji.
            (cl-loop for char in alist
                     for i in (append (number-sequence ?a ?z)
                                      (number-sequence ?A ?Z)
                                      (number-sequence ?0 ?9)
                                      (number-sequence ?! ?/))
                     collect (let ((this-char char))
                               (list
                                (string i)
                                (emoji--fontify-char
                                 char inhibit-derived)
                                (let ((derived
                                       (and (not inhibit-derived)
                                            (not (gethash char
                                                          emoji--done-derived))
                                            (gethash char emoji--derived))))
                                  (if derived
                                      ;; We have a derived glyph, so add
                                      ;; another level.
                                      (progn
                                        (setf (gethash char
                                                       emoji--done-derived)
                                              t)
                                        (emoji--define-transient
                                         (cons (concat mname " " char)
                                               (cons char derived))
                                         t end-function))
                                    ;; Insert the emoji.
                                    (lambda ()
                                      (interactive)
                                      ;; Allow switching to the correct
                                      ;; buffer.
                                      (when end-function
                                        (funcall end-function))
                                      (emoji--add-recent this-char)
                                      (insert this-char)))))))))
         (args (apply #'vector mname
                      (emoji--columnize layout
                                        (if has-subs 2 8)))))
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
                   (cons :variable-pitch (cons t slots))))
       (put name 'transient--layout
            (cl-mapcan (lambda (s) (transient--parse-child name s))
                       suffixes)))
    name))

(defun emoji--recent-transient (end-function)
  "Create a function to display a dynamically generated menu."
  (lambda ()
    (interactive)
    (funcall (emoji--define-transient
              (cons "Recent" emoji--recent) t end-function))))

(defun emoji--add-recent (char)
  "Add CHAR to the set of recently used emojis."
  (setq emoji--recent (delete char emoji--recent))
  (push char emoji--recent)
  ;; Shorten the list.
  (when-let ((tail (nthcdr 30 emoji--recent)))
    (setcdr tail nil)))

(defun emoji--columnize (list columns)
  "Split LIST into COLUMN columns."
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
           collect (cons (cl-loop for char across (concat
                                                   (downcase name)
                                                   (upcase name))
                                  while (gethash char taken)
                                  finally (progn
                                            (setf (gethash char taken) t)
                                            (cl-return (string char))))
                         entry)))

(defun emoji--compute-name (entry)
  "Add example emojis to the name."
  (let ((name (concat (car entry) " "))
        (children (emoji--flatten entry))
        (max 30))
    (cl-loop for i from 0 upto 20
             ;; Choose from all the children.
             while (< (string-width name) max)
             do (cl-loop for child in children
                         for char = (elt child i)
                         while (< (string-width name) max)
                         when char
                         do (setq name (concat name char))))
    (if (= (length name) max)
        ;; Make an ellipsis signal that we've not exhausted the
        ;; possibilities.
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
  ;; Find all names.
  (let ((names (make-hash-table :test #'equal)))
    (dolist (section (emoji--flatten (cons "Emoji" emoji--labels)))
      (dolist (char section)
        (when-let ((name (emoji--name char)))
          (setf (gethash (downcase name) names) char))))
    ;; Use the list of names.
    (let* ((name (completing-read "Emoji: " names nil t))
           (glyph (gethash name names))
           (derived (gethash glyph emoji--derived)))
      (if (not derived)
          ;; Simple glyph with no derivations.
          (progn
            (emoji--add-recent glyph)
            (insert glyph))
        ;; Choose a derived version.
        (let ((emoji--done-derived (make-hash-table :test #'equal)))
          (setf (gethash glyph emoji--done-derived) t)
          (funcall
           (emoji--define-transient
            (cons "Choose Emoji" (cons glyph derived)))))))))

(provide 'emoji)

;;; emoji.el ends here
