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

(defvar emoji--labels nil)

;;;###autoload
(defun emoji-insert ()
  "Choose and insert an emoji glyph."
  (interactive)
  (setq transient-use-variable-pitch t)
  ;; Remove debugging.
  (unless (and nil emoji--labels)
    (emoji--parse-labels)
    (emoji--define-transient))
  (funcall (intern "emoji-command-Emoji")))

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
        (setq subs (list "person" "human")))
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
      (nconc elem chars))))

(defun emoji--define-transient (&optional alist)
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
                               (list (string i)
                                     char
                                     (lambda ()
                                       (interactive)
                                       (insert this-char)))))))
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

(provide 'emoji)

;;; emoji.el ends here
