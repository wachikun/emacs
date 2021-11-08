;;; user-directory.el --- Find user-specific directories  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Stefan Kangas <stefan@marxist.se>
;; Keywords: internal
;; Package: emacs

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

;; * Introduction
;;
;; This library contains functions to handle various user directories,
;; such as configuration files, user data, etc. in a platform
;; independent way.
;;
;; Users can override the returned directories with
;; `user-directory-alist'.
;;
;; * Using from Lisp
;;
;; The entry point for Lisp libraries is `user-file' and
;; `user-directory'.
;;
;; - User options for file names should be defined relative to the
;;   paths returned by this library.
;;
;; - Instead of calling this once and caching the value in a variable,
;;   best practice is to call it on use.  That way the user can update
;;   the settings here without having to reload your library.

;;; Code:

(require 'cl-lib)
(require 'xdg)

(defgroup user-directory nil
  "User directories."
  :group 'environment
  :version "29.1")

(defcustom user-directory-alist ()
  "Overrides for `user-directory'.
This allows you to override where Emacs stores your configuration
and data files."
  :type 'list
  :risky t)

(defcustom user-directory-warning t
  "If non-nil, `user-directory' will warn if unable to access or create directory.
Set this to nil at your own risk, as it might lead to data loss
when Emacs tries to save something in a non-existent or
inaccessible location."
  :type 'boolean)


;;;; user-directory

;;;###autoload
(cl-defgeneric user-directory (type)
  "Return a user-specific directory of TYPE.
TYPE is one of these symbols:

 - cache        Emacs cached files
 - config       Emacs configuration files
 - data         Emacs user data
 - runtime      Emacs runtime files
 - state        Emacs state data
 - desktop      User desktop              (e.g. \"~/Desktop\")
 - downloads    User downloaded files     (e.g. \"~/Downloads\")
 - documents    User documents            (e.g. \"~/Documents\")
 - music        User music files          (e.g. \"~/Music\")
 - public       User shared files         (e.g. \"~/Public\")
 - pictures     User picture files        (e.g. \"~/Pictures\")
 - templates    User template files       (e.g. \"~/Templates\")
 - videos       User video files          (e.g. \"~/Videos\")

For more details, see below.")

(defun user-directory--find-or-create-dir (dirs)
  "Find the first directory that exists and is accessible in DIRS.
Return value is (DIR . ERRTYPES)."
  (let* (errtypes
         (found
          (catch 'found
            (dolist (dir dirs)
              ;; Make sure the top level directory exists, unless
              ;; we're in batch mode or dumping Emacs.
              (or noninteractive
                  dump-mode
                  (if (file-directory-p dir)
                      (or (and (file-accessible-directory-p dir)
                               (throw 'found (cons dir errtypes)))
                          (push "access" errtypes))
                    (with-file-modes #o700
                      (condition-case nil
                          (progn (make-directory dir t)
                                 (setq errtypes nil)
                                 (throw 'found (cons dir errtypes)))
                        (error (push "create" errtypes))))))))))
    (or found (cons nil errtypes))))

(cl-defmethod user-directory :around (type)
  (convert-standard-filename
   (pcase-let* ((dirs (delq nil (cons (cdr (assq type user-directory-alist))
                                      (cl-call-next-method))))
                (`(,dir . ,errtypes) (user-directory--find-or-create-dir dirs)))
     (when (and (not dir) user-directory-warning
                (get 'user-directory-warning type))
       ;; Warn only once per Emacs session and type.
       (put 'user-directory-warning type t)
       (display-warning 'initialization
                        (format "\
`user-directory' is unable to %s the %s user directory: %s
Any data that would normally be written there may be lost!
If you never want to see this message again,
customize the variable `user-directory-warning'."
                                ;; Warn about the most specific directory.
                                (car errtypes) type (car dirs))))
     ;; If no usable directory was found, return the most specific one.
     (or dir (car dirs)))))


;;;; Configuration, cache, and state.

(cl-defmethod user-directory ((_type (eql 'cache)))
  "Return the user cache directory.
The cache directory contains non-essential user data that is not
necessarily important to save."
  (cons (expand-file-name "emacs" (xdg-cache-home))
        '("~/.cache/emacs")))

(cl-defmethod user-directory ((_type (eql 'config)))
  "Return the user configuration file directory.
The configuration file directory contains any user-specific
configuration."
  ;; We rely startup.el to find and set this.
  (list user-emacs-directory))

(cl-defmethod user-directory ((_type (eql 'data)))
  "Return the user data directory.
Examples of things that belong in the user data directory are
bookmarks and chat logs."
  (cons (expand-file-name "emacs" (xdg-data-home))
        '("~/.local/share/emacs")))

(cl-defmethod user-directory ((_type (eql 'runtime)))
  "Return the user runtime directory.
The runtime directory contains user-specific non-essential
runtime files and other file objects (such as sockets, named
pipes, etc.)."
  (list (cond ((expand-file-name "emacs" (xdg-runtime-dir)))
              ((error "Unable to find runtime directory")))))

(cl-defmethod user-directory ((_type (eql 'state)))
  "Return the user state directory.
The state directory contains user data that should persist
between restarts of Emacs, but is not important enough to store
in the data directory.  Things like completion history and lists
of recently opened files probably belong here."
  (cons (expand-file-name "emacs" (xdg-state-home))
        '("~/.local/state/emacs")))


;;;; User files.

(cl-defmethod user-directory ((_type (eql 'desktop)))
  "Return user desktop directory."
  (cons (xdg-user-dir "DESKTOP")
        '("~/Desktop")))

(cl-defmethod user-directory ((_type (eql 'download)))
  "Return user directory for downloads."
  (cons (xdg-user-dir "DOWNLOAD")
        '("~/Downloads")))

(cl-defmethod user-directory ((_type (eql 'downloads)))
  "Return user directory for downloads."
  (cons (xdg-user-dir "DOWNLOAD")
        '("~/Downloads")))

(cl-defmethod user-directory ((_type (eql 'documents)))
  "Return user directory for documents."
  (cons (xdg-user-dir "DOCUMENTS")
        '("~/Documents")))

(cl-defmethod user-directory ((_type (eql 'music)))
  "Return user directory for music."
  (cons (xdg-user-dir "MUSIC")
        '("~/Music")))

(cl-defmethod user-directory ((_type (eql 'public)))
  "Return user directory for public (shared) files."
  (cons (xdg-user-dir "PUBLIC")
        '("~/Public")))

(cl-defmethod user-directory ((_type (eql 'pictures)))
  "Return user directory for documents."
  (cons (xdg-user-dir "PICTURES")
        '("~/Pictures")))

(cl-defmethod user-directory ((_type (eql 'templates)))
  "Return user directory for templates."
  (cons (xdg-user-dir "TEMPLATES")
        '("~/Templates")))

(cl-defmethod user-directory ((_type (eql 'videos)))
  "Return user directory for video files."
  (cons (xdg-user-dir "VIDEOS")
        '("~/Videos")))


;;;; user-file

(defun user-directory--find-old-name (old-name)
  "Create a list of readable file names based on OLD-NAME.
OLD-NAME is a string or a list, as in `user-file'.

This is an internal helper function to `user-file'."
  (catch 'found
    (dolist (name (or (and (listp old-name) old-name)
                      (list old-name)))
      (mapcar (lambda (name)
                (when (file-readable-p name)
                  (throw 'found name)))
              (list old-name
                    (expand-file-name old-name
                                      user-emacs-directory))))))

;;;###autoload
(defun user-file (type name &optional old-name)
  "Return an absolute per-user Emacs-specific file name.
TYPE should be a symbol and is passed as an argument to
`user-directory'.

1. If NEW-NAME exists in the directory for TYPE, return it.

2. Else if OLD-NAME is non-nil and OLD-NAME exists, return OLD-NAME.
   OLD-NAME is an absolute file name or a list of absolute file
   names.  If it is a list, try each of the names in the list.

3. Else return NEW-NAME in the directory for TYPE, creating the
   directory if it does not exist.  (Only the top level directory
   for that type will be created, as with `user-directory'.)

Note: in contrast with `locate-user-emacs-file', OLD-NAME is not
a relative but an absolute file name.  This typically means that
you will need to add an explicit \"~/\" at the beginning of the
string, when converting calls from that function to this one."
  (convert-standard-filename
   (let* ((dir (user-directory type))
          (new-name (expand-file-name name dir)))
     (abbreviate-file-name
      (or (and old-name
               (not (file-readable-p new-name))
               (user-directory--find-old-name old-name))
          new-name)))))

(provide 'user-directory)

;;; user-directory.el ends here
