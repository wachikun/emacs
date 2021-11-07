;;; user-directory-tests.el --- tests for user-directory.el -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Stefan Kangas <stefan@marxist.se>

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

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'user-directory)

(defmacro with-user-directory-test (&rest body)
  (declare (indent defun) (debug (symbolp body)))
  `(ert-with-temp-directory dir
     (let ((user-directory-alist `((cache . ,dir)
                     (config . ,dir)
                     (data . ,dir)
                     (runtime . ,dir)
                     (state . ,dir)
                     (desktop . ,dir)
                     (downloads . ,dir)
                     (documents . ,dir)
                     (music . ,dir)
                     (public . ,dir)
                     (pictures . ,dir)
                     (templates . ,dir)
                     (videos . ,dir))))
       ,@body)))


;;;; user-directory

(ert-deftest user-directory/returns-strings ()
  (with-user-directory-test
    (should (equal (user-directory 'cache) dir))
    (should (equal (user-directory 'config) dir))
    (should (equal (user-directory 'data) dir))
    (should (equal (user-directory 'runtime) dir))
    (should (equal (user-directory 'state) dir))
    (should (equal (user-directory 'desktop) dir))
    (should (equal (user-directory 'downloads) dir))
    (should (equal (user-directory 'documents) dir))
    (should (equal (user-directory 'music) dir))
    (should (equal (user-directory 'public) dir))
    (should (equal (user-directory 'pictures) dir))
    (should (equal (user-directory 'templates) dir))
    (should (equal (user-directory 'videos) dir))))

(ert-deftest user-directory/creates-dir-if-missing ()
  (with-user-directory-test
    (delete-directory dir)
    (user-directory 'downloads)
    (should (file-exists-p dir))))

(ert-deftest user-directory/alist-entry-overrides ()
  (with-user-directory-test
    (ert-with-temp-directory newdir
      (let* ((user-directory-alist `((desktop . ,newdir))))
        (unwind-protect
            (should (equal (user-directory 'desktop) newdir))
          (delete-directory newdir))))))

(ert-deftest user-directory/alist-entry-overrides/inaccessible ()
  (with-user-directory-test
    (ert-with-temp-directory dir
      (let* ((user-directory-alist `((desktop . ,dir))))
        (unwind-protect
            (progn
              (chmod dir #x000)
              (should-not (equal (user-directory 'desktop) dir)))
          (delete-directory dir))))))


;;;; user-file

(ert-deftest user-directory-tests-user-file/name-exists ()
  (with-user-directory-test
    (should (string-match "\\`foo\\'"
                          (file-name-base
                           (user-file 'downloads "foo"))))))

(ert-deftest user-directory-tests-user-file/name-missing ()
  (with-user-directory-test
    (should (string-match "\\`foo\\'"
                          (file-name-base
                           (user-file 'downloads "foo"))))))

(ert-deftest user-directory-tests-user-file/old-missing ()
  (with-user-directory-test
    (let ((old-name (make-temp-name "/tmp/bar")))
      (should (string-match "\\`foo\\'"
                            (file-name-base
                             (user-file 'downloads "foo" old-name)))))))

(ert-deftest user-directory-tests-user-file/old-exists ()
  (with-user-directory-test
    (ert-with-temp-file old-name
      (should (file-equal-p (user-file 'downloads "foo" old-name)
                            old-name)))))

(ert-deftest user-directory-tests-user-file/old-and-new-exists ()
  (with-user-directory-test
    (ert-with-temp-file old-name
      (with-temp-file (expand-file-name "new-name"
                                        (user-directory 'downloads))
        (insert "foo"))
      (should (string-match "new-name"
                            (user-file 'downloads "new-name" old-name))))))

(ert-deftest user-directory-tests-user-file/creates-dir-if-missing ()
  ;; Already tested for `user-directory' but let's make sure.
  (with-user-directory-test
    (ert-with-temp-directory dir
      (let ((user-directory-alist `((downloads . ,dir))))
        (delete-directory dir)
        (user-file 'downloads "foo/bar")
        (should (file-exists-p dir))
        ;; Sanity checks.
        (should-not (file-exists-p (expand-file-name "foo" dir)))
        (should-not (file-exists-p (expand-file-name "foo/bar")))))))


;;;; Internal

(ert-deftest user-directory--find-or-create-dir ()
  (ert-with-temp-directory dir1
    (should (equal dir1 (car (user-directory--find-or-create-dir
                              (list dir1)))))))

(ert-deftest user-directory--find-or-create-dir/creates-directory ()
  (ert-with-temp-directory dir
    (let ((new-dir (expand-file-name "foo" dir)))
      (user-directory--find-or-create-dir (list new-dir))
      (should (file-directory-p new-dir)))))

(ert-deftest user-directory-tests--find-or-create-dir/skips-inacessible ()
  (ert-with-temp-directory dir1
    (ert-with-temp-directory dir2
      (chmod dir1 #x000)
      (should (equal dir2 (car (user-directory--find-or-create-dir
                                (list dir1 dir2)))))
      (should (equal dir2 (car (user-directory--find-or-create-dir
                                (list dir2 dir1))))))))

;;; user-directory-tests.el ends here
