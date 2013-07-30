;;; vc-git-check-status-test.el ---

;; Copyright (C) 2012-2013 Sylvain Rousseau <thisirs at gmail dot com>

;; Author: Sylvain Rousseau <thisirs at gmail dot com>
;; Maintainer: Sylvain Rousseau <thisirs at gmail dot com>
;; URL: https://github.com/thisirs/vc-check-status.git
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code

(defvar vc-check-test-repository-list ()
  "List of repositories used in the tests.")

(defun vc-check-test-shell-commands (&rest commands)
  "Execute the set of COMMANDS with `shell-command'."
  (declare (indent nil))
  (mapc (lambda (command)
          (shell-command command))
        commands))

(defmacro with-empty-repository (prefix &rest body)
  "Create an empty repository in a temporary directory prefixed
by PREFIX and execute BODY in it. Return the path of the newly
created repository to allow easy cloning."
  (declare (indent 1))
  `(let ((default-directory
           (file-name-as-directory (make-temp-file ,prefix :dir))))
     (add-to-list 'vc-check-test-repository-list default-directory)
     (shell-command "git init .")
     ,@body
     default-directory))

(defmacro with-untracked (&rest body)
  "Execute BODY in a repository with untracked file."
  `(with-delete-repository
    (with-empty-repository "untracked"
      (shell-command "touch blah")
      ,@body)))

(defmacro with-changes (&rest body)
  "Execute BODY in a repository with changes."
  `(with-delete-repository
    (with-empty-repository "untracked"
      (vc-check-test-shell-commands
       "touch blah"
       "git add ."
       "git commit -m Blah"
       "echo blah > blah")
      ,@body)))

(defmacro with-clone-of (prefix origin &rest body)
  "Execute BODY in a cloned repository of ORIGIN."
  (declare (indent 2))
  `(let ((default-directory
           (file-name-as-directory (make-temp-file ,prefix :dir))))
     (shell-command (concat "git clone " ,origin " " default-directory))
     (add-to-list 'vc-check-test-repository-list default-directory)
     ,@body))

(defmacro with-cloned-unpushed (&rest body)
  "Execute BODY in a cloned repository that has unpushed
commits."
  `(with-delete-repository
    (with-clone-of "local" (with-empty-repository "origin")
      (vc-check-test-shell-commands
       "touch blah"
       "git add ."
       "git commit -m Blah")
      ,@body)))

(defmacro with-delete-repository (&rest body)
  "Execute BODY and delete all the repositories created during
the process."
  `(let (vc-check-test-repository-list)
     (unwind-protect
         (progn ,@body)
       (mapc (lambda (dir)
               (ignore-errors (delete-directory dir :recursive)))
             vc-check-test-repository-list))))

(ert-deftest vc-git-check-status-untracked ()
  "Check for the untracked state."
  (with-untracked
   (should (vc-git-check-dirty-p))
   (should (vc-git-check-dirty-ignore-submodule-p))
   (should-not (vc-git-check-changes-p))
   (should (vc-git-check-untracked-p))
   (should-not (vc-git-check-unpushed-p))
   (should-not (vc-git-check-unpushed-current-p))
   (should-not (vc-git-check-stash-p))))

(ert-deftest vc-git-check-status-changes ()
  "Check for the changes state."
  (with-changes
   (should (vc-git-check-dirty-p))
   (should (vc-git-check-dirty-ignore-submodule-p))
   (should (vc-git-check-changes-p))
   (should-not (vc-git-check-untracked-p))
   (should-not (vc-git-check-unpushed-p))
   (should-not (vc-git-check-unpushed-current-p))
   (should-not (vc-git-check-stash-p))))

(ert-deftest vc-git-check-status-unpushed ()
  "Check for the unpushed state."
  (with-cloned-unpushed
   (should-not (vc-git-check-dirty-p))
   (should-not (vc-git-check-dirty-ignore-submodule-p))
   (should-not (vc-git-check-changes-p))
   (should-not (vc-git-check-untracked-p))
   (should (vc-git-check-unpushed-p))
   (should (vc-git-check-unpushed-current-p))
   (should-not (vc-git-check-stash-p))))

;;; vc-git-check-status-test.el ends here
