;;; vc-git-check-status-test.el ---

;; Copyright (C) 2012-2015 Sylvain Rousseau <thisirs at gmail dot com>

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

;;; Code:

(defvar vc-check-test-repository-list ()
  "List of repositories used in the tests.")

(defun vc-check-test-shell-commands (&rest commands)
  "Execute the set of COMMANDS with `shell-command'."
  (declare (indent nil))
  (mapc (lambda (command)
          (shell-command command))
        commands))

(defun make-changes (&optional filename)
  "Make changes to the current repository.

Create a file FILENAME if it does not exist and change it if it
exists."
  (vc-check-test-shell-commands
   (concat "echo 1 >> " (if filename
                            (shell-quote-argument filename)
                          "blah"))))

(defun make-commit ()
  "Add a commit to the current repository."
  (make-changes)
  (vc-check-test-shell-commands
   "git add -A ."
   "git commit -m blah"))

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

(defmacro with-standard-repository (prefix &rest body)
  "Executes BODY in a repository with one commit."
  (declare (indent 1))
  `(with-empty-repository ,prefix
     (make-commit)
     ,@body))

(defmacro with-submodule (main sub &rest body)
  `(let* ((submodule (with-standard-repository ,sub)))
     (with-standard-repository ,main
       (vc-check-test-shell-commands
        (format "git submodule add %s %s"
                submodule "submodule")
        "git commit -am \"Add submodule\"")
       ,@body)))

(defmacro with-untracked (&rest body)
  "Execute BODY in a repository with untracked file."
  `(with-standard-repository "untracked"
     (make-changes "foo")
     ,@body))

(defmacro with-changes (&rest body)
  "Execute BODY in a repository with changes."
  `(with-standard-repository "changes"
     (make-changes)
     ,@body))

(defmacro with-clone-of (prefix origin &rest body)
  "Execute BODY in a cloned repository of ORIGIN."
  (declare (indent 2))
  `(let ((default-directory
           (file-name-as-directory (make-temp-file ,prefix :dir))))
     (shell-command (concat "git clone " ,origin " " default-directory))
     (add-to-list 'vc-check-test-repository-list default-directory)
     ,@body))

(defmacro with-cloned (&rest body)
  "Executes BODY in a cloned repository."
  `(with-clone-of "local" (with-standard-repository "origin")
     ,@body))

(defmacro with-cloned-unpushed (&rest body)
  "Execute BODY in a cloned repository that has unpushed
commits."
  `(with-cloned
    (make-commit)
    ,@body))

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
  (with-delete-repository
   (with-untracked
    (should (vc-git-check-dirty-p))
    (should (vc-git-check-dirty-ignore-submodule-p))
    (should-not (vc-git-check-changes-p))
    (should (vc-git-check-untracked-p))
    (should-not (vc-git-check-unpushed-p))
    (should-not (vc-git-check-unpushed-current-p))
    (should-not (vc-git-check-stash-p)))))

(ert-deftest vc-git-check-status-changes ()
  "Check for the changes state."
  (with-delete-repository
   (with-changes
    (should (vc-git-check-dirty-p))
    (should (vc-git-check-dirty-ignore-submodule-p))
    (should (vc-git-check-changes-p))
    (should-not (vc-git-check-untracked-p))
    (should-not (vc-git-check-unpushed-p))
    (should-not (vc-git-check-unpushed-current-p))
    (should-not (vc-git-check-stash-p)))))

(ert-deftest vc-git-check-status-unpushed-1 ()
  "Check for unpushed commits in local repository."
  (with-delete-repository
   (with-standard-repository "unpushed"
     (should-not (vc-git-check-unpushed-p)))))

(ert-deftest vc-git-check-status-unpushed-2 ()
  "Check for unpushed commits in cloned repository."
  (with-delete-repository
   (with-cloned
    (should-not (vc-git-check-unpushed-p)))))

(ert-deftest vc-git-check-status-unpushed-3 ()
  "Check for unpushed commits in unpushed repository."
  (with-delete-repository
   (with-cloned-unpushed
    (should (vc-git-check-unpushed-p)))))

(ert-deftest vc-git-check-status-unpushed-4 ()
  "Check for unpushed commits in unpushed repository, other
branch."
  (with-delete-repository
   (with-cloned-unpushed
    (vc-check-test-shell-commands
     "git checkout -b other")
    (should (vc-git-check-unpushed-p)))))

(ert-deftest vc-git-check-status-unpushed-5 ()
  "Check for unpushed commits in unpushed repository only on
master branch when in other branch."
  (with-delete-repository
   (with-cloned-unpushed
    (vc-check-test-shell-commands
     "git checkout -b other")
    (should (equal "unpushed commits on master" (vc-git-check-unpushed-p "master"))))))

(ert-deftest vc-git-check-status-unpushed-6 ()
  "Check for unpushed commits in unpushed repository only on
master branch when in master branch."
  (with-delete-repository
   (with-cloned-unpushed
    (should (vc-git-check-unpushed-p "master")))))

(ert-deftest vc-git-check-status-unpushed-current-1 ()
  "Check for unpushed current, simplest case."
  (with-delete-repository
   (with-cloned
    (should-not (vc-git-check-unpushed-current-p)))))

(ert-deftest vc-git-check-status-unpushed-current-2 ()
  "Check for unpushed current, simplest case."
  (with-delete-repository
   (with-cloned-unpushed
    (should (vc-git-check-unpushed-current-p)))))

(ert-deftest vc-git-check-status-unpushed-current-3 ()
  "Check for unpushed current when in other branch."
  (with-delete-repository
   (with-cloned-unpushed
    (vc-check-test-shell-commands
     "git checkout -b other")
    (should-not (vc-git-check-unpushed-current-p)))))

(ert-deftest vc-git-check-status-on-branch ()
  "Check if on specified branches or not."
  (with-delete-repository
   (with-standard-repository "not-on-branch"
     (should (vc-git-check-on-branch-p "master"))
     (should (vc-git-check-on-branch-p '("dev" "master")))
     (should-not (vc-git-check-on-branch-p "dev"))
     (should-not (vc-git-check-on-branch-p '("other" "dev")))
     (should-not (vc-git-check-not-on-branch-p "master"))
     (should-not (vc-git-check-not-on-branch-p '("dev" "master"))))))

;;; vc-git-check-status-test.el ends here
