;;; vc-git-check-status.el --- Git checking functions

;; Copyright (C) 2012-2016 Sylvain Rousseau <thisirs at gmail dot com>

;; Author: Sylvain Rousseau <thisirs at gmail dot com>
;; Maintainer: Sylvain Rousseau <thisirs at gmail dot com>
;; URL: https://github.com/thisirs/vc-check-status.git
;; Keywords: vc, convenience

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

(require 'vc-git)

(defconst vc-git-sym-name
  '((unpushed . "unpushed commits")
    (unpushed-current . "unpushed commits in current branch")
    (dirty . "changes or untracked files")
    (dirty-ignore-submodule . "changes or untracked files")
    (changes . "changes")
    (stash . "stashed changes")
    (untracked . "untracked files")
    (on-branch . "forbidden branches")
    (not-on-branch . "required branches"))
  "Alist of symbols and corresponding description.")

(defun vc-git-check-dirty-p ()
  "Return t if local repository is dirty."
  (with-temp-buffer
    (vc-git-command t 0 nil "status" "--porcelain")
    (> (buffer-size) 0)))

(defun vc-git-check-dirty-ignore-submodule-p ()
  "Return t if local repository is dirty. Changes in submodules
are ignored."
  (with-temp-buffer
    (vc-git-command t 0 nil "status" "--porcelain" "--ignore-submodules")
    (> (buffer-size) 0)))

(defun vc-git-check-changes-p ()
  "Return t if local repository is changed.
Untracked files are ignored."
  (with-temp-buffer
    (vc-git-command t 0 nil "status" "--porcelain")
    (goto-char 1)
    (flush-lines "^\\?\\?")
    (> (buffer-size) 0)))

(defun vc-git-check-untracked-p ()
  "Return t if local repository has untracked files."
  (with-temp-buffer
    (vc-git-command t 0 nil "status" "--porcelain")
    (goto-char 1)
    (keep-lines "^\\?\\?")
    (> (buffer-size) 0)))

(defun vc-git-check-unpushed-p (&rest branches)
  "Return non-nil if local repository has some unpushed commits.

If BRANCHES is nil, it tests for unpushed commits on all existing
 branches. If not, it limits its search on BRANCHES only."
  (and (with-temp-buffer
         (vc-git-command t 0 nil "remote" "show")
         (> (buffer-size) 0))
       (if branches
           (let (branch)
             (while (and (setq branch (pop branches))
                         (not (with-temp-buffer
                                ;; For some reason, git expects to
                                ;; find a glob character in branch...
                                ;; So insert one...
                                (let ((branch+glob (concat "[" (substring branch 0 1) "]" (substring branch 1))))
                                  (vc-git-command t 0 nil "log" (format "--branches=%s" branch+glob) "--not" "--remotes" )
                                  (> (buffer-size) 0))))))
             (if branch
                 (format "unpushed commits on %s" branch)))
         (with-temp-buffer
           (vc-git-command t 0 nil "log" "--branches" "--not" "--remotes" )
           (> (buffer-size) 0)))))

(defun vc-git-check-unpushed-current-p ()
  "Return non-nil if local repository has some commit on current
branch not pushed yet."
  (with-temp-buffer
    (and
     (eq 0 (vc-git-command t t nil "cherry"))
     (> (buffer-size) 0))))

(defun vc-git-check-stash-p ()
  "Return t if local repository has changes stashed."
  (with-temp-buffer
    (eq 0 (vc-git-command t 1 nil "stash" "show"))))

(defun vc-git-check-on-branch-p (branch)
  "Return non-nil if current branch is BRANCH or belongs to
BRANCH if it is a list."
  (if (stringp branch)
      (setq branch (list branch)))
  (let ((current-branch
         (substring
          (with-temp-buffer
            (vc-git-command t 1 nil "symbolic-ref" "-q" "HEAD")
            (buffer-string))
          11 -1)))
    (and (member current-branch branch) t)))

(defun vc-git-check-not-on-branch-p (branch)
  "Return non-nil if current branch is not BRANCH or does not
belong to BRANCH if it is a list."
  (not (vc-git-check-on-branch-p branch)))

(provide 'vc-git-check-status)

;;; vc-git-check-status.el ends here
