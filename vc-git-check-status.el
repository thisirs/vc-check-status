;;; vc-git-check-status.el --- Warn you when quitting emacs and
;;; leaving repo dirty.

;; Copyright (C) 2012 Sylvain Rousseau <thisirs at gmail dot com>

;; Author: Sylvain Rousseau <thisirs at gmail dot com>
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

;; This package warns you when a local repository is in a state that
;; needs to be changed before quitting emacs. For example, it is able
;; to warn you when there are some unpushed commits or if the
;; repository is dirty. The functions of the form `vc-git-check-*-p'
;; perform the check. The checks are controlled in two ways: The
;; buffer-local variable `vc-git-check' specifies the checks to
;; perform. If it is not set, the associative list
;; `vc-git-check-alist' is looked into.

(require 'vc-git)

(defconst vc-git-sym-name
  '((unpushed . "unpushed commits")
    (dirty . "changes or untracked files")
    (dirty-ignore-submodule . "changes or untracked files")
    (changes . "changes")
    (untracked . "untracked files"))
  "Alist of symbols and corresponding description.")

(defvar vc-git-check-alist
  '((".*" unpushed changes))
  "Alist of file-name patterns vs corresponding states to check.
  The list of the checks currently implemented is: dirty,
  dirty-ignore-submodule, changes, untracked, unpushed.")

(defvar vc-git-check nil
  "List of states to check.")
(make-variable-buffer-local 'vc-git-check)

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

(defun vc-git-check-unpushed-p ()
  "Return t if local repository has some commit on some branch
not pushed yet."
  (with-temp-buffer
    (vc-git-command t nil nil "log" "--branches" "--not"
                    "--remotes" "--simplify-by-decoration"
                    "--decorate" "--oneline")
    (> (buffer-size) 0)))

(defun vc-git-check-repos ()
  "Check all known repos and ask for confirmation.
This function first lists all known repositories. Then for every
one of them, it checks if they are clean. If not, it asks you if
you really want to quit."
  (let* (result
         (repos
          (dolist (buffer (buffer-list) result)
            (let* ((file (buffer-file-name buffer))
                   (repo (and file (vc-git-root
                                    (file-truename file)))))
              (if (and repo (not (assoc repo result)))
                  (push (cons repo
                              (with-current-buffer buffer
                                (if (local-variable-p 'vc-git-check)
                                    vc-git-check
                                  (assoc-default default-directory
                                                 vc-git-check-alist
                                                 'string-match))))
                        result))))))
    (while
        (and repos
             (let* ((default-directory (caar repos))
                    (checks (cdar repos))
                    error
                    (checks-ok
                     (delete
                      nil
                      (mapcar
                       (lambda (check)
                         (if (condition-case e
                                 (funcall
                                  (intern
                                   (format "vc-git-check-%s-p" check)))
                               (error (setq error e)))
                             check))
                       checks))))

               (if error
                   (yes-or-no-p
                    (format "An error occured on repo %s: %s; Exit anyway?"
                            (caar repos) error))
                 (or
                  (not checks-ok)
                  ;; if repo is an autocommited one, we don't need
                  ;; to warn user
                  (and
                   (fboundp 'vc-git-auto-committed-repo-p)
                   (vc-git-auto-committed-repo-p))
                  (yes-or-no-p
                   (format "You have %s in repository %s; Exit anyway?"
                           (mapconcatend
                            (lambda (e) (assoc-default e vc-git-sym-name))
                            checks-ok ", " " and ")
                           default-directory))))))
      (setq repos (cdr repos)))
    (null repos)))

(add-to-list 'kill-emacs-query-functions 'vc-git-check-repos)

;; Helper functions

(defun mapconcatend (func list separator last-separator)
  "Like mapconcat but the last separator can be specified. Useful
when building sentence like blah, blih, bloh and bluh."
  (let ((beg (butlast list))
        (end (car (last list))))
    (if beg
        (concat (mapconcat func beg separator) last-separator
                (funcall func end))
      (funcall func end))))


(provide 'vc-git-check-status)

;;; vc-git-check-status.el ends here
