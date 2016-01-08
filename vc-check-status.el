;;; vc-check-status.el --- Warn you when quitting emacs and leaving repo dirty.

;; Copyright (C) 2012-2016 Sylvain Rousseau <thisirs at gmail dot com>

;; Author: Sylvain Rousseau <thisirs at gmail dot com>
;; Maintainer: Sylvain Rousseau <thisirs at gmail dot com>
;; URL: https://github.com/thisirs/vc-check-status
;; Version: 0.1
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

;; This is an Emacs extension that warns you when you are about to quit
;; Emacs and leaving a git repository that has some file opened in Emacs
;; in a dirty state: uncommitted changes, unpushed commits, etc...

;;; Installation:

;; Put the following in your .emacs:

;; (require 'vc-check-status)
;; (vc-check-status-activate 1)

;; See documentation on https://github.com/thisirs/vc-check-status#vc-check-status

;;; Code:

(require 'vc)
(require 'vc-git-check-status)

(defvar vc-check-alist
  '((".*" unpushed changes))
  "Alist of file-name patterns vs corresponding states to check.

A state is either a symbol described in `vc-<VCS>-sym-name` or a
list of a symbol and its parameters.")

(defvar vc-check-backend
  '(Git)
  "Backends that are checked. A subset of `vc-handled-backends'.
Currently, only git backends are supported.")

;;;###autoload
(progn
  (defvar vc-check nil
    "Buffer-local variable that defines the list of states to
check.")
  (make-variable-buffer-local 'vc-check)
  (put 'vc-check 'safe-local-variable 'vc-check-safe-p))

(defvar vc-check-cancel-hook nil
  "Normal hook run by `vc-check-repositories', if any of the
function returns non-nil, the checking is canceled.")

(defun vc-check-safe-p (keywords)
  "Return non-nil if KEYWORDS is a list of states as described in
`vc-check-alist`."
  (and (listp keywords)
       (null (delete t (mapcar
                        (lambda (e)
                          (or (symbolp e)
                              (and (listp e)
                                   (symbolp (car e)))))
                        keywords)))))

(defun vc-check--responsible-backend (buffer)
  "Return (ROOT BACKEND) if the file visited by BUFFER is under a
version controlled system. Otherwise, return nil."
  (condition-case nil
      (with-current-buffer buffer
        (let* ((backend (vc-responsible-backend buffer-file-name))
               (root (vc-call-backend backend 'root default-directory)))
          (and backend root (list root backend))))
    (error)))

(defun vc-check--get-repositories ()
  "Return a list of elements of the form (PATH BACKEND
KEYWORDS...) where PATH is the path to a repository, BACKEND
its backend and KEYWORDS the list of the checks to perform on it
when quitting."
  (let (result)
    (dolist (buffer (buffer-list) result)
      (let ((root+backend (vc-check--responsible-backend buffer)))
        (if (and root+backend
                 (memq (cadr root+backend) vc-check-backend)
                 (not (assoc (car root+backend) result))
                 (not (with-current-buffer buffer
                        (run-hook-with-args-until-success
                         'vc-check-cancel-hook))))
            (let (temp)
              (cond
               ((and (local-variable-p 'vc-check buffer)
                     (buffer-local-value 'vc-check buffer))
                (push (append root+backend (buffer-local-value 'vc-check buffer)) result))
               ((setq temp (assoc-default (car root+backend) vc-check-alist 'string-match))
                (push (append root+backend temp) result)))))))
    result))

(defun vc-check-repositories ()
  "Check all known repos and ask for confirmation.
This function first lists all known repositories. Then for every
one of them, it checks if they are clean. If not, it asks you if
you really want to quit."
  (interactive)
  (let* ((repos (vc-check--get-repositories)))
    (while (and repos (vc-check--repository-ok (car repos)))
      (setq repos (cdr repos)))
    (null repos)))

(defun vc-check--repository-ok (repo)
  "Return non-nil if the repository described by REPO passed the
specified checks."
  (let* ((default-directory (car repo))
         (checks (cddr repo))
         (backend (downcase (symbol-name (cadr repo))))
         (sym (intern (format "vc-%s-sym-name" backend)))
         sym-alist
         checks-ok
         error)

    (if (not (boundp sym))
        (warn "Backend %s not implemented" backend)
      (setq sym-alist (symbol-value sym))
      (setq checks-ok
            (mapcar
             (lambda (check)
               (condition-case e
                   (let ((sym (if (listp check) (car check) check))
                         (params (if (listp check) (cdr check))))
                     (unless (assoc sym sym-alist)
                       (error "Check `%s' not listed in `vc-%s-sym-name'" sym backend))
                     (let ((msg (apply (intern (format "vc-%s-check-%s-p" backend sym)) params)))
                       (if (stringp msg)
                           msg
                         (and msg sym))))
                 (error (setq error e))))
             checks))

      ;; Remove nil corresponding to passed checks
      (setq checks-ok (delete nil checks-ok))

      (if error
          (yes-or-no-p
           (format "An error occurred on repo %s: %s; Exit anyway?"
                   (car repo) error))
        (or
         (not checks-ok)
         (yes-or-no-p
          (format "You have %s in repository %s; Exit anyway?"
                  (mapconcatend
                   (lambda (e)
                     (if (stringp e) e (assoc-default e sym-alist)))
                   checks-ok ", " " and ")
                  default-directory)))))))


;;;###autoload
(defun vc-check-status-activate (&optional arg)
  (interactive "P")
  (if (< (prefix-numeric-value arg) 0)
      (remove-hook 'kill-emacs-query-functions 'vc-check-repositories)
    (add-hook 'kill-emacs-query-functions 'vc-check-repositories)))


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

(provide 'vc-check-status)

;;; vc-check-status.el ends here
