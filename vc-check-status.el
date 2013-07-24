;;; vc-check-status.el --- Warn you when quitting emacs and leaving repo dirty.

;; Copyright (C) 2012-2013 Sylvain Rousseau <thisirs at gmail dot com>

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

(defvar vc-check-alist
  '((".*" unpushed changes))
  "Alist of file-name patterns vs corresponding states to check.
  The list of the checks currently implemented is: dirty,
  dirty-ignore-submodule, changes, untracked, unpushed.")

(defvar vc-check nil
  "List of states to check.")
(make-variable-buffer-local 'vc-check)
(put 'vc-git-check 'safe-local-variable 'vc-check-safe-p)

(defun vc-git-check-safe-p (keywords)
  (and (listp keywords)
       (let ((list (mapcar #'car vc-git-sym-name))
             (safe t))
         (while (and safe keywords)
           (setq safe (memq (car keywords) list)
                 keywords (cdr keywords)))
         (null (null safe)))))

(defun vc-check--responsible-backend (file)
  (catch 'found
    (dolist (backend vc-handled-backends)
      (let ((path (vc-call-backend backend 'responsible-p file)))
        (if path (throw 'found (list path backend)))))))

(defun vc-get-repos ()
  "Return a list of elements of the form (PATH [KEYWORDS]...)
where PATH is the path to a repositories and KEYWORDS the list of
the checks to perform on it when quitting."
  (let ((-compare-fn (lambda (a b) (equal (car a) (car b))))
        repos)

    (--keep
     (vc-check--responsible-backend it)
     (-distinct
      (--keep
       (when (buffer-file-name it)
         (file-name-directory (buffer-file-name it)))
       (buffer-list))))


    (dolist (buffer (buffer-list) repos)
      (if (buffer-is)))

   (when
       (let* ((file (buffer-file-name it))
              (repo (and file (vc-git-root (file-truename file)))))
         (if (and repo (not (assoc repo result)))
             (push (cons repo
                         (with-current-buffer buffer
                           (if (local-variable-p 'vc-git-check)
                               vc-git-check
                             (assoc-default default-directory
                                            vc-git-check-alist
                                            'string-match))))
                   result)))
     (buffer-list))))

(defun vc-check-repos ()
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
                    (format "An error occurred on repo %s: %s; Exit anyway?"
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

(provide 'vc-check-status)

;;; vc-check-status.el ends here
