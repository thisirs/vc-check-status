;;; vc-check-status-tests.el ---

;; Copyright (C) 2012-2016 Sylvain Rousseau <thisirs at gmail dot com>

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

(ert-deftest vc-check-status-is-check-status-local ()
  "Test local settings."
  (with-delete-repository
   (let ((repo (with-empty-repository "check-local"
                 (shell-command "touch blah-local")
                 (with-temp-buffer
                   (princ '((nil (vc-check . (changes))))
                          (current-buffer))
                   (write-file ".dir-locals.el"))
                 (find-file "blah-local"))))
     (with-current-buffer "blah-local"
       (should (local-variable-p 'vc-check))
       (should vc-check))
     (should (equal (assoc repo (vc-check--get-repositories))
                    `(,repo Git changes))))))

(ert-deftest vc-check-status-is-check-status-global ()
  "Test global settings."
  (with-delete-repository
   (let ((repo (with-empty-repository "check-global"
                 (shell-command "touch blah-global")
                 (find-file "blah-global"))))
     (setq vc-check-alist nil)
     (should-not (vc-check--get-repositories))
     (setq vc-check-alist '((".*" changes)))
     (should (assoc-default repo (vc-check--get-repositories)
                            'string-match)))))

;;; vc-check-status-tests.el ends here
