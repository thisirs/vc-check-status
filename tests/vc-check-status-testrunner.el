;;; vc-check-status-testrunner.el ---

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

(require 'ert)

(defadvice ert--pp-with-indentation-and-newline (around fix-display activate)
  "Do not truncate the results of a failing test."
  (let ((print-escape-newlines t)
        (print-level nil)
        (print-length nil))
    ad-do-it))

(let* ((current-directory (file-name-directory load-file-name))
       (test-path (expand-file-name "." current-directory))
       (root-path (expand-file-name ".." current-directory)))

  (add-to-list 'load-path root-path)

  ;; Activating vc-check-status
  (load (expand-file-name "vc-check-status.el" root-path) t t)
  (vc-check-status-activate)

  ;; Loading the tests
  (load (expand-file-name "vc-git-check-status-test.el" test-path) t t)

  (ert-run-tests-batch-and-exit t))

;;; vc-check-status-testrunner.el ends here
