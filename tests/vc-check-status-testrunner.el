(require 'ert)

(defadvice ert--pp-with-indentation-and-newline (around fix-display activate)
  (let ((print-escape-newlines t)
        (print-level nil)
        (print-length nil))
    ad-do-it))

(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

(ert-run-tests-batch-and-exit t)
