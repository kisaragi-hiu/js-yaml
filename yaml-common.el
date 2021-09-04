;;; yaml-common.el --- Common utils -*- lexical-binding: t -*-

;;; Commentary:

;; Utils.

;;; Code:

(let ((not-found (make-symbol "not-found")))
  (defun yaml-common--hash-has-key? (hash-table key)
    "Does HASH-TABLE contain KEY?"
    (not (eq (gethash key hash-table not-found) not-found))))

(provide 'yaml-common)

;;; yaml-common.el ends here
