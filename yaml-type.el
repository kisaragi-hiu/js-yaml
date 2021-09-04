;;; yaml-type.el --- YAML Types -*- lexical-binding: t -*-

;;; Commentary:

;; Types are hash tables.
;;
;; Type methods should receive type as the first argument.

;;; Code:

(defun yaml-type-construct (type args))
(defun yaml-type-resolve (type args))

(provide 'yaml-type)

;;; yaml-type.el ends here
