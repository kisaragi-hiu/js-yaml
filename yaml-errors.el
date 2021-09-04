;;; yaml-errors.el --- Errors -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(define-error 'yaml-error "Unknown YAML error")

(prog1 'yaml-schema
  ;; lib/schema.js::84
  (define-error 'yaml-schema-extend-argument "YAML schema extend" 'yaml-error)
  ;; lib/schema.js::90, lib/schema.js::104
  (define-error 'yaml-schema-extend-non-type "YAML schema extend" 'yaml-error)
  ;; lib/schema.js::94
  (define-error 'yaml-schema-extend-implicit-non-scalar "YAML schema extend" 'yaml-error)
  ;; lib/schema.js::98
  (define-error 'yaml-schema-extend-implicit-multi "YAML schema extend" 'yaml-error))

(prog1 'yaml-dumper
  ;; lib/dumper.js::105
  (define-error 'yaml-dumper-large-code-point "YAML dumper" 'yaml-error)
  ;; lib/dumper.js::441
  (define-error 'yaml-dumper-scalar-invalid "YAML dumper" 'yaml-error)
  ;; lib/dumper.js::693: use `wrong-type-argument'
  ;; lib/dumper.js::781
  (define-error 'yaml-dumper-tag-resolver-wrong-style "YAML dumper" 'yaml-error)
  ;; lib/dumper.js::868
  (define-error 'yaml-dumper-unaccceptable-object "YAML dumper" 'yaml-error))

(prog1 'yaml-type
  ;; lib/type.js::43
  (define-error 'yaml-type-unknown-option "Unknown option" 'yaml-error)
  ;; lib/type.js::62
  (define-error 'yaml-type-unknown-kind "Unknown kind" 'yaml-error))

(prog1 'yaml-loader
  ;; lib/loader.js::1722
  (define-error 'yaml-loader-too-many-documents "Too many documents" 'yaml-error)
  (define-error 'yaml-loader "YAML loader" 'yaml-error))

(define-error 'yaml-end-of-file "End of file while parsing YAML"
  '(end-of-file yaml-error))

(provide 'yaml-errors)

;;; yaml-errors.el ends here
