;;; yaml-loader.el --- YAML Loader -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)

(require 'yaml-common)
;; (require 'yaml-exception)
;; (require 'yaml-snippet)
;; (require 'yaml-schema-default)

;; Realistically, N is only ever going to be nil/0 or 1.
(defmacro yaml-loader--set-char (state out-var &optional n)
  "Advance STATE.position by N and write the character to OUT-VAR.

The character is STATE.input[STATE.position].

Neither STATE nor OUT-VAR should be quoted.

Returns the character at runtime."
  `(setq ,out-var (elt (gethash :input ,state)
                       ;; Don't output code to increment by 0
                       ,@(if n
                             `((cl-incf (get-hash :position ,state) ,n))
                           `((get-hash :position ,state))))))

(defvar yaml-loader--context-flow
  '(context-flow-in context-flow-out context-block-in context-block-out))
(defvar yaml-loader--chomping
  '(chomping-clip chomping-strip chomping-keep))

(defun yaml-loader--state (input options)
  "Initialize a new state with INPUT and OPTIONS.

OPTIONS should be a plist."
  (let ((ht (make-hash-table)))
    (puthash :input input ht)
    (puthash :length (length input) ht)

    (dolist (prop (list :filename :on-warning :json :listener
                        ;; Makes the loader expect YAML 1.1 if no %YAML
                        ;; directive is given
                        :legacy))
      (puthash prop (plist-get prop options) ht))

    (dolist (prop (list :position :line :line-start :line-indent))
      (puthash prop 0 ht))

    ;; position of first leading tab in the current line,
    ;; used to make sure there are no tabs in the indentation
    (puthash :first-tab-in-line -1 ht)

    ;; Other props "initialized" to nil:
    ;; - :documents
    ;; - :version
    ;; - :check-line-breaks
    ;; - :tag
    ;; - :anchor
    ;; - :kind
    ;; - :result
    ;; "initialized" to nil, but maybe we should initialize to a new
    ;; hash table here instead of in read-cocument?
    ;; - :tag-map
    ;; - :anchor-map

    (let ((schema (or (plist-get :schema options) yaml-schema-default)))
      (puthash :schema schema ht)
      (--> (gethash :compiled-implicit schema)
        (puthash :implicit-types it ht))
      (--> (gethash :compiled-type-map schema)
        (puthash :type-map it ht)))))

;;;; Regexp
(defun yaml-loader--contains-non-printable? (str)
  "Does STR contain non-printable characters?"
  (or
   (s-matches? "[\x00-\x08\x0B\x0C\x0E-\x1F\x7f-\x84\x86-\x9F\uFFFE\uFFFF]" str)
   (and (s-matches? "[\uD800-\uDBFF]" str)
        (s-matches? "[\uD800-\uDBFF]\\(?:[^\uDC00-\uDFFF]\\|$\\)" str))
   (s-matches? "\\(?:^\\|[^\uD800-\uDBFF]\\)[\uDC00-\uDFFF]" str)))

(defun yaml-loader--contains-non-ascii-line-breaks? (str)
  "Does STR contain non-ASCII line breaks?"
  (s-matches? "[\x85\u2028\u2029]" str))

;; lib/loader.js::28
(defun yaml-loader--tag-handle? (str)
  "Is STR a tag handle?"
  (let ((case-fold-search t))
    (s-matches?
     (rx bol
         (or "!"
             "!!"
             (seq "!" (any "a-z" "-") "!"))
         eol)
     str)))

;;;; Directive handlers
(defun yaml-loader--handle-yaml-directive (state _name args)
  "Handle the YAML directive.

ARGS can only contain one element, the YAML version (as a string).
STATE is mutated to conform to the version."
  (let (major minor)
    (when (gethash :version state)
      (yaml--throw-error state "duplication of %YAML directive"))

    (when (/= 1 (length args))
      (yaml--throw-error state "YAML directive accepts exactly one argument"))

    (save-match-data
      (unless (string-match (rx bol (group (one-or-more digit))
                                "." (group (one-or-more digit)) eol)
                            (car args))
        (yaml--throw-error state "ill-formed argument of the YAML directive"))
      (setq major (string-to-number (match-string 1 (car args)))
            minor (string-to-number (match-string 2 (car args)))))

    (when (/= 1 major)
      (yaml--throw-error state "unacceptable YAML version of the document"))

    (puthash :version (car args) state)
    (puthash :check-line-breaks (< minor 2) state)

    (unless (memq minor '(1 2))
      (yaml--throw-warning state "unsupported YAML version of the document"))))

(defun yaml-loader--handle-tag-directive (state _name args)
  "Handle tag directives.

ARGS can only contain two elements: the handle and the prefix.
STATE is mutated to set up `:tag-map'."
  (let (handle prefix)
    (when (/= (length args) 2)
      (yaml--throw-error state "TAG directive accepts exactly two arguments"))
    (setq handle (elt args 0)
          prefix (elt args 1))
    (unless (yaml-loader--tag-handle? handle)
      (yaml--throw-error
       state "ill-formed tag handle (first argument) of the TAG directive"))
    (when (yaml-common--hash-has-key? (gethash :tag-map state) handle)
      (yaml--throw-error
       state (format "there is a previously declared suffix for \"%s\" tag handle"
                     handle)))
    (unless (yaml-loader--tag-uri? prefix)
      (yaml--throw-error
       state "ill-formed tag prefix (second argument) of the TAG directive"))
    ;; lib/loader.js::255
    ;;
    ;; DEVIATION: unlike decodeURIComponent, `url-unhex-string' will
    ;; not complain about malformed uris.
    ;;
    ;; (url-unhex-string "%3Fx%3test") -> "?x%3test"
    ;; decodeURIComponent("%3Fx%3test") -> Error: malformed URI sequence
    ;;
    ;; As such, I'm not checking for malformed tag prefixes here.
    (puthash handle prefix (gethash :tag-map state))))

(defvar yaml-loader--directive-handlers
  `(
    :YAML yaml-loader--handle-yaml-directive
    :TAG yaml-loader--handle-tag-directive))

;;;; etc.

;; lib/loader.js::266
(defun yaml-loader--capture-segment (state start end check-json)
  "."
  (when (< start end)
    (let (result)
      (setq result (-> (gethash :input state)
                     (substring start end)))
      (cond
       (check-json
        (cl-loop for position from 0 to (length result)
                 do
                 (let ((character (elt result position)))
                   (unless (or (= character #x09)
                               (<= #x20 character #x10FFFF))
                     (yaml--throw-error state "expected valid JSON character"))
                   (cl-incf position))))
       ((yaml-loader--contains-non-printable? result)
        (yaml--throw-error state "the stream contains non-printable characters")))
      (--> (concat (gethash :result state) result)
        (puthash :result it state)))))

(defun yaml-loader--compose-node (state parent-indent node-context allow-to-seek allow-compact)
  (let (allow-block-styles
        allow-block-scalars
        allow-block-collections
        (indent-status 1) ; 1: this>parent, 0: this=parent, -1: this<parent
        (at-new-line nil)
        (has-content nil)
        type-index
        type-quantity
        type-list
        type
        flow-indent
        block-indent)

    (when (gethash :listener state)
      (funcall (gethash :listener state) "open" state))

    ;; lib/loader.js::1383
    (puthash :tag nil state)
    (puthash :anchor nil state)
    (puthash :kind nil state)
    (puthash :result nil state)

    ;; lib/loader.js::1388
    (when (memq node-context '(context-block-in context-block-out))
      (setq allow-block-styles t
            allow-block-scalars t
            allow-block-collections t))

    ;; lib/loader.js::1392
    (when allow-to-seek
      (when (yaml-loader--skip-separation-space state t -1)
        (setq at-new-line t)
        (cond ((> (gethash :line-indent state) parent-indent)
               (setq indent-status 1))
              ((= (gethash :line-indent state) parent-indent)
               (setq indent-status 0))
              ((< (gethash :line-indent state) parent-indent)
               (setq indent-status -1)))))

    ;; lib/loader.js::1406
    (when (= 1 indent-status)
      (while (or (yaml-loader--read-tag-property state)
                 (yaml-loader--read-anchor-property state))
        (if (yaml-loader--skip-separation-space state t -1)
            (progn
              (setq at-new-line t
                    allow-block-collections allow-block-styles)
              (cond ((> (gethash :line-indent state) parent-indent)
                     (setq indent-status 1))
                    ((= (gethash :line-indent state) parent-indent)
                     (setq indent-status 0))
                    ((< (gethash :line-indent state) parent-indent)
                     (setq indent-status -1))))
          (setq allow-block-collections nil))))

    (when allow-block-collections
      (setq allow-block-collections (or at-new-line allow-compact)))

    ;; lib/loader.js::1429
    (when (or (= indent-status 1)
              (eq node-context 'context-block-out))
      (if (memq node-context '(context-flow-int context-flow-out))
          (setq flow-indent parent-indent)
        (setq flow-indent (1+ parent-indent)))

      (setq block-indent (- (gethash :position state)
                            (gethash :line-start state)))

      ;; lib/loader.js::1438
      (cond ((= indent-status 1)
             (if (or (and allow-block-collections
                          (or (yaml-loader--read-block-sequence state block-indent)
                              (yaml-loader--read-block-mapping state block-indent flow-indent)))
                     (yaml-loader--read-flow-collection state flow-indent))
                 (setq has-content t)
               (cond ((or (and allow-block-scalars
                               (yaml-loader--read-block-scalar state flow-indent))
                          (yaml-loader--read-single-quoted-scalar state flow-indent)
                          (yaml-loader--read-double-quoted-scalar state flow-indent))
                      (setq has-content t))
                     ((yaml-loader--read-alias state)
                      (setq has-content t)
                      (when (or (gethash :tag state)
                                (gethash :anchor state))
                        (yaml--throw-error
                         state "alias node should not have any properties")))
                     ((yaml-loader--read-plain-scalar
                       state flow-indent (eq node-context 'context-flow-in))
                      (setq has-content t)
                      (unless (gethash :tag state)
                        (puthash :tag state "?"))))
               (when (gethash :anchor state)
                 (puthash (gethash :anchor state)
                          (gethash :result state)
                          (gethash :anchor-map state)))))
            ;; lib/loader.js::1469
            ((= indent-status 0)
             ;; Special case: block sequences are allowed to have same indentation level as the parent.
             ;; http://www.yaml.org/spec/1.2/spec.html#id2799784
             (setq has-content (and allow-block-collections
                                    (yaml-loader--read-block-sequence
                                     state block-indent))))))

    ;; lib/loader.js::1476
    (cond ((null (gethash :tag state))
           (when (gethash :anchor state)
             (puthash (gethash :anchor state)
                      (gethash :result state)
                      (gethash :anchor-map state))))
          ((string= (gethash :tag state) "?")
           ;; Implicit resolving is not allowed for non-scalar types,
           ;; and '?' non-specific tag is only automatically assigned
           ;; to plain scalars.
           ;;
           ;; We only need to check kind conformity in case user
           ;; explicitly assigns '?' tag, for example like this:
           ;; "!<?> [0]"
           ;;
           ;; --- Comment at lib/loader.js::1482
           ;;
           ;; I don't claim to know what any of that means.
           (when (and (gethash :result state)
                      (not (equal "scalar" (gethash :kind state))))
             (yaml--throw-error
              state
              (format "unacceptable node kind for !<?> tag; it should be \"scalar\", not \"%s\""
                      (gethash :kind state))))
           ;; lib/loader.js::1492
           (-when-let (tp
                       ;; resolver updates state.result if matched
                       (--first (yaml-type-resolve it (gethash :result state))
                                (gethash :implicit-types state)))
             (--> (yaml-type-construct tp (gethash :result state))
               (puthash :result it state))
             (puthash :tag state (gethash :tag tp))
             (when (gethash :anchor state)
               (puthash (gethash :anchor state)
                        (gethash :result state)
                        (gethash :anchor-map state)))))
          ((not (string= (gethash :tag state) "!"))
           ;; lib/loader.js::1505
           (if (yaml-common--hash-has-key? (gethash (or (gethash :kind state)
                                                        "fallback")
                                                    (gethash :type-map state))
                                           (gethash :tag state))
               (setq type (->> state
                            (gethash :type-map)
                            (gethash (or (gethash :kind state)
                                         "fallback"))
                            (gethash (gethash :tag state))))
             ;; lib/loader.js::1508
             ;; looking for multi type
             (setq type nil
                   type-list (->> state
                               (gethash :type-map)
                               (gethash :multi)
                               (gethash (or (gethash :kind state)
                                            "fallback"))))
             (-when-let (tp (--first (s-prefix? (gethash :tag it)
                                                (gethash :tag state))
                                     type-list))
               (setq type tp)))

           (unless type
             (yaml--throw-error
              state (format "unknown tag !<%s>" (gethash :tag state))))

           ;; lib/loader.js::1524
           (when (and (gethash :result state)
                      (not (equal (gethash :kind type)
                                  (gethash :kind state))))
             (yaml--throw-error
              state
              (format "unacceptable node kind for !<%s> tag; it should be \"%s\", not \"%s\""
                      (gethash :tag state)
                      (gethash :kind type)
                      (gethash :kind state))))

           ;; state.result updated in resolver if matched
           (if (not (yaml-type-resolve (gethash :result state) (gethash :tag state)))
               (yaml--throw-error
                state (format "cannot resolve a node with !<%s> explicit tag"
                              (gethash :tag state)))
             ;; lib/loader.js::1531
             (--> (yaml-type-construct (gethash :result state)
                                       (gethash :tag state))
               (puthash :result it state))
             (when (gethash :anchor state)
               (puthash (gethash :anchor state)
                        (gethash :result state)
                        (gethash :anchor-map state))))))

    (when (gethash :listener state)
      (funcall (gethash :listener state) "close" state))

    (not (not (or (gethash :tag state)
                  (gethash :anchor state)
                  has-content)))))

;; lib/loader.js::1544
(defun yaml-loader--read-document (state)
  (cl-block nil
    (let ((document-start (gethash :position state))
          position directive-name directive-args has-directives ch)
      (puthash :version nil state)
      (puthash :check-line-breaks nil state)
      (puthash :tag-map (make-hash-table) state)
      (puthash :anchor-map (make-hash-table) state)

      ;; lib/loader.js:1557
      (catch 'break
        (while (/= 0 (yaml-loader--set-char state ch))
          (yaml-loader--skip-separation-space state t -1)
          (yaml-loader--set-char state ch)

          (when (or (> (gethash :line-indent state) 0)
                    (/= ch ?%))
            (throw 'break nil))

          (setq has-directives t)
          (yaml-loader--set-char state ch 1)
          (setq position (gethash :position state))

          (while (and (/= ch 0)
                      (not (yaml-loader--ws-or-eol? ch)))
            (yaml-loader--set-char state ch 1))

          (setq directive-name (-> (gethash :input state)
                                 (substring position (gethash :position state)))
                directive-args nil)

          (when (< (length directive-name) 1)
            (yaml--throw-error
             state "directive name must not be less than one character in length"))

          ;; lib/loader.js::1581
          (catch 'break
            (while (/= ch 0)
              (while (yaml-loader--white-space? ch)
                (yaml-loader--set-char state ch 1))
              (when (= ch ?#)
                (cl-loop do (yaml-loader--set-char state ch 1)
                         while (and (/= ch 0)
                                    (not (yaml-loader--eol? ch))))
                (throw 'break nil))
              (when (yaml-loader--eol? ch)
                (throw 'break nil))
              (setq position (gethash :position state))
              (while (and (/= ch 0)
                          (not (yaml-loader--ws-or-eol? ch)))
                (yaml-loader--set-char state ch 1))
              (push (-> (gethash :input state)
                      (substring position (gethash :position state)))
                    directive-args)))

          (when (/= ch 0)
            (yaml-loader--read-line-break state))

          (if (plist-member
               yaml-loader--directive-handlers
               (intern (concat ":" directive-name)))
              (funcall (plist-get yaml-loader--directive-handlers
                                  (intern (concat ":" directive-name)))
                       state directive-name directive-args)
            (yaml--throw-warning
             state (format "unknown document directive \"%s\"" directive-name)))))

      (yaml-loader--skip-separation-space state t -1)

      (cond
       ;; "---" at indent level 0
       ((and (= 0 (gethash :line-indent state))
             (= ?- (elt (gethash :input state)
                        (gethash :position state)))
             (= ?- (elt (gethash :input state)
                        (+ 1 (gethash :position state))))
             (= ?- (elt (gethash :input state)
                        (+ 2 (gethash :position state)))))
        (cl-incf (gethash :position state) 3)
        (yaml-loader--skip-separation-space state t -1))
       (has-directives
        (yaml--throw-error state "directives end mark is expected")))

      (yaml-loader--compose-node
       state (- (gethash :line-indent state) 1)
       'context-block-out nil t)
      (yaml-loader--skip-separation-space state t -1)

      (when (and (gethash :check-line-breaks state)
                 (yaml-loader--contains-non-ascii-line-breaks?
                  (-> (gethash :input state)
                    (substring document-start (gethash :position state)))))
        (yaml--throw-warning state "non-ASCII line breaks are interpreted as content"))

      ;; lib/loader.js::1633
      (push (gethash :result state) (gethash :documents state))

      (when (and (= (gethash :position state) (gethash :line-start state))
                 (yaml-loader--test-document-separator state))
        (when (= ?. (elt (gethash :input state)
                         (gethash :position state)))
          (cl-incf (gethash :position state) 3)
          (yaml-loader--skip-separation-space state t -1))
        (cl-return))

      ;; state.length is set to state.input.length on initialization
      (if (< (gethash :position state) (1- (gethash :length state)))
          (yaml--throw-error state "end of the stream or a document separator is expected")
        (cl-return)))))

(provide 'yaml-loader)

;;; yaml-loader.el ends here
