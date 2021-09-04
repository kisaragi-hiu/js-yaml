;;; yaml-loader.el --- YAML Loader -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(require 'yaml-common)
(require 'yaml-exception)
(require 'yaml-snippet)
(require 'yaml-schema-default)

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
            (throw 'break))

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
                (throw 'break))
              (when (yaml-loader--eol? ch)
                (throw 'break))
              (setq position (gethash :position state))
              (while (and (/= ch 0)
                          (not (yaml-loader--ws-or-eol? ch)))
                (yaml-loader--set-char state ch 1))
              (push (-> (gethash :input state)
                      (substring position (gethash :position state)))
                    directive-args)))

          (when (/= ch 0)
            (yaml-loader--read-line-break state))

          (if (yaml-common--hash-has-key?
               yaml-loader--directive-handlers
               directive-name)
              (funcall (gethash yaml-loader--directive-handlers directive-name)
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
