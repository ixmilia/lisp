;;; To bootstrap the interpreter, this file is loaded with an extremely restricted parser that can only handle:
;;;   1. Whitespace.
;;;   2. Comments starting with `;` and proceeding to the end of the line.
;;;   3. Lists delimited by `(` and `)`.
;;;   4. Anything symbol-like.
;;; No other parsing can occur in this file, e.g.:
;;;   1. No strings.
;;;   2. No reader macros.

(setf *left-paren-character*    (code-char 40)) ; (
(setf *right-paren-character*   (code-char 41)) ; )

(defun list-builder (stream left-paren)
    (swallow-trivia stream)
    (setf +eof+ (gensym))
    (setf next (peek-char () stream nil +eof+ t))
    (cond ((eq next +eof+)                      (error-at-location *unmatched-left-paren-error* left-paren))
          ((char= next *right-paren-character*) (progn (read-char stream) ; swallow right paren
                                                       nil))              ; empty list
          (t    (progn (setf next-item (read stream t nil t))
                       (cond ((is-dot-symbol next-item) (progn (setf last-item (read stream t nil t)) ; read one more item
                                                               (swallow-trivia stream)
                                                               (setf next-char (peek-char () stream t nil t))
                                                               (cond ((char= next-char *right-paren-character*) (progn (read-char stream) ; swallow right paren
                                                                                                                       last-item))
                                                                     (t                                         (error-at-location *illegal-end-of-dotted-list-error* next-char)))))
                             (t                         (cons next-item (list-builder stream left-paren))))))))
(defun list-reader (stream left-paren)
    (set-list-item-parent (list-builder stream left-paren)))
(set-macro-character *left-paren-character* (function list-reader))

(defun read-interpreted (&optional (input-stream *terminal-io*) (eof-error-p t) (eof-value nil) (recursive-p nil))
    (swallow-trivia input-stream)
    (setf next (peek-char () input-stream eof-error-p eof-value recursive-p))
    ;                     ^^ swallow-trivia has already run; don't consume anything, do a true peek
    (setf reader-function (get-macro-character-reader next))
    (cond (reader-function  (progn (read-char input-stream) ; swallow
                                   (apply-end-source-location (apply-start-source-location (funcall reader-function input-stream next) next) input-stream)))
          ((char= next *right-paren-character*) (error-at-location *an-object-cannot-start-with-right-paren-error* next))
          (t                (read-symbol-like input-stream))))

(defun eval-stream (stream last-value)
    (setf +eof+  (gensym))
    (setf next   (read stream nil +eof+))
    (cond ((eq next +eof+)  last-value)
          (t                (eval-stream stream (eval next)))))

; sentinel to ensure the script was loaded correctly
t
