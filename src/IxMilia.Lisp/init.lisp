;;; we can't yet read raw characters, so we have to use `(code-char ...)` and `(function ...)`

(setf *double-quote-character*      (code-char 34)) ; "
(setf *hash-character*              (code-char 35)) ; #
(setf *single-quote-character*      (code-char 39)) ; '
(setf *left-paren-character*        (code-char 40)) ; (
(setf *digit-zero-character*        (code-char 48)) ; 0
(setf *digit-nine-character*        (code-char 57)) ; 9
(setf *less-than-character*         (code-char 60)) ; <
(setf *upper-case-c*                (code-char 67)) ; C
(setf *backslash-character*         (code-char 92)) ; \
(setf *lower-case-c*                (code-char 99)) ; c

;; read double-quoted strings
(defun build-double-quoted-string (stream output-stream)
    (let ((next-char (read-char stream t nil t)))
        (cond ((char= next-char *double-quote-character*)   (get-output-stream-string output-stream)) ; done; return what we have
              ((char= next-char *backslash-character*)      (progn (write-char (read-char stream t nil t) output-stream) ; append the next character
                                                                   (build-double-quoted-string stream output-stream))) ; continue reading
              (t                                            (progn (write-char next-char output-stream) ; append the read character
                                                                   (build-double-quoted-string stream output-stream)))))) ; continue reading
(defun double-quote-reader (stream char)
    (build-double-quoted-string stream (make-string-output-stream)))
(set-macro-character *double-quote-character* (function double-quote-reader))

;;; from here on forward we can use double-quoted string literals like `"this is a string"`, etc.

;; read hash characters
(defun process-complex-number (number-list)
    (cond ((and (listp number-list)
                (= (length number-list) 2))         (complex (car number-list) (car (cdr number-list))))
          (t                                        (error "Expected exactly two numbers"))))
(defun single-hash-reader (stream char)
    (let ((next-char (peek-char nil stream t nil t)))
        (cond
              ;; error on:
              ;;   #<foo>
              ((char= next-char *less-than-character*)      (error "Unreadable object"))
              ;; prepare reader for:
              ;;   #\A
              ((char= next-char *backslash-character*)      (progn (read-char stream t nil t) ; swallow backslash
                                                                   (read-char stream t nil t))) ; keep next raw character
                                                                   ; TODO: handle `#\SPACE`, etc.
              ;; prepare reader for:
              ;;   #'some-function-reference
              ;;   #'(lambda ...)
              ((char= next-char *single-quote-character*)   (progn (read-char stream t nil t) ; swallow single quote (code 39 = ')
                                                                   (eval (list (quote function) (read stream t nil t)))))
              ;; prepare reader for:
              ;;   #(1 2 3)
              ((char= next-char *left-paren-character*)     (apply (function vector) (read stream t nil t)))
              ;; prepare reader for:
              ;;   #C(1 2)
              ((or (char= next-char *upper-case-c*)
                   (char= next-char *lower-case-c*))        (progn (read-char stream t nil t) ; swallow 'c'
                                                                   (process-complex-number (read stream t nil t))))
              ;; prepare reader for:
              ;;   #1=(1 2 #1#)
              ((and (>= (char-code next-char) (char-code *digit-zero-character*))
                    (<= (char-code next-char) (char-code *digit-nine-character*)))
                                                            (kernel:process-list-forward-reference)) ; `kernel:process-list-forward-reference` is defined internally
              ;; not a supported character
              (t                                            (error "Expected back slash, single quote, '(', 'c', or digit")))))
(set-macro-character *hash-character* (function single-hash-reader)) ; code 35 = #

;;; from here on forward we can do things like `#\X`, `#'function-ref`, etc.

(defun single-quote-reader (stream char)
    (list (quote quote) (read stream t nil t)))
(set-macro-character #\' #'single-quote-reader)

;;; from here on forward we can do things like `'some-symbol`, `'(1 2 3)`, etc.

(defun back-quote-list-handler (items)
    (cond
        ((eq nil items)                                 ;; special case, end of list
                                                        nil)
        ((listp (car items))                            ;; recurse into sublist
                                                        (list 'cons
                                                              (back-quote-list-handler (car items))
                                                              (back-quote-list-handler (cdr items))))
        (t                                              ;; quote item and move on
                                                        (list 'cons
                                                              (list 'quote (car items))
                                                              (back-quote-list-handler (cdr items))))))
(defun back-quote-comma-reader (stream char)
    (eval (read stream t nil t)))
(defun back-quote-reader (stream char)
    (let ((*readtable*  (copy-readtable)))
        (set-macro-character #\, #'back-quote-comma-reader)
        (let ((item     (read stream t nil t)))
            (cond
                ((listp item)     (back-quote-list-handler item))
                (t                (list 'quote item))))))
(set-macro-character #\` #'back-quote-reader)

;;; from here on forward we can do things like ``(1 ,(+ 2 3))`, etc.

(defun null (x) (equal () x))

(defmacro if (pred tv fv)
    (cond (pred tv)
          (t fv)))

(defun not (pred)
    "negates the predicate"
    (cond (pred nil)
          (t t)))

(defmacro incf (var &optional (amt 1))
    (setf var (+ var amt)))

(defmacro decf (var &optional (amt 1))
    (setf var (- var amt)))

(defmacro push (value stack)
    (setf stack (cons value stack)))

(defmacro pop (stack)
    (let ((result (car stack)))
        (setf stack (cdr stack))
        result))

(defmacro when (pred &rest body)
    (cond (pred (apply #'progn 'body))
          (t    nil)))

(defmacro unless (pred &rest body)
    (cond (pred nil)
          (t    (apply #'progn 'body))))

(defun atom (a)
    ; an atom is anything other than a cons cell
    (not (consp a)))

(defun posnump (n)
    (and (numberp n) (plusp n)))

(defun second (items)
    (first (rest items)))
(defmacro cadr (items)
    (second items))

(defun third (items)
    (first (rest (rest items))))
(defmacro caddr (items)
    (third items))

(defun nthcdr (n items)
    (if (= n 0)
        items
        (nthcdr (- n 1) (cdr items))))

(defun nth (n x)
    "Returns the Nth element of the list X, counting from 0"
    (car (nthcdr n x)))

(defun last (items)
    (cond ((eql nil (cdr items))    items)                  ; last of a proper list
          ((listp (cdr items))      (last (cdr items)))     ; not the end of still a list
          (t                        items)))                ; end of improper list

(defun member (item items)
    (cond ((eql nil items)          ())                             ; not found
          ((eql item (car items))   items)                          ; found it
          (t                        (member item (cdr items)))))    ; check deeper

(defun subsetp (subset superset)
    (cond ((eql nil subset)                 t)                                  ; always true
          ((member (car subset) superset)   (subsetp (cdr subset) superset))    ; found first, check rest
          (t                                nil)))                              ; missing element

(defun assoc (key table)
    (cond ((eql key (car (car table)))  (car table))                ; found it
          ((consp (cdr table))          (assoc key (cdr table)))))  ; check deeper if more items remain

(defun rassoc (key table)
    (cond ((eql key (cdr (car table)))  (car table))                ; found it
          ((consp (cdr table))          (rassoc key (cdr table))))) ; check deeper if more items remain

(defun sublis (table items)
    (let ((keypair (assoc (car items) table)))
        (cond ((eql nil items)  ())                                                 ; empty/end of list
              (keypair          (cons (cdr keypair) (sublis table (cdr items))))    ; perform replacement on head
              (t                (cons (car items) (sublis table (cdr items)))))))   ; no key/value pair found; continue

(defun subst (r s items)
    (sublis (list (cons s r)) items))

(defun nsublis (table items)
    (let ((keypair (assoc (car items) table)))
        (cond ((eql nil items)  ())                                                                     ; empty/end of list
              (keypair          (cons (setf (car items) (cdr keypair)) (nsublis table (cdr items))))    ; perform replacement on head
              (t                (cons (car items) (nsublis table (cdr items)))))))                      ; no key/value pair found; continue

(defun nsubst (r s items)
    (nsublis (list (cons s r)) items))

(defun append (&rest lists)
    (reduce #'kernel:append/2 lists))

(defun terpri (&optional (output-stream *terminal-io*))
    (format output-stream "~%"))

(defun prin1 (value &optional (output-stream *terminal-io*))
    (format output-stream "~s~%" value)
    value)

(defun princ (value &optional (output-stream *terminal-io*))
    (format output-stream "~a~%" value)
    value)

(defun print (value &optional (output-stream *terminal-io*))
    (apply #'terpri (list output-stream))
    (apply #'prin1 (cons value output-stream))
    (apply #'princ (cons " " output-stream))
    value)

(defun yes-or-no-p (prompt)
    (labels ((prompt-and-get-input ()
                (format t prompt)
                (let ((response (read)))
                    (cond ((eql response 'yes) t)
                          ((eql response 'no) nil)
                          (t (prompt-and-get-input))))))
        (prompt-and-get-input)))

(defun y-or-n-p (prompt)
    (labels ((prompt-and-get-input ()
                (format t prompt)
                (let ((response (read)))
                    (cond ((eql response 'y) t)
                          ((eql response 'n) nil)
                          (t (prompt-and-get-input))))))
        (prompt-and-get-input)))

; math
(defun + (&rest values)
    (reduce #'kernel:+/2 (cons 0 values)))

(defun - (&rest values)
    (cond ((equal () values)            (error "At least one argument required"))
          ((equal 1 (length values))    (kernel:-/1 (car values)))
          (t                            (reduce #'kernel:-/2 values))))

(defun * (&rest values)
    (reduce #'kernel:*/2 (cons 1 values)))

(defun / (&rest values)
    (cond ((equal () values)            (error "At least one argument required"))
          ((equal 1 (length values))    (/ 1 (car values)))
          (t                            (reduce #'kernel://2 values))))

(defpackage :common-lisp-user
    (:use :common-lisp))
(in-package :common-lisp-user)

; just to ensure the script was properly loaded
t
