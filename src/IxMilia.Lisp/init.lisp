;;; we can't yet read raw characters, so we have to use `(code-char ...)` and `(function ...)`
(defun process-function-reference (item)
    (cond ((symbolp item)                           (eval (list (quote function) item))) ; #'some-function-reference
          ((and (listp item)
                (eql (car item) (quote lambda)))    (kernel:make-lambda-function item)) ; #'(lambda ...); `kernel:make-lambda-function` is defined internally
          (t                                        (error "Expected function reference or lambda"))
    )
)
(defun single-hash-reader (stream char)
    (let ((next-char (peek-char nil stream t nil t)))
        (cond ((char= next-char (code-char 92)) (progn (read-char stream t nil t) ; swallow backslash (code 92 = \)
                                                       (read-char stream t nil t))) ; keep next raw character
                                                       ; TODO: handle `#\SPACE`, etc.
              ((char= next-char (code-char 39)) (progn (read-char stream t nil t) ; swallow single quote (code 39 = ')
                                                       (process-function-reference (read stream t nil t))))
              ((or (char= next-char (code-char 48))  ; code 48 = 0
                   (char= next-char (code-char 49))  ; code 49 = 1
                   (char= next-char (code-char 50))  ; code 50 = 2
                   (char= next-char (code-char 51))  ; code 51 = 3
                   (char= next-char (code-char 52))  ; code 52 = 4
                   (char= next-char (code-char 53))  ; code 53 = 5
                   (char= next-char (code-char 54))  ; code 54 = 6
                   (char= next-char (code-char 55))  ; code 55 = 7
                   (char= next-char (code-char 56))  ; code 56 = 8
                   (char= next-char (code-char 57))) ; code 57 = 9
                                                     (kernel:process-list-forward-reference)) ; `kernel:process-list-forward-reference` is defined internally
              (t                                     (error "Expected back slash, single quote, or digit"))
        ) ; end of `cond`
    ) ; end of `let`
) ; end of `defun`
(set-macro-character (code-char 35) (function single-hash-reader)) ; code 35 = #

;;;  from here on forward we can do things like `#\X`, `#'function-ref`, etc.

(defun single-quote-reader (stream char)
    (list (quote quote) (read stream t nil t)))
(set-macro-character #\' #'single-quote-reader)

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

; just to ensure the script was properly loaded
t
