(defmacro eval (form)
    (apply #'progn '(form)))

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
    (reduce #'kernel:two-arg-+ (cons 0 values)))

(defun - (&rest values)
    (cond ((equal () values)            (error "At least one argument required"))
          ((equal 1 (length values))    (kernel:one-arg-- (car values)))
          (t                            (reduce #'kernel:two-arg-- values))))

(defun * (&rest values)
    (reduce #'kernel:two-arg-* (cons 1 values)))

(defun / (&rest values)
    (cond ((equal () values)            (error "At least one argument required"))
          ((equal 1 (length values))    (/ 1 (car values)))
          (t                            (reduce #'kernel:two-arg-/ values))))

; just to ensure the script was properly loaded
t
