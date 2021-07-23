(defun null (x) (equal () x))

(defmacro if (pred tv fv)
    (cond (pred tv)
          (t fv)))

(defun not (pred)
    "negates the predicate"
    (cond (pred nil)
          (t t)))

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

; just to ensure the script was properly loaded
t
