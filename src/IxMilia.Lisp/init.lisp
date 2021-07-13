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

(defun find-if (pred items)
    (cond ((eql nil items)              nil)                            ; not found
          ((funcall pred (car items))   (car items))                    ; found it
          (t                            (find-if pred (cdr items)))))   ; recurse

(defun remove-if (pred items)
    (cond ((eql nil items)              nil)                                                ; done
          ((funcall pred (car items))   (remove-if pred (cdr items)))                       ; skip it
          (t                            (cons (car items) (remove-if pred (cdr items))))))  ; keep building

(defun remove-if-not (pred items)
    (cond ((eql nil items)              nil)                                                    ; done
          ((funcall pred (car items))   (cons (car items) (remove-if-not pred (cdr items))))    ; keep building
          (t                            (remove-if-not pred (cdr items)))))                     ; skip it

(defun reduce (fn items)
    (cond ((eql nil items)              nil)                                                                                ; nothing to do
          ((eql 1 (length items))       (car items))                                                                        ; return single value
          (t                            (reduce fn (cons (funcall fn (first items) (second items)) (rest (rest items))))))) ; combine first two and recurse

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

(defun mapcar (fn items)
    (cond ((eql nil items)      ())                                                         ; end of list
          (t                    (cons (funcall fn (car items)) (mapcar fn (cdr items))))))  ; perform replacement on head

; just to ensure the script was properly loaded
t
