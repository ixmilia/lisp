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

(defun second (list)
    (first (rest list)))
(defmacro cadr (list)
    (second list))

(defun third (list)
    (first (rest (rest list))))
(defmacro caddr (list)
    (third list))

(defun nthcdr (n list)
    (if (= n 0)
        list
        (nthcdr (- n 1) (cdr list))))

(defun nth (n x)
    "Returns the Nth element of the list X, counting from 0"
    (car (nthcdr n x)))

(defun last (list)
    (cond ((= nil (cdr list)) list)                 ; last of a proper list
          ((listp (cdr list)) (last (cdr list)))    ; not the end of still a list
          (t                  list)))               ; end of improper list

(defun remove (item list)
    (cond ((= nil list)        ())                                              ; noop
          ((= item (car list)) (remove item (cdr list)))                        ; skip item, remove from rest
          (t                   (cons (car list) (remove item (cdr list))))))    ; keep first, remove from rest

(defun member (item list)
    (cond ((= nil list)        ())                          ; not found
          ((= item (car list)) list)                        ; found it
          (t                   (member item (cdr list)))))  ; check deeper

(defun subsetp (subset superset)
    (cond ((= nil subset)                   t)                                  ; always true
          ((member (car subset) superset)   (subsetp (cdr subset) superset))    ; found first, check rest
          (t                                nil)))                              ; missing element

(defun assoc (key table)
    (cond ((= key (car (car table)))    (car table))                ; found it
          ((consp (cdr table))          (assoc key (cdr table)))))  ; check deeper if more items remain

(defun rassoc (key table)
    (cond ((= key (cdr (car table)))    (car table))                ; found it
          ((consp (cdr table))          (rassoc key (cdr table))))) ; check deeper if more items remain

(defun subst (r s list)
    (cond ((= nil list)     ())                                         ; empty list
          ((= s (car list)) (cons r (subst r s (cdr list))))            ; replace head
          (t                (cons (car list) (subst r s (cdr list)))))) ; no match, do nothing

; just to ensure the script was properly loaded
t
