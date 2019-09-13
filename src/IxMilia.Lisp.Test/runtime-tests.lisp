(defun assert (pred message)
    "Asserts that the given predicate is true, otherwise fails"
    (if pred t (fail message)))

(defun assert-eq (expected actual message)
    "Asserts the given values are equal, otherwise fails"
    (assert (= expected actual) (join "Error:" message ": Expected" expected "but received" actual)))

(defun add-list ()
    (assert-eq 10 (+ 1 2 3 4) "add-list"))

(defun sub-list ()
    (assert-eq 1 (- 10 4 3 2) "sub-list"))

(defun mul-list ()
    (assert-eq 24 (* 1 2 3 4) "mul-list"))

(defun div-list ()
    (assert-eq 1 (/ 24 4 3 2) "div-list"))

(defun neg ()
    (assert-eq -1 (- 1) "neg"))

(defun short-circuit-and ()
    (assert-eq nil (and nil (undefined-function)) "short-circuit-and"))

(defun short-circuit-or ()
    (assert (or t (undefined-function)) "short-circuit-or"))

(defun built-in-predicates ()
    (assert
        (and     (symbolp 'a)
            (not (symbolp 3))
                 (numberp 1)
            (not (numberp "one"))
                 (zerop 0)
            (not (zerop 1))
                 (evenp 2)
            (not (evenp 3))
                 (oddp 1)
            (not (oddp 2))
                 (listp '(1))
                 (listp ())
            (not (listp 4))
                 (consp '(1))
            (not (consp ()))
            (not (consp 4))
                 (atom 4)
                 (atom ())
            (not (atom '(1))))
        "built-in-predicates"))

(defun nil-equality ()
    (assert-eq nil () "nil-equality 1")
    (assert-eq () nil "nil-equality 2"))

(defun list-helpers ()
    (setq l '(4 5 6))
    (assert-eq 3 (length l) "list length")
    (assert-eq 4 (first l) "list first")
    (assert-eq 5 (second l) "list second")
    (assert-eq 6 (third l) "list third")
    (assert-eq '(5 6) (rest l) "list rest")
    (assert-eq 4 (car l) "list car")
    (assert-eq nil (car ()) "() car")
    (assert-eq nil (cdr ()) "() cdr")
    (assert-eq nil (third ()) "() third")
    (assert-eq nil (third nil) "nil third")
    (assert-eq '(1) (cons 1 nil) "nil cons")
    (assert-eq '(1 2 3) (cons 1 '(2 3)) "list cons")
    (assert-eq '(1 2 3) (list 1 2 3) "create list")
    (assert-eq '(1 . 2) (cons 1 2) "dotted list")
    (assert-eq '(a b c d e f) (append '(a b c) '(d e f)) "append list")
    (assert-eq '(a b c . d) (append '(a b c) 'd) "append value")
    (assert-eq '(c b a) (reverse '(a b c)) "reverse list")
    (assert-eq '(a b c) (nthcdr 0 '(a b c)) "nthcdr 0")
    (assert-eq '(b c) (nthcdr 1 '(a b c)) "nthcdr 1")
    (assert-eq '(c) (nthcdr 2 '(a b c)) "nthcdr 2")
    (assert-eq nil (nthcdr 3 '(a b c)) "nthcdr 3")
    (assert-eq nil (nthcdr 4 '(a b c)) "nthcdr 4")
    (assert-eq '(c . d) (nthcdr 2 '(a b c . d)) "nthcdr 2 improper list")
    (assert-eq 'd (nthcdr 3 '(a b c . d)) "nthcdr 3 improper list")
    (assert-eq 'a (nth 0 '(a b c)) "nth 0")
    (assert-eq 'b (nth 1 '(a b c)) "nth 1")
    (assert-eq 'c (nth 2 '(a b c)) "nth 2")
    (assert-eq nil (nth 3 '(a b c)) "nth 3")
    (assert-eq '(c) (last '(a b c)) "last proper")
    (assert-eq nil (last nil) "last nil")
    (assert-eq '(c . d) (last '(a b c . d)) "last improper")
    (assert-eq '(b n n) (remove 'a '(b a n a n a)) "remove item")
    (assert-eq '(b a n a n a) (remove 'x '(b a n a n a)) "remove nothing")
    (assert-eq '(a b c) (member 'a '(a b c)) "member first")
    (assert-eq '(b c) (member 'b '(a b c)) "member second")
    (assert-eq '(c) (member 'c '(a b c)) "member third")
    (assert-eq nil (member 'x '(a b c)) "member none")
    (let* ((l1 '(a b c))
           (l2 '(d c a))
           (int (intersection l1 l2)))
        (assert-eq 2 (length int) "intersection length")
        (assert (member 'a int) "intersection 1a")
        (assert (member 'c int) "intersection 1c"))
    (assert-eq nil (intersection '(a b c) '(d e f)) "intersection 2")
    (let* ((l1 '(f h a))
           (l2 '(e f o l))
           (un (union l1 l2)))
        (assert-eq 6 (length un) "union length")
        (assert (member 'f un) "union 1-1")
        (assert (member 'h un) "union 1-2")
        (assert (member 'a un) "union 1-3")
        (assert (member 'e un) "union 1-4")
        (assert (member 'o un) "union 1-5")
        (assert (member 'l un) "union 1-6"))
    (let* ((l1 '(a b c d))
           (l2 '(b c))
           (sd (set-difference l1 l2)))
        (assert-eq 2 (length sd) "set-difference 1 length")
        (assert (member 'a sd) "set-difference 1-1")
        (assert (member 'd sd) "set-difference 1-2"))
    (assert-eq nil (set-difference '(a b) '(b a)) "set-difference 2")
    (assert-eq t (subsetp '(a i) '(a e i o u)) "subsetp 1")
    (assert-eq nil (subsetp '(a x) '(a e i o u)) "subsetp 2")
    (setf words
        '((one uno)
          (two dos)
          (three tres)
          (four quatro)))
    (assert-eq '(two dos) (assoc 'two words) "assoc 2")
    (assert-eq '(three tres) (assoc 'three words) "assoc 3")
    (assert-eq nil (assoc 'six words) "assoc 6")
    (setf words
        '((one . uno)
          (two . dos)
          (three . tres)
          (four . quatro)))
    (assert-eq '(two . dos) (rassoc 'dos words) "rassoc 2")
    (assert-eq '(three . tres) (rassoc 'tres words) "rassoc 3")
    (assert-eq nil (rassoc 'seis words) "rassoc 6")
)

(defun common-helpers ()
    (assert-eq '(1 2) (quote (1 2)) "quote")
    (assert-eq 3 (eval '(+ 1 2)) "eval"))

(defun test-cond ()
    (assert-eq 'a (cond ((< 1 2) 'a)
                        ((not-a-function-and-will-error) 'b)) "cond only evals as far as necessary")
    (assert-eq 'b (cond ((< 2 1) 'a)
                        ((< 1 2) 'b)) "cond falls through to find proper value")
    (assert-eq () (cond ((< 2 1) 'a)
                        ((= 1 2) 'b)) "cond defaults to nil"))

(and (add-list)
     (sub-list)
     (mul-list)
     (div-list)
     (neg)
     (short-circuit-and)
     (short-circuit-or)
     (built-in-predicates)
     (nil-equality)
     (list-helpers)
     (common-helpers)
     (test-cond)
)
