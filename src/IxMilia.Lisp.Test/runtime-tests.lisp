(defun add-list ()
    (assert
        (= 10 (+ 1 2 3 4))
        "add-list"))

(defun sub-list ()
    (assert
        (= 1 (- 10 4 3 2))
        "sub-list"))

(defun mul-list ()
    (assert
        (= 24 (* 1 2 3 4))
        "mul-list"))

(defun div-list ()
    (assert
        (= 1 (/ 24 4 3 2))
        "div-list"))

(defun neg ()
    (assert
        (= -1 (- 1))
        "neg"))

(defun short-circuit-and ()
    (assert
        (=
            (and nil
                (undefined-function))
            nil)
        "short-circuit-and"))

(defun short-circuit-or ()
    (assert
        (or t
            (undefined-function))
        "short-circuit-or"))

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
    (assert
        (= nil ())
        "nil-equality"))

(defun list-helpers ()
    (setq l '(4 5 6))
    (assert (= 3 (length l)) "list length")
    (assert (= 4 (first l)) "list first")
    (assert (= 5 (second l)) "list second")
    (assert (= 6 (third l)) "list third")
    (assert (= '(5 6) (rest l)) "list rest")
    (assert (= 4 (car l)) "list car")
    (assert (= nil (car ())) "() car")
    (assert (= nil (cdr ())) "() cdr")
    (assert (= nil (third ())) "() third")
    (assert (= nil (third nil)) "nil third")
    (assert (= '(1) (cons 1 nil)) "nil cons")
    (assert (= '(1 2 3) (cons 1 '(2 3))) "list cons")
    (assert (= '(1 2 3) (list 1 2 3)) "create list")
    (assert (= '(1 . 2) (cons 1 2)) "dotted list"))

(defun common-helpers ()
    (assert (= '(1 2) (quote (1 2))) "quote")
    (assert (= 3 (eval '(+ 1 2))) "eval"))

(defun test-cond ()
    (assert (= 'a (cond ((< 1 2) 'a)
                        ((not-a-function-and-will-error) 'b))) "cond only evals as far as necessary")
    (assert (= 'b (cond ((< 2 1) 'a)
                        ((< 1 2) 'b))) "cond falls through to find proper value")
    (assert (= () (cond ((< 2 1) 'a)
                        ((= 1 2) 'b))) "cond defaults to nil"))

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
