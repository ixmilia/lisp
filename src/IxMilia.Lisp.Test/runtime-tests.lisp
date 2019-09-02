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
            (&& nil
                (undefined-function)))
            nil)
        "short-circuit-and")

(defun short-circuit-or ()
    (assert
        (|| t
            (undefined-function)))
        "short-circuit-or")

(&& (add-list)
    (sub-list)
    (mul-list)
    (div-list)
    (neg)
    (short-circuit-and)
    (short-circuit-or))
