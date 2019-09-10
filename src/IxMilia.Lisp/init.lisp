(defmacro if (pred tv fv)
    (cond (pred tv)
          (t fv)))

(defun not (pred)
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

; just to ensure the script was properly loaded
t
