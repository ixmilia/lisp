(defmacro if (pred tv fv)
    (cond (pred tv)
          (t fv)))

; just to ensure the script was properly loaded
t
