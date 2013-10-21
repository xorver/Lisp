(defun plus (a b) 
    (cond ((and (numberp a) (numberp b)) (+ a b))
    (t nil))
)

(defun sumakw (L)
    (cond ((null L) 0) (T (+ (* (car L) (car L)) (sumakw (cdr L)))))
)