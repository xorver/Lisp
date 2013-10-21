(defun plus (a b) 
    (cond ((and (numberp a) (numberp b)) (+ a b))
    (t nil))
)

(defun sumakw (L)
    (cond ((null L) 0) (T (+ (* (car L) (car L)) (sumakw (cdr L)))))
)

(defun liczba (L)
	(cond ((null L) 0) (T (+ 1 (liczba (cdr L)))))
)

(defun lnum (L)
	(cond ((null L) 0) ((numberp (car L)) (+ 1 (lnum (cdr L)))) (T (+ 0 (lnum (cdr L)))))
)

(defun my-max (L)
	
)
