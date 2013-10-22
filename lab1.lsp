(defun plus (a b) 
    (cond 
		((and (numberp a) (numberp b)) (+ a b))
		(t nil)
	)
)
(defun sumakw (L)
    (cond 
		((null L) 0) 
		(T (+ (* (car L) (car L)) (sumakw (cdr L))))
	)
)
(defun liczba (L)
	(cond 
		((null L) 0) 
		(T (+ 1 (liczba (cdr L))))
	)
)
(defun lnum (L)
	(cond 
		((null L) 0) 
		((numberp (car L)) (+ 1 (lnum (cdr L)))) 
		(T (+ 0 (lnum (cdr L))))
	)
)
(defun my-max (L)
	(cond 
		((null L) nil) 
		((numberp (car L)) (cond 
								((numberp (my-max (cdr L))) (max (car L) (my-max (cdr L))))
								(T (car L))
							)
		)
		(T (my-max (cdr L)))
	)
)
(defun potega (A N)
	(cond 
		((= N 0) 1)
		((= N 1) A)
		((< N 0) (potega (/ 1 A) (- N)))
		(T (* A (potega A (- N 1))))
	)
)
(defun listnump (L)
	(cond
		((null L) nil)
		((listp L) (or (listnump (car L)) (listnump (cdr L))))
		((numberp L) T)
		(T nil)
	)
)

























