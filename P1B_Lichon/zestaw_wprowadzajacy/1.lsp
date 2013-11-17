;Tomasz Lichon

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