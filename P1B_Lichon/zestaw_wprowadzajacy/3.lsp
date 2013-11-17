;Tomasz Lichon

(defun listnump (L)
	(cond
		((null L) nil)
		((listp L) (or (listnump (car L)) (listnump (cdr L))))
		((numberp L) T)
		(T nil)
	)
)