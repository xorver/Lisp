;Tomasz Lichon

(defun my-find (el L)	
	(cond
		((null L) nil)
		((atom L) nil)
		(T (or (equal el (car L)) (my-find el (cdr L)) (my-find el (car L)) ))
	)
)