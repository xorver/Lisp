;Tomasz Lichon

(defun my-last (L)
	(cond
		((atom L) L)
		((null (cdr L)) (my-last (car L)))
		(T (my-last (cdr L)))
	)
)