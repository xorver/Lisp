;Tomasz Lichon

(defun rotate-right (L)
	(cond
		((list L) (cons (car (reverse L)) (reverse (cdr (reverse L)))))
		(T nil)
	)
)