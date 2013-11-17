;Tomasz Lichon

(defun suma (A B)
	(setq R '())
	(mapcar 
		#'(lambda (E) (cond
						((null E) E)
						((find E R) E) 
						(T (setq R (append R (list E))) E)
					)
		)
		A
	)
		(mapcar 
		#'(lambda (E) (cond
						((null E) E)
						((find E R) E) 
						(T (setq R (append R (list E))) E)
					)
		)
		B
	)
	R
)