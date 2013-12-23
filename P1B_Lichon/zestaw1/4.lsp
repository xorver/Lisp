;Tomasz Lichon

(defun suma (A1 B1)
	(let 
		(
			(A A1)
			(B B1)
			(R '() )
		)
		(RSORT A #'(lambda (X Y) 
				(string< 
					(write-to-string X) 
					(write-to-string Y)
				)
			)
		)
		(RSORT B #'(lambda (X Y) 
				(string< 
					(write-to-string X) 
					(write-to-string Y)
				)
			)
		)	
		(mapcar 
			#'(lambda (E) (cond
					((null E) E)
					((my-find  E R) E) 
					(T (setq R (append R (list E))) E)
				)
			)
			A
		)
		(mapcar 
			#'(lambda (E) (cond
					((null E) E)
					((my-find E R) E) 
					(T (setq R (append R (list E))) E)
				)
			)
			B
		)
		R
	)
)

(defun RSORT (DATA FUN)
	(map 'list 
		#'(lambda (X) 
			(cond 
				((listp X) (RSORT X FUN))
			)
		)
		DATA
	)
	(sort DATA FUN)
)