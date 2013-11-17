;Tomasz Lichon

(defun sum (K)
	(apply '+ (mapcar 
				#'(lambda (A) 
					(cond 
						((listp A) (sum A))
						(T A)
					)
				)
				K
			)
	)
)

(defun flatten (L)
	(mapcar
		#'(lambda (A) (cond ((atom A) A) (T (sum A))))
		L
	)
)