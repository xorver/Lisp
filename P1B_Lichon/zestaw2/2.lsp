;Tomasz Lichon

(defun ROOT (&OPTIONAL (TREE TREE))
	(car (remove nil (mapcar
			#'(lambda (X) (cond
					((null (cdr (assoc 'UP (eval X))))
						X
					)
				)				
			)
			TREE
	)))
)

(defun DOWN (D  &OPTIONAL (TREE TREE))
	(cond
		((find D TREE)
			(cdr (assoc 'DOWN (eval D)))
		)
	)
)

(defun UP (D &OPTIONAL (TREE TREE))
	(cond
		((find D TREE)
			(cdr (assoc 'UP (eval D)))
		)
	)
)

(defun LINKP (D1 D2 &OPTIONAL (TREE TREE))
	(or (equal (UP D1) D2) (equal (UP D2) D1))
)

(defun DLIST (&OPTIONAL (ROOT (ROOT TREE)) (TREE TREE))
	(cond 
		((null (DOWN ROOT TREE)) 
			ROOT
		)
		(T 
			(cons ROOT (mapcan #'(lambda (X) (list (DLIST X TREE))) (DOWN ROOT TREE)) )
		)
	)
)

(defun ADD (New Parent Name P &OPTIONAL (TREE TREE))
	(rplacd (last TREE) (list New))
	(set New `((NAME . ,Name) (P . ,P) (UP . ,Parent) (DOWN)))
	(DOWN Parent)
	(mapcar 
		#'(lambda (NODE) 
			(nsubstitute `(P ,(* (cdr (assoc 'P (eval NODE))) (- 1 P))) (assoc 'P (eval NODE)) (eval NODE))
		) 
		(DOWN Parent TREE)
	)
	(rplacd (last (assoc 'DOWN (eval Parent))) (list New))
	TREE
)

(defun DEL-CHLD (Dn &OPTIONAL (TREE TREE))
	(cond
		((not (null (DOWN Dn TREE)))
			(mapcar #'(lambda (X) (DEL-CHLD X)) (DOWN Dn))
		)
	)
	(delete Dn TREE)
	(unintern Dn)
)

(defun DEL (Dn &OPTIONAL (TREE TREE))
	(cond 
		((equal Dn (ROOT)) 
			(mapcar #'(lambda (X) (unintern X)) TREE) 
			(unintern 'TREE)
		)
		(T
			(setq P (cdr (assoc 'P (eval Dn))))
			(delete Dn (assoc 'DOWN (eval (UP Dn TREE))))
			(mapcar #'(lambda (X) 
					(nsubstitute (cons 'P (/ (cdr (assoc 'P (eval X))) (- 1 P))) (assoc 'P (eval X)) (eval X))
				) 
				(DOWN (UP Dn TREE) TREE)
			)
			(DEL-CHLD Dn TREE)
		)
	)
)

(defun IS-ZERO (X) 
	(and (> X (- 0.0001)) (< X 0.0001))
)

(defun CHECKP (&OPTIONAL (ROOT (ROOT TREE)) (TREE TREE))

	(not(member nil 
		(mapcar #'(lambda (X) 
				(cond
					((null(DOWN X TREE)) 
						T
					)
					(T
						(IS-ZERO 
							(- 1 
								(reduce '+ (mapcar #'(lambda (Y) (cdr (assoc 'P (eval Y)))) (DOWN X TREE)))
							)
						)			
					)
				)
			) 
		(DOWN ROOT TREE)
		)
	))
)







