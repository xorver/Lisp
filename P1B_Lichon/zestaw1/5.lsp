;Tomasz Lichon

(defun my-subst (EL1 EL2 L)
	(cond
		((null L)
			nil
		)
		((equal (car L) EL1)
			(cons EL2 (my-subst EL1 EL2 (cdr L)))
		)
		((listp (car L))
			(cons (my-subst EL1 EL2 (car L)) (my-subst EL1 EL2 (cdr L)))
		)
		(t
			(cons (car L) (my-subst EL1 EL2 (cdr L)))
		)
	)
)