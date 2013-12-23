(defun  LIST- (L1 L2 &key (niedomiar 0))
	(cond
		((PIERWSZA-WIEKSZA L2 L1)
			(cons '- (LIST- L2 L1))
		)
		((and (null L2) (> niedomiar 0) )
			(LIST- L1 '(0) :niedomiar niedomiar)
		)
		((null L2)
			L1
		)
		((>= (- (car (last L1)) (car (last L2)) niedomiar) 0)
			(append  
				(LIST- (butlast L1) (butlast L2)) 
				(list (- (car (last L1)) (car (last L2)) niedomiar))
			)
		)
		((< (- (car (last L1)) (car (last L2)) niedomiar) 0)
			(append  
				(LIST- (butlast L1) (butlast L2) :niedomiar 1) 
				(list (- (+ (car (last L1)) 10) (car (last L2)) niedomiar))
			)
		)
	)
)

(defun PIERWSZA-WIEKSZA (L1 L2)
	(cond
		((and (null L1) (null L2))
			nil
		)
		((> (length L1) (length L2))
			t
		)
		((< (length L1) (length L2))
			nil
		)
		((> (car L1) (car L2))
			t
		)
		((< (car L1) (car L2))
			nil
		)
		(t
			(PIERWSZA-WIEKSZA (cdr L1) (cdr L2))
		)
	)
)