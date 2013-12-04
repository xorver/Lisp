;Tomasz Lichon

(defun B-ASSOC (PROP IDENT DEPOTS)
	(cdr (assoc PROP (cdr (assoc IDENT DEPOTS))))
)

(defun NBRSP (P1 P2 &key (depots PRZYSTANKI))
	(and (assoc P2 (B-ASSOC 'NBRS P1 PRZYSTANKI)) (assoc P1 (B-ASSOC 'NBRS P2 PRZYSTANKI)))
)

(defun PTIME (P1 P2 &key (depots PRZYSTANKI))
	(cdr (assoc P1 (B-ASSOC 'NBRS P2 PRZYSTANKI)) )
)

(defun DEPOTS (&key name (depots PRZYSTANKI))
	(cond
		(name (mapcar #'(lambda (X) (assoc 'NAME (cdr X))) PRZYSTANKI))
		(T (mapcar #'(lambda (X) (car X)) PRZYSTANKI))		
	)
)

(defun ROUTE (NR_POJAZDU &key (bus TRASY))
	(B-ASSOC 'ROUTE NR_POJAZDU TRASY)
)

(defun CONNECT (P1 P2 time &KEY (depots PRZYSTANKI))
	(setq new1 `(,P1 (NAME . ,P1) (NBRS (,P2 . ,time))))
	(setq new2 `(,P2 (NAME . ,P2) (NBRS)))
	(cond
		((null (assoc P1 PRZYSTANKI)) 
			(setq PRZYSTANKI (cons new1 PRZYSTANKI))
		)
		((null (assoc P2 (B-ASSOC 'NBRS P1 PRZYSTANKI)))
			(rplacd (last (B-ASSOC 'NBRS P1 PRZYSTANKI)) `((,P2 . ,time)))
		)
		(T
			(rplacd (assoc P2 (B-ASSOC 'NBRS P1 PRZYSTANKI)) time)
		)
	)
	(cond
		((null (assoc P2 PRZYSTANKI)) 
			(setq PRZYSTANKI (cons new2 PRZYSTANKI))
		)
	)
	PRZYSTANKI
)

(defun ROUTEP-REK (TRASA PRZYSTANKI)
	(cond 
		((null (cdr TRASA))
			T
		)
		(
			(and (NBRSP (car TRASA) (car (cdr TRASA))) (ROUTEP-REK (cdr TRASA) PRZYSTANKI))
		)
	)
)

(defun CHECK-ROUTEP (NR_BUSA &key (depots PRZYSTANKI) (bus TRASY))
	(setq TRASA (B-ASSOC 'ROUTE NR_BUSA TRASY))
	(cond 
		((ROUTEP-REK TRASA PRZYSTANKI)
			TRASA
		)
		(T
			nil
		)
	)
)


(defun WAY (P1 P2 &key (depots PRZYSTANKI) (bus TRASY))
	(setq NUMERY (mapcar #'(lambda (X) (car X)) TRASY))
	(remove nil 
		(mapcar #'(lambda (NR)
				(setq TRASA (member P1 (CHECK-ROUTEP NR :depots PRZYSTANKI :bus TRASY)))
				(cond
					((and (not (null TRASA)) (not(null (member P2 TRASA))))
						(rplacd (member P2 TRASA) '())
						`(,NR ,TRASA)
					)
					(T
						nil
					)
					
				)
			)
			NUMERY
		)
	)
)

(defun LIST-PTIME (LIST PRZYSTANKI)
	(cond
		((null (cdr LIST))
			0
		)
		(T
			(+ (PTIME (car LIST) (car (cdr LIST)) :depots PRZYSTANKI) (LIST-PTIME (cdr LIST) PRZYSTANKI))
		)
	)
)

(defun HMT (&KEY start end list (depots PRZYSTANKI) (bus TRASY))
	(cond
		((null list)
			(setq list (WAY start end :depots PRZYSTANKI :bus TRASY))
			(mapcar
				#'(lambda (X)
					`(,(car X) ,(LIST-PTIME (cadr X) PRZYSTANKI))
				)
				list
			)
		)
		(T
			(HMT (car list) (last list) :depots PRZYSTANKI :bus TRASY)
		)
	)
)
