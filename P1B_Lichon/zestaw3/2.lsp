;Tomasz Lichon

(defun MKSTR (SEP &rest DN)
	(cond
		((null (cdr DN))
			(cond
				((stringp (car DN)) (car DN) )
				((vectorp (car DN)) (write-to-string (coerce (car DN) 'list)) )
				((listp (car DN)) (write-to-string (car DN)) )
				(t (string (car DN)))
			)
		)
		(t
			(string-concat
				(string-concat
					(cond 
						((stringp (car DN)) (car DN) )
						((vectorp (car DN)) (write-to-string (coerce (car DN) 'list)) )
						((listp (car DN)) (write-to-string (car DN)) )
						(t (string (car DN)))
					)
					SEP
				) 
				(apply #'MKSTR SEP (cdr DN))
			)
		)
	)
)

(defmacro MAKESTR (&rest tekst)
	(cond 
		((and (null (cdr `,tekst)) (numberp (car '`,tekst)))
			`(string (car ',tekst))
		)
		((null (cdr `,tekst))
			`(write-to-string (car ',tekst))
		)
		(t
			`(string-concat
				(string-concat
					(cond
						((numberp (car ',tekst)) (write-to-string (car ',tekst)))
						(t  (string (car ',tekst)))
					)
					" "
				)
				(MAKESTR ,@(cdr `,tekst))
			)
		)
	)
)

(defun MFIND-IF (predicate seq &key (from-end nil) (start 0) (end nil) (key nil))
	(cond
		((or (not (eql start 0)) (not (null end)) )
			(MFIND-IF predicate (subseq seq start end) :from-end from-end :start 0 :end nil :key key)
		)
		(from-end
			(MFIND-IF predicate (reverse seq) :from-end nil :start start :end end :key key)
		)
		((null key)
			(MFIND-IF predicate seq :from-end from-end :start start :end end :key #'(lambda (X) X))
		)
		((eql 0 (length seq))
			nil
		)
		((funcall predicate (funcall key (elt seq 0)) )
			(elt seq 0)
		)
		(t
			(MFIND-IF predicate (subseq seq 1) :key key)   
		)
		
	)
)

(defun LICZBA (L)
	(let 
		(
			(LST (write-to-string L)) 
			(POS (position #\. (coerce (write-to-string L) 'list)))
		)
		(cond 
			((null POS)
				(reverse (LICZBA-CALKOWITA  (reverse LST)))
			)
			(t
				(append (reverse (LICZBA-CALKOWITA (reverse (subseq LST 0 POS)))) '(#\.) (reverse (LICZBA-CALKOWITA (reverse (subseq LST (+ POS 1))))) )
			)
		)
	)
)
(defun LICZBA-CALKOWITA (L)
	(LET
		(
			(mul 1)
		)
		(mapcar 
			#'(lambda (X) (setq mul (* mul 10)) (* (truncate mul 10) (parse-integer (string x))) ) 
			(COERCE L 'list)
		)
	)
)

(defun SPLIT-TYPE (TYPE SOURCE)
	(LET
		(
			(liczby '())
			(listy '())
			(pozostale '())
		)
		(map 'list #'(lambda (X)
				(cond
					((listp X)
						(setq listy (cons X listy))
					)
					((numberp X)
						(setq liczby (cons X liczby))
					)
					(t
						(setq pozostale (cons X pozostale))
					)
				)
			)
			SOURCE
		)
		(cond
			((eql TYPE 'string)
				(string-concat
					(string-concat
						(write-to-string (sort liczby #'<))
						(RSORT listy 
							#'(lambda (X Y) 
								(string< 
									(write-to-string X) 
									(write-to-string Y)
								)
						)
					)
					)
					(write-to-string (sort pozostale #'string<))
				)
			)
			(t
				(funcall TYPE
					(sort liczby #'<)
					(RSORT listy 
						#'(lambda (X Y) 
							(string< 
								(write-to-string X) 
								(write-to-string Y)
							)
						)
					)
					(sort pozostale #'string<)
				)
			)
		)
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

(defun MMERGE (RESULT-TYPE DATA1 DATA2 PREDICATE &key (key nil) (msort nil))

	(cond
		((null key)
			(MMERGE RESULT-TYPE DATA1 DATA2 PREDICATE :key #'(lambda (X) X) :msort msort)
		)
		(msort
			(rsort DATA1 #'(lambda (X Y) (funcall PREDICATE (write-to-string (funcall key X)) (write-to-string (funcall key Y)))))
			(rsort DATA2 #'(lambda (X Y) (funcall PREDICATE (write-to-string (funcall key X)) (write-to-string (funcall key Y)))))
			(MMERGE RESULT-TYPE DATA1 DATA2 PREDICATE :key key)
		)
		(t
			(MERGE RESULT-TYPE DATA1 DATA2 #'(lambda (X Y) (funcall PREDICATE (write-to-string (funcall key X)) (write-to-string (funcall key Y)))) )
		)
	)
)
