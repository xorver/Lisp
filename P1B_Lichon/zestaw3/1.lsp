;Tomasz Lichon

(defmacro MY-WHEN (test &body body)
	`(cond
		(,test ,@body)
	)
)

(defmacro MY-CASE (X &body body)
	(cons 'cond
		(mapcar (lambda (Y) 
				(cond
					((eql 'otherwise (car Y)) `(t ,(cadr `,Y)) )
					((eql 't (car Y)) `(t ,(cadr `,Y)) )
					(t `((eql ,X ,(car `,Y)) ,(cadr `,Y)) )
				)
			) 
		`,body) 
	)
)

(defmacro DOTIMES2D ((I NI J NJ RESULT) &body BODY)
	`(loop for ,I from 0 below ,NI do 
		(loop for ,J from 0 below ,NJ do ,@BODY)
	)
)

(defmacro DO-NOT-PRIME((INDEX START END &optional RESULT) &body BODY)
	`(loop for ,INDEX from ,START below ,END do 
		(cond 
			((not (PRIMEP ,INDEX)) ,@BODY) 
		)
	)
	
)

(defun PRIMEP (N &optional (K 2))
	(cond
		((= N 0) nil)
		((= N 1) nil)
		((= N 2) t)
		((> (* K K) N ) t)
		((= 0 (mod N K)) nil )
		(t (PRIMEP N (+ K 1)))
	)
)

(defmacro DO-IF((RULE INDEX START END &optional RESULT) &body BODY)
	`(loop for ,INDEX from ,START below ,END do 
		(cond 
			((funcall ,RULE ,INDEX) ,@BODY) 
		)
	)
	
)

(defmacro INIT2D(m n)
	`(let 
		((tab (make-array '(,m ,n))))
		(DOTIMES2D (i ,m j ,n t) (setf (aref tab i j) 0))
		(eval tab)
	)
)

(defmacro FILL2D (TAB2D VALUE &optional RULE)
	`(DO-IF (,RULE i 0 (array-dimension ,TAB2D 0)) 
		(DO-IF (,RULE j 0 (array-dimension ,TAB2D 1)) 
			(setf (aref tab i j) ,VALUE)
		)
	)
)

(defun PRINT2D (TAB2D)
	(DOTIMES (i (array-dimension TAB2D 0)) 
		(DOTIMES (j (array-dimension TAB2D 1)) 
			(prin1 (aref TAB2D i j)) (format t " ")
		) 
		(format t "~%") 
	)
	
)
