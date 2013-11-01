;Tomasz Lichon

(defun potega (A N)
	(cond 
		((= N 0) 1)
		((= N 1) A)
		((< N 0) (potega (/ 1 A) (- N)))
		(T (* A (potega A (- N 1))))
	)
)