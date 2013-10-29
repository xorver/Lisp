(defun sumakw (L)
    (cond 
		((null L) 0) 
		(T (+ (* (car L) (car L)) (sumakw (cdr L))))
	)
)
(defun liczba (L)
	(cond 
		((null L) 0) 
		(T (+ 1 (liczba (cdr L))))
	)
)
(defun lnum (L)
	(cond 
		((null L) 0) 
		((numberp (car L)) (+ 1 (lnum (cdr L)))) 
		(T (+ 0 (lnum (cdr L))))
	)
)
(defun my-max (L)
	(cond 
		((null L) nil) 
		((numberp (car L)) (cond 
								((numberp (my-max (cdr L))) (max (car L) (my-max (cdr L))))
								(T (car L))
							)
		)
		(T (my-max (cdr L)))
	)
)
(defun potega (A N)
	(cond 
		((= N 0) 1)
		((= N 1) A)
		((< N 0) (potega (/ 1 A) (- N)))
		(T (* A (potega A (- N 1))))
	)
)
(defun listnump (L)
	(cond
		((null L) nil)
		((listp L) (or (listnump (car L)) (listnump (cdr L))))
		((numberp L) T)
		(T nil)
	)
)

(defun my-last (L)
	(cond
		((atom L) L)
		((null (cdr L)) (my-last (car L)))
		(T (my-last (cdr L)))
	)
)

(defun rotate-right (L)
	(cond
		((list L) (cons (car (reverse L)) (reverse (cdr (reverse L)))))
		(T nil)
	)
)

(print (sumakw '(1 2)))
(print (liczba '(1 2 a)))
(print (lnum '(1 2 a)))
(print (my-max '(1 2)))
(print (potega 2 (- 2)))
(print (listnump '((aa (a b)) (2) a)))
(print (my-last '((aa (a b)) (2) a)))
(print (rotate-right '(1 (2 3) 4)))

























