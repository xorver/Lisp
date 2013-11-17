;Tomasz Lichon

(defun klucz (L)
	(mapcar 
		#'(lambda (A) (list (append '(nazwisko) (last A))(append '(imie) (butlast A))))
		L
	)
)