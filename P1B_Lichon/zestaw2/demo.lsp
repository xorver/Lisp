;Tomasz Lichon

(load "1.lsp")
(load "2.lsp")


(SETQ PRZYSTANKI  '( (CZARNOWIEJSKA (NAME . CZARNOWIEJSKA)(NBRS (KAWIORY . 3) (PLAC_INWALIDOW . 5) (AGH . 3) (NAWOJKI . 5) )  )
                     (KAWIORY (NAME . KAWIORY)(NBRS (CZARNOWIEJSKA . 3) (NAWOJKI . 3) (KROLEWSKA . 7) (AZORY . 15) ))
                     (NAWOJKI (NAME . NAWOJKI)(NBRS (KAWIORY . 3) (AZORY . 15) (CZARNOWIEJSKA . 5) ) )
                     (AGH (NAME . AGH)(NBRS (CZARNOWIEJSKA . 5) (KIJOW . 4) (PLAC_INWALIDOW . 8) ) )
                     (KIJOW (NAME KINO KIJOW)(NBRS (JUBILAT . 7) (AGH . 4) ) )
                     (JUBILAT (NAME . JUBILAT)(NBRS (KIJOW . 7)  (PROKOCIM . 40) ) )
                     (AZORY (NAME . AZORY)(NBRS (KAWIORY . 15) (NAWOJKI . 18) ) )
                     (PROKOCIM (NAME . PROKOCIM)(NBRS (JUBILAT . 40) ) )
                     (PLAC_INWALIDOW (NAME PLAC INWALIDOW)(NBRS (AGH . 8) ) )
                     (KROLEWSKA (NAME . KROLEWSKA)(NBRS (KAWIORY . 7) ) )
	)
)

(SETQ TRASY '((103 (NAME . 103)(ROUTE PROKOCIM JUBILAT KIJOW AGH CZARNOWIEJSKA KAWIORY AZORY) )
              (103P (NAME . 103)(ROUTE AZORY KAWIORY CZARNOWIEJSKA AGH KIJOW JUBILAT PROKOCIM) )
              (173 (NAME . 173) (ROUTE PROKOCIM JUBILAT KIJOW AGH CZARNOWIEJSKA NAWOJKI AZORY) )
              (173P (NAME . 173)(ROUTE AZORY NAWOJKI CZARNOWIEJSKA AGH KIJOW JUBILAT PROKOCIM) )
))

(SETF TREE '(D D1 D2 D3 D4 D11 D12)) 
(SETF D '((NAME . "LISP")(P . 1)(UP)(DOWN D1 D2 D3 D4)))
(SETF D1 '((NAME . "Listy") (P . 0.45)(UP . D)(DOWN D11 D12 D13))) 
(SETF D2 '((NAME . "Tablice")(P . 0.2)(UP . D) (DOWN)))
(SETF D3 '((NAME . "Liczby")(P . 0.2)(UP . D) (DOWN)))
(SETF D4 '((NAME . "Stringi")(P . 0.15)(UP . D) (DOWN)))
(SETF D11 '((NAME . "Proste operacje na listach")(P . 0.3)(UP . D1) (DOWN)))
(SETF D12 '((NAME . "Listy asocjacyjne")(P . 0.35)(UP . D1) (DOWN))) 
(SETF D13 '((NAME . "Przebudowywanie list")(P . 0.35)(UP . D1) (DOWN)))

(defun demo (&optional n)
	(cond
		((null n)
			(print "zestaw 2 zadanie 1 i 2")
		)
		((= n 11)
			(print (B-ASSOC 'NBRS 'PROKOCIM PRZYSTANKI))
			(print (NBRSP 'PROKOCIM 'JUBILAT :depots PRZYSTANKI))
			(print (PTIME 'PROKOCIM 'JUBILAT :depots PRZYSTANKI))
			(print (DEPOTS :name nil :depots PRZYSTANKI))
			(ROUTE '103 :bus TRASY)
		)
		((= n 12)
			(CONNECT 'GRUNWALDZKIE 'JUBILAT 6 :depots PRZYSTANKI)
		)
		((= n 13)
			(CHECK-ROUTEP 173 :depots PRZYSTANKI :bus TRASY)
		)
		((= n 14)
			(WAY 'PROKOCIM 'AGH :depots PRZYSTANKI :bus TRASY)
		)
		((= n 15)
			(HMT :start 'PROKOCIM :end 'AGH :depots PRZYSTANKI :bus TRASY)
		)
		((= n 21)
			(print (ROOT))
			(print (DOWN 'D))
			(print (UP 'D1))
			(LINKP 'D1 'D11)
			
		)
		((= n 22)
			(DLIST)
		)
		((= n 23)
			(ADD 'D5 'D 'nazwa 0.5)
			(print TREE)
			(print D)
			(print D1)
			(print D2)
			(print D3)
			(print D4)
			D5
		)
		((= n 24)
			(DEL 'D1)
			(print TREE)
			(print D)
			(print D2)
			(print D3)
			D4
		)
		((= n 25)
			(CHECKP)
		)
	)
)











