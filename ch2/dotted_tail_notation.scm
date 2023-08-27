(define (same-parity i . l)
	(cond ((null? l) 
			(list i))
		((= (remainder i 2) (remainder (car l) 2))
			(cons i (apply same-parity l)))
		(else
			(if (null? (cdr l))
				(list i)
				(apply same-parity (cons i (cdr l))))
			)
	))

(display (same-parity 2 3 4 5 6 7))