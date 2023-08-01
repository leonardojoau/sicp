(define (pascal row pos)
	(if (or (= pos 1) (= pos row))
		1
		(+ (pascal (- row 1) (- pos 1))
	   	   (pascal (- row 1) pos)
	   	)))

(define (display-pascal-iter row pos n)
	(if (<= row n)
		(begin
			(display " ")
			(display (pascal row pos))
			(if (= pos row)
				(begin
					(display "\n")
					(display-pascal-iter (+ row 1) 1 n))
				(display-pascal-iter row (+ pos 1) n))
		)
	)
)

(define (display-pascal n)
	(display-pascal-iter 1 1 n))

(display-pascal 20)