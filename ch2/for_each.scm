
;redefinition of the native for-each procedure

(define (for-each f l) 
	(if (not (null? l))
		(begin (f (car l)) (for-each f (cdr l)))))

(for-each (lambda (x) (newline) (display x)) (list 1 2 3))