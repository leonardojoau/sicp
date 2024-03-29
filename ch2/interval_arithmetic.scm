(define (add-interval x y)
	(make-interval 
		(+ (lower-bound x) (lower-bound y))
		(+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
(let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	        (p3 (* (upper-bound x) (lower-bound y)))
	        (p4 (* (upper-bound x) (upper-bound y))))
	    (make-interval (min p1 p2 p3 p4)
	                   (max p1 p2 p3 p4))))

(define (div-interval-dummy x y) (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

;define lower and upper bounds

(define (lower-bound x) (car x))

(define (upper-bound x) (cdr x))

;define subtraction operation

(define (add-interval x y)
	(make-interval 
		(- (lower-bound x) (upper-bound y))
		(- (upper-bound x) (lower-bound y))))

;assert that div-interval throws an error when dividing by an interval that spans 0

(define (div-interval x y) 
	(if (and (<= (lower-bound y) 0) 
		(>= (upper-bound y) 0))
		(error "Interval spans 0")
		(div-interval-dummy x y)))

(display (div-interval 
	(make-interval 2 3)
	(make-interval -1 1)))
