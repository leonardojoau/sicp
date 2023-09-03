; binary tree abstraction

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
	(list entry left right))

(define (element-of-set? x set)
	(cond ((null? set) false)
		((= x (entry set)) true)
		((< x (entry set))
			(element-of-set? x (left-branch set)))
		((> x (entry set))
			(element-of-set? x (right-branch set)))
		))

(define (adjoin-set x set)
	(cond ((null? set) (make-tree x '() '()))
		((< x (entry set))
			(make-tree (entry set)
				(adjoin-set x (left-branch set))
				(right-branch set)))
		((> x (entry set))
			(make-tree (entry set)
				(left-branch set)
				(adjoin-set x (right-branch set))))
		))

; construct a binary tree from an ordered list

(define (list->tree elements)
	(car (partial-tree elements (length elements))))

(define (partial-tree elts n)
	(if (= n 0)
		(cons '() elts)
		(let ((left-size (quotient (- n 1) 2)))
			(let ((left-result (partial-tree elts left-size)))
				(let ((left-tree (car left-result))
					(non-left-elts (cdr left-result))
					(right-size (- n (+ left-size 1))))
					(let ((this-entry (car non-left-elts))
						(right-result (partial-tree (cdr non-left-elts) right-size)))
						(let ((right-tree (car right-result))
							(remaining-elts (cdr right-result)))
							(cons (make-tree this-entry left-tree right-tree)
								remaining-elts)))
				)))
))

; collapse the binary tree back to a list

(define (tree->list tree)
	(if (null? tree)
		()
		(append (tree->list (left-branch tree))
			(cons (entry tree) 
				(tree->list (right-branch tree))))))

; intersection implementation based on transforming trees back to lists 

(define (intersection-set set1 set2)
	(define (intersection-set-list set1 set2)
		(if (or (null? set1) (null? set2))
		'()
		(let ((x1 (car set1)) (x2 (car set2)))
		(cond ((= x1 x2)
				(cons x1 (intersection-set-list (cdr set1) (cdr set2))))
		    ((< x1 x2)
		        (intersection-set-list (cdr set1) set2))
			((< x2 x1)
		        (intersection-set-list set1 (cdr set2)))))))
	(let ((list1 (tree->list set1))
		(list2 (tree->list set2)))
		(let ((intersection-list
			(intersection-set-list list1 list2)
			))
		(list->tree intersection-list))
		))

; test intersection
(define set1 (list->tree (list 1 2 3 4 5 6 7 8 9 10)))
(define set2 (list->tree (list 6 7 8 9 10 11 12 13 14 15)))
(display (intersection-set set1 set2))
(newline)