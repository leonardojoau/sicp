; leaf

(define (make-leaf symbol weight)
	(list 'leaf symbol weight))

(define (leaf? object)
	(eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

; a tree is a list of left and right branches, a set of symbols and a weight

(define (make-code-tree left right)
	(list left
		right
		(append (symbols left) (symbols right))
		(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
	(if (leaf? tree)
		(list (symbol-leaf tree))
		(caddr tree)))

(define (weight tree) 
	(if (leaf? tree)
    	(weight-leaf tree)
    	(cadddr tree)))

; decode a string of bits given a tree

(define (decode bits tree)
	(define (decode-1 bits current-branch)
		(if (null? bits)
			'()
			(let ((next-branch (choose-branch (car bits) current-branch)))
				(if (leaf? next-branch)
					(cons (symbol-leaf next-branch)
						(decode-1 (cdr bits) tree))
					(decode-1 (cdr bits) next-branch)))))
	(decode-1 bits tree))

(define (choose-branch bit branch)
	(cond ((= bit 0) (left-branch branch))
		((= bit 1) (right-branch branch))
		(else ((error "bad bit -- CHOOSE-BRANCH" bit)))))

; adjoin function for an ordered list set representation of the weights

(define (adjoin-set x set)
	(cond ((null? set) (list x))
		((< (weight x) (weight (car set))) (cons x set))
		(else (cons (car set)
			(adjoin-set x (cdr set))))))

; construct ordered set of leaves from list of pairs (symbol weight)

(define (make-leaf-set pairs)
	(if (null? pairs)
		'()
		(let ((pair (car pairs)))
			(adjoin-set (make-leaf (car pair) (cadr pair))
				(make-leaf-set (cdr pairs))))))

; encode message to a string of bits given a tree

(define (encode message tree)
	(define (encode-symbol symbol tree)
		(define (symbol-in-list? symbol symbol-list)
			(cond ((null? symbol-list) false)
				((eq? symbol (car symbol-list)) true)
				(else (symbol-in-list? symbol (cdr symbol-list)))))
		(cond ((leaf? tree) '())
			((symbol-in-list? symbol (symbols (left-branch tree))) (append '(0) (encode-symbol symbol (left-branch tree))))
			((symbol-in-list? symbol (symbols (right-branch tree))) (append '(1) (encode-symbol symbol (right-branch tree))))
			(else (error "Invalid symbol" symbol))))
	(if (null? message)
		'()
		(append (encode-symbol (car message) tree)
			(encode (cdr message) tree))))

; generate the Huffman encoding tree

(define (generate-huffman-tree pairs)
	(define (successive-merge set)
		(if (or (null? set) (null? (cdr set)))
			set
			(let ((left-branch (car set))
				(right-branch (cadr set)))
				(successive-merge
					(adjoin-set (make-code-tree left-branch right-branch)
					(cddr set)))
				)))
	(successive-merge (make-leaf-set pairs)))

; test on the example from figure 2.18

(define pairs '((a 8) (b 3) (c 1) (d 1) (e 1) (f 1) (g 1) (h 1)))
(display (generate-huffman-tree pairs))
(newline)


