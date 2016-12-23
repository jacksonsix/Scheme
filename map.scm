(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))
					
					
					
					
					
					
(define (proc x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else 
	      (+ (proc (car x)) (proc (cdr x)) ))))
      


(define x (list 1 2 3))

(define tree (list 1 2 (list 5 7) 8 ))
					
					
(define (accu op init sequence)
 (if (null? sequence) 
	  init
	  (op (car sequence)
		  (accu op init (cdr sequence)))))					
					
(define matrix (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(define (first seqs)
	(accu (lambda (x y) ( cons (car x) 
								(first (cdr seqs))))
	      ()
		  seqs
	))
	
(define (rest seqs)
		(accu (lambda (x y) (cons (cdr x) (rest (cdr seqs)))) 
		      ()
			  seqs
		))
