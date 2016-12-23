(define x (list 1 2 3))
(define y (list 7 9))

(define (accu op init sequence)
 (if (null? sequence) 
	  init
	  (op (car sequence)
		  (accu op init (cdr sequence)))))
		  
		  

(define (map proc sequence)
	(accu (lambda (x y) (cons (proc x) (map proc (cdr sequence))))
		  ()	
	      sequence
	))		
	
	
(define (len sequence)
  (accu  (lambda (x y) (+ (if (null? (car sequence)) 0 1) (len (cdr sequence))))
         0
		 sequence
  ))
  
  
(define (append s1 s2)
  (accu  cons
		 s1
		 (cons (car s2) (append  s1 (cdr s2)))
  ))  
  
  
  (define coff (list 1 3 0 5 0 1))
  
 (define (horner-eval x coefficient-sequence)
  (accu (lambda (this-coeff higher-terms) (+ this-coeff 
                                                   (* (horner-eval x (cdr coefficient-sequence)) x) ))
              0
              coefficient-sequence)) 
			  
			  
(define (countleaf tree)
  (accu  (lambda (x y) (+ (if (null? (car sequence)) 0 1) (len (cdr sequence))))
        0  
        (map (lambda) tree)
  ))

(define (count-leaf tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
		(else
		   (+ count-leaf(car tree) 
		      count-leaf(cdr tree)))))
  
