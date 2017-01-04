;;;stream
(define ones (cons-stream 1 ones))


(define (begin-n n)
     (cons-stream n
	              (begin-n (+ n 1))))
(define integers  (begin-n 1))	

(define (fibgen a b)
	(cons-stream a
	             (fibgen b (+ a b))))			
				 
;;; map filter to stream

(define no-sevens-filter		
  (lambda (x)
     (not (= 0 (remainder x 7)))))
	 
	 
(define no-sevens 	 
  	(stream-filter no-sevens-filter integers))	 
	
;;(stream-ref no-sevens 3)	


(define (sieve stream)
	(cons-stream (stream-car stream)
	              (sieve (stream-filter 
				                        (lambda (x) (not (= 0 (remainder x (stream-car stream)))))
										(stream-cdr stream)))))
										
										
										
;;;
(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream scale)
   (stream-map (lambda (x) (* x scale))
               stream))   
  
(define int 
   (cons-stream 1 
               (add-streams ones int)))  
			   
			   
(define fib
 (cons-stream 0
              (cons-stream 1 
			               (add-streams   fib (stream-cdr fib)))))
						   
						   
						   
;;;

(define primes
    (cons-stream 2
             (stream-filter prime? 
			                (stream-cdr integers))))


(define (prime? x)
   (define (iter n)
     (cond ((> n (sqrt x)) true)
	       ((= 0 (remainder x n)) false)
           (else (iter (+ n 1)))))
	(iter 2))	   
    		 
		   
;;; factorial  two streams work together

(define factorial 
   (cons-stream 1
                (multi-stream factorial (begin-n 2))))


(define (multi-stream s1 s2)
	(cons-stream (* (stream-car s1) (stream-car s2))
                  (multi-stream (stream-cdr s1) (stream-cdr s2))))
				  
	
(define (partial-sum stream)
   (add-streams   stream
                 (cons-stream  0 
				               (partial-sum stream))))
                     
				  
;;; (stream-ref stream 1)		   

(define (display-stream s n)
	(define (iter stream number)
	   (if (= 0 number)
	        'done
			(begin 
				  (newline)
				  (display (stream-car stream))
				  (iter (stream-cdr stream) (- number 1)))))
	(iter s n))		


	
			          	   
		   
