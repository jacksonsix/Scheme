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


	
			          	   
;; merge streams

(define twos
        (scale-stream integers 2))
(define fives
     (scale-stream integers 5))
(define threes
      (scale-stream integers 3))

(define (merge s1 s2)
   (if (stream-null? s1)
     s2
	(cons-stream  (stream-car s1)
	               (merge (stream-cdr s1) s2))))
	  
;; merge with order,weight function

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))	

;; interleave
(define (interleave s1 s2)
	(if (stream-null? s1)
         s2
		 (cons-stream (stream-car s1)
                      (interleave s2 (stream-cdr s1)))))

					  
;;; test 
(define s1 
	(cons-stream 1 
		(cons-stream 2 
		(cons-stream 4 (cons-stream 6 (cons-stream 8 the-empty-stream))))))	

(define s2 
	(cons-stream 1 
		(cons-stream 3 
		(cons-stream 5 (cons-stream 7 (cons-stream 9 the-empty-stream))))))	

;;; wrap procedure, high order
(define (const2) 
    2)
(define (memo proc)	
   (let ((number 0))
	    (lambda (a b commnd)
		   (if (equal? commnd 'count)
		       number
			   (begin (set! number (+ number 1))
				       (proc a b))))))
					   
					   
					   
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))					   
					   
;;;
(define m 
     (merge s1 s2))

(define hamming 
    (cons-stream 1
	             (merge (scale-stream hamming 2) 
				     (merge (scale-stream hamming 3)  (scale-stream hamming 5) ))))	

;; long dividen 
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))					 
           	   
			   
;;; integral seriers   

(define poweri
	(cons-stream 1
	             (stream-map (lambda (x) (/ 1 (+ x 1))) integers)))

(define (integrate-powerseries stream)
    (multi-stream poweri stream))
	
(define exp-series
  (cons-stream 1
               (integrate-powerseries exp-series)))	
			   
			   
(define cosine-series
  (cons-stream 1 
               (stream-map - (integrate-powerseries sine-series))))
(define sine-series
  (cons-stream 0 
               (integrate-powerseries cosine-series)))  
			   
;; multi-series
;; get combination of  power n. then add all these pairs up
;;
(define (multi-series s1 s2)
	(cons-stream)		   
