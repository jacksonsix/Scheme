; object state

(define (make-acc init)
  (lambda(x)
    (set! init (+ x init))
	init))


(define (make-monitored proc)
  (let ((count 0))
    (lambda(x)
      (cond ((equal? x 'how-many-calls?) count)
            ((equal? x 'reset-count) (set! count 0))
            (else (begin (set! count (+ count 1))
			             (apply proc (list x))))))))
						 
						 
						 
; password account

(define (make-acct init password)
  (let ((pass password)
        (wrong 0)) 
    (define (withdraw x)
	  (if (> init x)
	      (begin (set! init (- init x)) 
		         init)
		  'not-enogh-money))
    (define (deposit x)
      (set! init (+ init x)))
	  
	(define (call-cop x)
      'call-911)
	  
    (define (dispatch passw msg)
	  (if (equal? passw pass)	      
            (begin (set! wrong 0)
			       (cond ((equal? msg 'withdraw) withdraw)
                         ((equal? msg 'deposit) deposit)
                         (else (error "unknown command"))))
			(begin (set! wrong (+ 1 wrong))
			       (if (> wrong 7)
				       call-cop
			           (error "wrong pass")))))	  
    dispatch))
	
        						 
;;;;;;;;;;;;;;random procedure 

 (define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))			

(define (rand-init)
  (let ((init 7))
    (lambda()
      (set! init (rand-update init))
	  init)))

(define rand (rand-init))


(define (rand-command)
  (let ((init 7))
   (define (generate)
     (set! init (rand-update init))
	 init)
   (define (reset x)
     (set! init x)
	 init)
   (define (dispatch msg)
    (cond ((equal? msg 'generate) (generate))
          ((equal? msg 'reset) reset)
          (else (error "no procedure"))))
   dispatch))
   

(define (random-in-range low high)
  (let ((range (- high low)))
     (+ low (random range))))
	 
	  

(define (mont-car trials experiment)
  (define (iter remain suc)
    (cond ((= 0  remain) (/ suc trials))
          ((experiment) (iter (- remain 1) (+ 1 suc)))
          (else (iter (-  remain 1) suc))))
  (iter trials 0))


(define (pi-test)
  (= 1 (gcd (rand) (rand))))

(define (estimate trials)
  (sqrt (/ 6 (mont-car trials pi-test))))

  
(define (def-integral-test)
  (let ((rx (random-in-range 2 8.0))
        (ry (random-in-range 4 10.0)))
    (< (+ (square (- rx 5))
          (square (- ry 7)))
       9)))

(define (def-integral trials)
  (* 36 (mont-car trials def-integral-test)))
  
  
(define (plot)
  (define (iter n i result)
    (if (> n i)
	    (iter n (+ 1 i) (cons (def-integral i) result))
		result))
		
  (iter 100 5 '()))
  
  
; --
(define s 0)
(define (f n)
     (if (and (< n 1)
	          (= s 0)) 
        -1
        (begin (set! s n) s)))

		
(define (mys x)
 (define (loop x y)
   (if (null? x)
       y
	   (let ((temp (cdr x)))
	     (set-cdr! x y)
		 (loop temp x))))

 (loop x '()))
 
;;--   

  (define (contain? m x)
    (if (null? m)
	    false
		(if (eq? (car m) x)
		    true
			(contain? (cdr m) x))))
			
  (define (insert m x)
    (set-cdr! m (cons x (cdr m)))
	m)

(define (count-pairs x) 


  (let  ((mem (cons 'list '())))
   (define (cp x )
	  (cond ((not (pair? x)) 0)
     	    ((contain? mem x)  0)
			(else 
				     (begin (insert mem x)
					  (+ (cp (car x))
					    (cp (cdr x))
					    1)))))
    (cp x)))

	
 
 
  
