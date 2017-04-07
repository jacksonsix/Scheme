(define (small-div n x)
  (cond ((> (square n) x) x)
        ((= (remainder x n) 0) n)
		(else (small-divisor (+ n 1) x))))


(define (prime? n)
  (= (small-div 2 n) n))

(define (timedfunc  func)
  (display '*****)
  (newline)
  func)
  
  
(define (compose f g) 
  (lambda (x)
    (f (g x))))


(define (inc x)
  (+ x 1))	
	
((compose square inc) 5)	


(define (repeated f n)
  (if (= n 1)
      f
	  (compose f (repeated f (- n 1)))))
		


  
(define dx  0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) 
	      (g x))
	    dx)))
		
(define (newton-trans g)
  (lambda (x)
    (-x (/ (g x) ((deriv g) x)))))

(define (fixed-point f guess)
  (define (close-enough? v1 v2)
    (<  (abs (- v1 v2)) dx))
  (define (try guess)	
    (let ((value (f guess)))
	   (newline)
	   (display value) 
       (if (close-enough? guess value)
            value
           (try value))))
  (try guess))

  
(define (newton-method g guess)
  (fixed-point (newton-trans g)  guess))

(define (avg v1 v2)
  (/ (+ v1 v2)
      2))  
  
(define (search g neg pos)
  (define (close-enough? v1 v2)
    (<  (abs (- v1 v2)) dx))
  (let ((mid (avg neg pos)))
    (if (close-enough? neg pos)
        mid
        (let ((value (g mid)))
		     (cond ((> value 0)   (search g neg mid))
                   ((< value 0)   (search g mid pos))
                   (else mid))))))
			  
(define (half-method f a b)
  (let ((va (f a))
        (vb  (f b)))
    (cond ((and (< va 0)  (> vb 0))  (search f a b))
          ((and (> va 0)  (< vb 0))   (search f b a))
          (else (error 'no))))) 	


		  
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
		 
;;;;;;;;;;;;;;;;;;;;;;;;;;   combine , bottom up



					


(define (cont-frac n d k)
  (define (trans term k)
    (if (= 1 k)
      term
	  (trans (/ (n (- k 1))
	            (+ (d (- k 1))
     		    	term)) (- k 1))))
  (trans (/ (n k) (d k)) k))
 
			   
(cont-frac (lambda(i) 1.0) (lambda (i) 1.0) 200)


(define (d i)
 (cond ((= 1 (remainder i 3)) 1)
       ((= 2 (remainder i 3)) (+ (floor (/ i 3)) 1))
       ((= 0 (remainder i 3)) 1)
       (else (error 'd))))
	   
(define (n i) 1.0)

(cont-frac n d 100)   ;;;;;;;;;e value


;;;; (tan x)

(define (d i)
  (- (* 2 i) 1))
  
  

(define (tanc x k)
  (define (n i)   (- 0 (square x)))
  (/ (cont-frac n d k)
     x))
	 
       
(tanc 0.5 30)			   



(define (double f)
  (lambda(x)
    ;(newline)
	(display 'double)
    (f (f x))))
	
(((double (double double)) inc) 5)
	
;;;;;;;;;;;;;;;;;;;;;;;;
			   
(define (smoothing g)
   (lambda (x) (avg (g x) (g (- x dx)))))
   

(define (n-fold-smoothing n)
   (repeated smoothing n)) 
