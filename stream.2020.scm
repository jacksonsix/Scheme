

;; explict define

(define (int-n n)
  (cons-stream
   n
   (int-n (+ n 1) )))

(define int (int-n 1))


(define (fib a b)
  (cons-stream
   a
   (fib b (+ a b))))

(define f (fib 0 1))

;; implicit define

(define ones
  (cons-stream 1
	       ones))

(define doubles
  (cons-stream
   1
   (stream-add doubles doubles)))


(define xint
  (cons-stream
   1
   (stream-add ones xint)))



(define fibs
  (cons-stream
   0
   (cons-stream
    1
    (stream-add fibs (stream-cdr fibs)))))

(define (stream-add s1 s2)
  (cons-stream
   (+ (stream-car s1)
      (stream-car s2))
   (stream-add (stream-cdr s1) (stream-cdr s2))))


;; this looks a bit bad
(define (partial-sum s)
  (cons-stream
   (stream-car s)
   (stream-add (stream-cdr s)
	       (partial-sum s))))
	       

;; reform like this

(define (partial-sum1 s)
  (define x
    (cons-stream
     (stream-car s)
     (stream-add (stream-cdr s)
		 x)))
  x)


(define (fact n)
  (define (loop v i)
    (if (> i n)
	v
	(loop (* v i) (+ i 1))))
  (loop 1 1))


(define (mm x n)
  (if (= 0 n)
      1
      (* x (mm x (- n 1)))))

(define (n-stream n)
  (cons-stream
   (/ (mm 'x n) (fact n))
   (n-stream (+ n 1))))

(define ns (n-stream 0))


(define (stream-mul s1 s2)
  (stream-map * s1 s2))

(define fact
  (cons-stream
   1
   (stream-mul fact xint)))



(define e-stream
  (stream-map (lambda(x) (/ 1 x))
	      fact))


(define rev-int
  (stream-map (lambda(x) (/ 1 x))
		     xint))

(define (integ-s s)
   (stream-mul rev-int
	       s))


(define ones
  (cons-stream 1
	       ones))

(define neg-s
  (stream-map (lambda(x) (* x -1))
	      ones))

(define exp-s
  (cons-stream
   1
   (integ-s exp-s)))


(define cos
  (cons-stream
   1
   (stream-mul neg-s
               (integ-s sin))))

(define sin
  (cons-stream
   0
   (integ-s cos)))


;; series-add   series-mul

(define (scale-stream n s)
  (stream-map (lambda(x) (* n x)) s))


;; this multi has some issue , scale to (stream-cdr s1)

(define (series-mul s1 s2)
  (let ((a (stream-car s1))
	(b (stream-car s2)))
    (cons-stream
     (* a b)
     (stream-add
                   (stream-add (scale-stream a (stream-cdr s2))
			       (scale-stream b (stream-cdr s1)))
		   (cons-stream
		    0
		    (series-mul (stream-cdr s1)
				(stream-cdr s2)))))))

;; this version works 

(define (mul-series s1 s2)
   (cons-stream (* (stream-car s1) (stream-car s2))
            (stream-add (scale-stream (stream-car s1)   (stream-cdr s2))
                          (mul-series  s2 (stream-cdr s1)))))


(define (mul-series2 s1 s2)
  (cons-stream
    (* (stream-car s1) (stream-car s2))
    (stream-add (stream-add (scale-stream (stream-car s2)   (stream-cdr s1))
                              (scale-stream  (stream-car s1)  (stream-cdr s2)))
                 (cons-stream 0 (mul-series2 (stream-cdr s1) (stream-cdr s2))))))  
  

   
      	       
;; invert series

(define (invert s)
  (define x 
    (cons-stream
     1
     (stream-map
      (lambda(x) (- x))
      (mul-series (stream-cdr s)
		   x))))
  x)


(define (div-series s1 s2)
  (if (= 0 (stream-car s2))
      'error
      (mul-series
       s1
       (invert s2))))


;;; stream accelaration

(define (transform s)
  (let ((a0 (stream-ref s 0))
	(a1 (stream-ref s 1))
	(a2 (stream-ref s 2)))
    (cons-stream
     (- a2 (/ (square (- a2 a1))
	      (+
	       (- a0 (* 2 a1))
	       a2)))
     (transform (stream-cdr s)))))


(define (pi-sum n)
  (cons-stream
   (/ 1.0 n)
   (stream-map -
	       (pi-sum (+ 2 n)))))

(define pi
  (scale-stream 4 (partial-sum (pi-sum 1))))


  
(define (make-table trans s)
  (cons-stream s
	       (make-table trans (trans s))))

(define pi-super
  (stream-map
   stream-car
   (make-table transform pi)))


(define (stream-ref-n s n)
  (if (= n 0)
      (display (stream-car s))
      (begin
	(newline)
	(display (stream-car s))
	(newline)
	(stream-ref-n (stream-cdr s) (- n 1)))))


(define (alt n)
  (cons-stream
   (/ 1.0 n)
   (stream-map -
	     (alt (+ n 1)))))

(define ln2
  (partial-sum (alt 1)))

(define ln2-super
  (stream-map
   stream-car
   (make-table transform ln2)))


;;;

(define (stream-limit s tolerance)
  (let ((m0 (stream-ref s 0))
	(m1 (stream-ref s 1)))
    (if (<  (abs (- m0 m1)) tolerance)
	m1
	(stream-limit (stream-cdr s) tolerance))))


(define (interleave s t)
  (if (stream-null? s)
      t
      (cons-stream
       (stream-car s)
       (interleave t (stream-cdr s)))))



(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave (stream-map (lambda(x) (list (stream-car s) x))
		      (stream-cdr t))
	  (pairs (stream-cdr s) (stream-cdr t)))))

;;;;  wrong one


(define (ps s t)
  (interleave
   (stream-map (lambda(x) (list (stream-car s) x))
	       t)
   (ps (stream-cdr s) (stream-cdr t))))

;; ?? why inifinite loop

(define (merge-weighted s1 s2 weight)
  (let ((w1 (weight (stream-car s1)))
	(w2 (weight (stream-car s2))))
    (cond ((< w1 w2)
	   (cons-stream
	    (stream-car s1)
	    (merge-weighted (stream-cdr s1) s2 weight)))
	  ((> w1 w2)
	   (cons-stream
	    (stream-car s2)
	    (merge-weighted s1 (stream-cdr s2) weight)))
	  (else
	   (cons-stream
	    (stream-car s1)
	    (merge-weighted (stream-cdr s1) (stream-cdr s2) weight))))))

(define (weight term)
  (+ (* 2 (car term))  (* 3 (cadr term)) (* 5 (car term) (cadr term))))



(define (pairs-weight s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted (stream-map (lambda(x) (list (stream-car s) x))
		      (stream-cdr t))
		   (pairs-weight (stream-cdr s) (stream-cdr t) weight)
		   weight
		   )))


;;; ram numbers

(define (ram-weight term)
  (+  (* (car term) (car term) (car term))
      (* (cadr term)  (cadr term)  (cadr term))))





(define (ram-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted (stream-map (lambda(x) (list (stream-car s) x))
			       (stream-cdr t))
		   (ram-pairs (stream-cdr s) (stream-cdr t))
		   ram-weight)))

(define ram (ram-pairs xint xint))

(define (r lt)
  (let ((r1 (stream-ref lt 0))
	(r2 (stream-ref lt 1)))
    (if (= (ram-weight r1) (ram-weight r2))
	(cons-stream
	 r1
	 (r (stream-cdr lt)))
	(r (stream-cdr lt)))))


(define dt 0.001)


(define (integral intg init dt)
  (define int 
   (cons-stream
   init
   (stream-add (scale-stream dt intg)
	       int)))
  int)

(define (volt  c r  dt)
  (lambda(i v0)
    (stream-add
     (scale-stream r i)
     (scale-stream (/ 1.0 c)
		   (integral i v0 dt)))))




(define (integ-delay  dint init dt)
  (define int
   (cons-stream init
	       (stream-add  (scale-stream dt (force dint))
			    int)))
  int)


(define (solve f y0 dt)
  (define y (integ-delay (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)


(define (solve-2nd dy0 y0 dt)
  (lambda(a b)
  (define y (integ-delay (delay dy) y0 dt))
  (define dy (integ-delay (delay ddy) dy0 dt))
  (define ddy (stream-add (scale-stream a dy)
			  (scale-stream b y)))
  y))


(define (solve-nd f dy0 y0 dt)
  (define y (integ-dealy (delay dy) y0 dt))
  (define dy (integ-delay (delay ddy) dy0 dt))
  (define ddy (stream-map f
			  dy
			  y))
  y)



(define x
  (stream-map (lambda(x y) (+ x y))
	      xint
	      xint))


(define (neg x)
  (- 0 x))


(define (rlc r l c dt)

  (lambda(vc0 il0)
    (define il (integ-delay (delay dil) il0 dt))
    (define vc (integ-delay (delay dvc) vc0 dt))
    (define dil (stream-add
		 (scale-stream (/ 1.0 l) vc)
		 (scale-stream (neg (/ r l)) il)))
    (define dvc (scale-stream (neg (/ 1.0 c)) il))
    (stream-map cons il vc)))


  
     
(define trls (rlc 1 1 0.2 0.001))
(define ww (trls 10 0))
