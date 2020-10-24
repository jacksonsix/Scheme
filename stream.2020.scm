

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
  

   
      	       
