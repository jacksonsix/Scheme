(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)


(define w (solve (lambda (y) y) 1 0.001))


(stream-ref (solve (lambda (y) y) 1 0.001) 1000)


(define (solve2nd f a b y0 dy0 dt)
  (define  y (integral (delay dy) y0 dt))
  (define  dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                            (scale-stream y b)))
  y)

(define sec (solve2nd (lambda(y) y) 2 3 1 1 0.001))


;;3.80

(define (RLC vc0 il0 dt)
  (define vc (integral (delay dvc) vc0 dt))
  (define il (integral (delay dil) il0 dt))
  (define dvc (scale-stream il (- (/ 1 CC))))
  (define dil (add-streams (scale-stream vc (/ 1 L))
                           (scale-stream il (- (/ R L)))))
  (cons vc il))


(define R 1.0)
(define CC 0.2)
(define L 1.0)

(define (solveRLC  dt il0 vc0)
  (RLC vc0 il0 dt))

(define d (solveRLC  0.1 0 10.0))

  
