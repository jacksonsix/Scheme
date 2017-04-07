;;; data abstraction

;;data barrier procedure

;(define (make-rate x y)
;(define (nom z)
;(define (dnom z))

;; define operations in terms of barrier procedure

(define (add-rat x y)
  (make-rate  (+ (* (nom x) (dnom y))
                 (* (dnom x) (nom y)))
              (* (dnom x) (dnom y))))
			  

(define (sub-rat x y)
  (make-rate  (- (* (nom x) (dnom y))
                 (* (dnom x) (nom y)))
              (* (dnom x) (dnom y))))			  

(define (mul-rat x y)
  (make-rate (* (nom x) (nom y))
             (* (dnom x) (dnom y))))

(define (div-rat x y)
    (make-rate (* (nom x) (dnom y))
             (* (dnom x) (nom y))))	

(define (equal-rat? x y)
 (= (* (nom x) (dnom y))
    (* (dnom x) (nom y))))
	
;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-rate x y)
  (cons x y))
  
(define (nom z)
  (car z))

(define (dnom z)
  (cdr z))

  
;; tests

(define x (make-rate 1 2))
(define y (make-rate 3 4))
  
  
  
;; point object and interface procedure

(define (make-point x y)
  (cons x y))
(define (px p)
  (car p))
(define (py p)
  (cdr p))


  
		   

 ;;  operations with point object

(define (mid-point p1 p2)
 (make-point (/ (+ (px p1) (px p2)) 
                2)
			 (/ (+ (py p1) (py p2)) 
                2)))



(define (distance p1 p2)
  (sqrt (+ (square (- (px p1) (px p2)))
           (square (- (py p1) (py p2))))))




  
(define (print-point p)
  (newline)
  (display "(")
  (display (px p))
  (display ",")
  (display (py p))
  (display ")"))

  
  
 
;; line segment object and interface operations
(define (make-segment start end)
  (cons start end))

(define (start segment)
 (car segment))

(define (end segment)
  (cdr segment))

;;; operations on line segment

(define (len segment)
  (distance (start segment) (end segment)))

(define (mid-point segment)
  (mid-point (start segment) (end segment))) 
 
 
 ;;; rectangle object, and interface operations

(define (make-rectangle  vert hor)
  (cons vert hor))

(define (vert rect)
  (car rect))

(define (hor rect)
  (cdr rect))

;;  operations on rectangle object

(define (peri  rect)
  (* 2 (+  (len (vert rect))
           (len (hor rect)))))

(define (area rect)
  (* (len (vert rect)) (len (hor rect))))

  
 
(define p1 (make-point 0 2)) 
(define p2 (make-point 0 0)) 
(define p3 (make-point 4 0)) 

(define vert1 (make-segment p1 p2))
(define hor1 (make-segment p2 p3))

(define rect1 (make-rectangle vert1 hor1))

