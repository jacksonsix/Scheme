(define (show-stream stream n)
    (if (= 0 n) 'done
          (begin (display (stream-car stream))
                 (display ",")
                 (show-stream (stream-cdr stream) (- n 1)))))

(define (sqrt-improve x num)
   (average x (/ num x)))

(define (sqrt num)
   (define sqrt-stream
        (cons-stream 1.0 
                     (stream-map  (lambda (x)
                                    (sqrt-improve x num))
                                 sqrt-stream)))
   sqrt-stream)

(define (average x y)
  (/ (+ x y) 2))


;; i<=j
(define (pairs s1 s2)
     (cons-stream (concat (stream-car s1) (stream-car s2))
                  (interleave  (stream-map (lambda(x) (concat (stream-car s1) x))
                                           (stream-cdr s2))
                               (pairs (stream-cdr s1) (stream-cdr s2)))))


(define (interleave s1 s2)
    (cons-stream (stream-car s1)
                 (interleave s2 (stream-cdr s1))))




(define (intger-from n)
   (cons-stream n
                (intger-from (+ 1 n))))

(define intgers (intger-from 1))


(define (triples s1 s2 s3)
    (pairs s1 (pairs s2 s3)))

(define (concat l1 l2)
   (cond ((null? l1) l2)
         ((null? l2) l1)
         ((not(pair? l2)) (conc l1 l2))   
         ((not(pair? l1)) (cons l1 l2))        
         (else (cons (car l1) (concat (cdr l1) l2)))))


(define (conc l1 item)
  (cond ((null?  l1) (list item))
        ((not (pair? l1)) (list l1 item))         
        (else (cons (car l1) (conc (cdr l1) item)))))


    

(define pytha
   (stream-filter (lambda(t3)
                    (let ((a1 (car t3))
                          (a2 (cadr t3))
                          (a3 (caddr t3)))
                      (=  (+ (square a1) (square a2))
                          (square a3))))
                   (triples intgers intgers intgers)))


;; userful order
(define (merge-weight s1 s2 weight)
   (cond ((< (weight (stream-car s1)) (weight (stream-car s2)))
          (cons-stream (stream-car s1)
                       (merge-weight  s2  (stream-cdr s1) weight)))
         ((> (weight (stream-car s1)) (weight (stream-car s2)))
          (cons-stream (stream-car s2)
                       (merge-weight (stream-cdr s2) s1 )))
         (else (cons-stream (stream-car s1)
                             (merge-weight  (stream-cdr s2) (stream-cdr s1) weight)))))

(define (weight  item)
      (+ (car item) (cadr item)))
 

(define (weight-pairs s1 s2 weight)
     (cons-stream (concat (stream-car s1) (stream-car s2))
                  (merge-weight  (stream-map (lambda(x) (concat (stream-car s1) x))
                                           (stream-cdr s2))
                                 (weight-pairs (stream-cdr s1) (stream-cdr s2) weight)
                                 weight)))

 
