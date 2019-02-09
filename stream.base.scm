(define (add-streams s1 s2)
  (cons-stream (+ (stream-car s1)
                   (stream-car s2))
         (add-streams (stream-cdr s1) (stream-cdr s2))))

(define (scale-stream s1 scale)
  (stream-map (lambda(x)
                   (* x scale))
               s1))

(define (show-stream stream n)
    (if (= 0 n) 'done
          (begin (display (stream-car stream))
                 (display ",")
                 (show-stream (stream-cdr stream) (- n 1)))))



(define (show-stream-file stream n)
  (let ((p (open-output-file "data.csv")))
    (define (show-stream s num)
      (if (= 0 num) 'done
          (begin (write (stream-car s) p)
                 (write-char #\, p)
                 (show-stream (stream-cdr s) (- num 1)))))
    (show-stream stream n)
    (close-output-port p)))
