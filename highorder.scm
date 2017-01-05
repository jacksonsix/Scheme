;;; wrap procedure, high order
(define (const2) 
    2)
(define (memo proc)	
   (let ((number 0))
	    (lambda (commnd)
		   (if (equal? commnd 'count)
		       number
			   (begin (set! number (+ number 1))
				       (proc))))))
