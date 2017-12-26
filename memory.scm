(define (list-set! set off val)
  (if (= off 0)
      (set-car! set val)
      (list-set! (cdr set) (- off 1) val)))
      
      
