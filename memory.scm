(define reg1 'unassigned)
(define reg2 'unassigned)

(define the-cars (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
(define the-cdrs (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

(define (list-set! set off val)
  (if (= off 0)
      (set-car! set val)
      (list-set! (cdr set) (- off 1) val)))
      
      
(define (assign target op oprand)
   ((cdr op) orand))
