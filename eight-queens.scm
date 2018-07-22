(define (queens board-size)
  (define (queen_cols k)
    (if (= k 0)
         (list empty-board)
         (filter (lambda (positions)
                    (safe? k positions))
                 (flatmap  
                   (lambda(rest-queens)
                        (map (lambda(new-row)
                               (adjoin-position new-row k rest-queens))
                             (enumerate 1 board-size)))
                    (queen-cols (- k1 ))))))
   
  (queen-cols board-size))
  
  
  (define empty-board ())
  
  (define (safe? k positions)
    ())
    
   (define (adjoin-position new-row k rest-queens)
      ())
      
      
      
