(define (accu op init sequence)
 (if (null? sequence) 
	  init
	  (op (car sequence)
		  (accu op init (cdr sequence)))))					
					
(define matrix (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(define (dot-product v w)
  (accu + 0 (map * v w)))
  
  
(define v (list 1 2 3))
(define w (list 4 5 6)) 

(define m (list (list 1 2 3) (list 4 5 6)))

;;;(define (accumulate-n op init seqs)
;;;  (if (null? (car seqs))
;;;      nil
;;;      (cons (accu op init <??>)
;;;            (accumulate-n op init <??>))))
			

(define (m*vector m v)
	(map (lambda (x) (dot-product x v))
	     m))
		 

     
		 
	
;;	
(define (first seqs)
  (if (matrix? seqs)	
      (accu (lambda (x y) ( cons (car x) 
								(first (cdr seqs))))
	      ()
		  seqs
	  )
	  ()
	  ))
(define (rest seqs)
 (if  (matrix? seqs)	
		(accu (lambda (x y) (cons (cdr x) (rest (cdr seqs)))) 
		      ()
			  seqs
		)
		()
		))

;;		
		 
(define (transpose m)
  (if (matrix? m)
       (cons 		    
		  (first m)       
           (transpose (rest m)))
		()
		)) 


;;;(define (m*matrix m n)
		  
		  
(define (matrix? m)
  (if (pair? m)
	  (pair? (car m))
	   false
	  ))		
			
