(define (last-pair l)
  (if (null? (cdr l))
      l
	  (last-pair (cdr l))))
	  
;;
;(define (append l1 l2)
;  (if (null? l1)
;       l2
;	   (cons (car l1) (append (cdr l1) l2))))
	   
	   
;; pretty slow 	   
(define (reverse l1)
  (if (null? l1)
      '()
      (append (reverse (cdr l1)) (list (car l1)))))
	  
	  
(define (deepreverse l1)
  (cond ((null? l1) '())
        ((not (pair? l1)) l1)
        (else (append (deepreverse (cdr l1)) (list (deepreverse (car l1)))))))
		
		
		
(define (len l1)
  (if (null? l1)
      0
      (+ 1 (len (cdr l1)))))


(define (count-leaf l1)
  (cond ((null? l1) 0)
        ((not (pair? l1)) 1)
        (else (+  (count-leaf (car l1))
                  (count-leaf (cdr l1))))))
				  
				  
(define (fringe l1)				  
  (cond ((null? l1) '())
        ((not (pair? l1)) (list l1))
	    (else (append (fringe (car l1))  (fringe (cdr l1))))))
		
		
		 
	
 ;; mobile object
(define (make-mobile left right)
  (cons left right))

(define (left mb)
  (car mb))

(define (right mb)
  (cdr mb))  
		 
		
;;; branch object

(define (make-branch length struct)		
  (cons length struct))
  
(define (blen bran)
  (car bran))

(define (struct bran)
  (cdr bran))

;;;

(define (wbranch br)
  (if (not (pair? (struct br)))
      (struct br)
	  (weight (struct br))))

(define (weight mob)
  (if (not (pair? mob))
       mob
	   (+ (wbranch (left mob))
	      (wbranch (right mob)))))
		  
        

		   

(define (balance? mob)
  (= (torque (left mob))
     (torque (right mob))))

(define (torque branh)
  (* (blen branh) (wbranch branh)))


(define l (make-branch 3 3))
(define r (make-branch 2 5))


  
   
		
			
