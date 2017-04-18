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


  
(define (scaletree scale t)
  (cond ((null? t) '())
        ((not (pair? t)) (* t scale))
        (else (cons (scaletree scale (car t))
                    (scaletree scale (cdr t))))))  
		


(define (scale-tree scale tr)
  (map (lambda (sub) 
             (cond ((not (pair? sub)) (* scale sub))
				   (else (scale-tree scale sub))))
       tr))


(define (tree-map func tr)
  (map (lambda (sub) 
              (if (pair? sub)
                  (tree-map func sub)
                  (func sub)))
       tr))

	   
(define (subsets s)
  (cond ((null? s) '())
        ((null? (cdr s))  (list '() s))
        (else (let ((rest (subsets (cdr s))))
                (append rest (map (lambda (x) 
		                            (cond ((null? x) (list (car s)))
    							          ((pair? x) (cons (car s) x))
						                  (else (cons  (car s) (list x))))) 
							rest))))))
		 


		 
	   
;;;; setup a convention of calculation flow, signal flow

(define (filter f sequence)
  (cond ((null? sequence) '())
        ((f  (car sequence)) (cons (car sequence) (filter f (cdr sequence))))
		(else (filter f (cdr sequence)))))
		
		
(define (accum  combin  init sequence)
  (if   (null? sequence) 
         init
		 (combin (car sequence) (accum combin init (cdr sequence)))))
		 
		 


(define (sum-odd-square sequence)
  (accum +
         0
		 (map  square
               (filter odd? sequence))))
			   
		            
(define (maxs e sequence)
  (if (null? sequence)
       e
      (maxs (max e (car sequence)) (cdr sequence))))
	  
	  
;(define (map p sequence)
;  (accum (lambda (x y) 
;              (cons (p x)  y))
;         '()
;         sequence))
		 
  	   
(define (append seq1 seq2)
  (accum cons
         seq2
         seq1))		 
		 
		 
(define (length sequence)	
  (accum (lambda(x y) (+ 1 y))
         0
		 sequence))
		 
	   
	   
(define (horner x coeff)
  (accum (lambda (thiscoff highercoff)
                 (+ thiscoff (* x highercoff)))
         0
         coeff))	


(define (count-leaf tree)
  (accum (lambda (f rest) (+ 1 rest))
         0         	      
		 (fringe  tree)))
						   

			   
(define (accum-n op init seqs)
  (if (null? (car seqs))
      '()
	  (cons (accum op init (map car seqs))
	        (accum-n op init (map cdr seqs)))))
			
			
;;;;;;;;;;matrix reprented by sequences

(define x (list 1 2 3))
(define y (list 4 5 6))

(define (para v w)
  (if (null? v)
      '()
      (cons (* (car v) (car w))
         (para (cdr v) (cdr w)))))     
		

(define (dot v w)
  (accum +
         0
		 (para v w)))
		 

(define (m*vector m v)
  (map (lambda (m1)
          (dot m1 v))
       m))


(define (transpose mat)
  (accum-n  cons
            '()
			 mat))

			 
(define (m*m m n)
  (let ((cols (transpose n)))
       (map (lambda (t)
	          (m*vector cols t))
			m)))

			
;;;;; fold-right , fold-left

(define (fold-left op init sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init sequence))


(accum / 
      1
      (list 1 2 3))


(fold-left / 
           1
		   (list 1 2 3))	


(define (reverse seq)	
   (fold-left  (lambda(x y)
                  (cons  y x))
               '()
			    seq))	

(define (reverse seq)
  (fold-right (lambda (x y)
                 (append y (list x)))
              '()
              seq))
			  
	  
		 
			   
