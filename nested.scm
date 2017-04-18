;;;;nested mapping  2 or more free variable
(define (accum  combin  init sequence)
  (if   (null? sequence) 
         init
		 (combin (car sequence) (accum combin init (cdr sequence)))))

(define (flatmap proc sequence)
  (accum  append
          '()
		  (map proc
		       sequence)))
		 
		 
(define (enumerate-interval m n)
  (if (> m n)
      '()
	  (cons m 
	        (enumerate-interval (+ m 1) n))))

(define (pairs n)
  (accum 
            append
			'()
			(map (lambda(x)
			       (map (lambda(y)
				           (list x y))
				        (enumerate-interval 1 (- x 1))))
				   
                 (enumerate-interval 1 n))))
				 
				 

;;; problem -->  rest part (cdr set) is not enough for the rest of each element
; (define (perm set)
  ; (if (null? set)
      ; (list '())
     ; (let ((rest (perm (cdr set))))
       ; (map 
	        ; (lambda(e)
              ; (map (lambda(r)
                     ; (cons e r))   			  
			       ; rest))
            ; set))))	



(define (remove set e)
  (filter (lambda(x)
             (not (= x e)))
          set))

		  
(define (perm set)
  (if (null? set)
      (list '())
      (map 
           (lambda(e)
               (map (lambda(r)
                       (append (list e) r))
                    (perm (remove set e))))
            set)))					
		  
		  
;;;;;;;;;;  define position object

(define n 4)

(define (pos x y)
  (cons x y))
(define (xn p)
  (car p))

(define (yn p)
  (cdr p))
;;;  
		  
(define (queen k)
  (cond ((= k 0)  '())
        ((= k 1) (extend-path '()))
		(else 
		  (filter safe?
		         (flatmap (lambda (path)				
					        (extend-path path))
			              (queen (- k 1)))))))


(define (extend-path path)
  (let ((k (length path)))
    (map 
       (lambda(i)
        (cons (pos k i) path))
       (enumerate-interval 0 n))))
	   
(define (safe? path)
  (not (check? (car path) (cdr path))))
  
	   
	   
(define (check? x path)
  (if (null? path) 
      false
      (if (chk x (car path))
          true
          (check? x (cdr path)))))


(define (chk x y)
   (cond ((= (xn x) (xn y)) true)
         ((= (yn x) (yn y)) true)
         ((diag x y) true)
         (else false)))

(define (diag x y)
  (cond ((< (xn x ) (xn y))  false)
        ((and (= (xn x) (xn y))
              (= (yn x) (yn y))) true)
        (else (or (diag  (pos (- (xn x) 1) (- (yn x) 1)) y)
				  (diag  (pos (- (xn x) 1) (+ (yn x) 1)) y))
					)))			  
  	   

	   
(define x (pos 0 0))
(define y (pos 1 1))		  
