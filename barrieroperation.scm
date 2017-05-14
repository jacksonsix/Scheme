;; vertical seperation  plus  horizontal seperation
;; multiple representation for abstract data

;; orgnize procedures in a set, table style
;;  generic operations/type   table
;;  op1     type1 type2 
;;   
;;
;; complex number
;; generic operations

;; revision add neg operation

;;; add type tag to data 
(define (add-type tag data)
  (if (equal? tag 'number)
      data
      (cons tag data)))
  
(define (data tagged)
  (if (pair? tagged)
      (cdr tagged)
	  (if (number? tagged)
	      tagged
	      (error "not supported"))))

	  
; number	  
(define (type tagged)
  (if (pair? tagged)
      (car tagged)
	  (if (number? tagged)
	      'number
		  (error "not supported")))) 

;; table of procedures
;; need key/value pair as basic structures
;;; (list key/value1  key/value2)
;; table and frame as container, sometimes needs to modify the container,
;; so use pair (car to data, cdr to link) as container, modify its car data member


(define (make-key-value key value)
  (list key value))

(define (key pair)
  (car pair))

(define (value pair)
  (cadr pair))
  
;; frame and its operations
(define (make-frame key)
  (cons key '()))
  
(define (add-to-frame kv frame)
  (set-cdr! frame (cons kv (cdr frame))))
  
(define (find ke frame)  
 (define (f k fram)
  (cond ((null?  fram) '())
        ((equal? k (key (car fram))) (value (car fram)))
        (else (f k (cdr fram)))))
  (f  ke (cdr frame)))		
  
 
  

(define (add-frame-to-table framename frame table)
  ;(set-cdr! proc-table (cons (make-key-value framename frame) (cdr table)))) 
  (let ((kv (make-key-value framename frame)))
      (add-to-frame kv table)))
           

(define (findframe framename table)
  (find framename table))
  
;; set of frames ,  { key/frame }    

(define (listframe)
  (define (lists tab)
    (if (null?  tab) 
        (display 'done)
	    (begin (display (key (car tab)))
             (newline)
             (lists (cdr tab)))))
  (lists (cdr proc-table)))	  

(define proc-table (cons 'table '()))


(define (put opkey typekey proc)
  (let ((frame (findframe opkey proc-table)))
    (if (null? frame)
	    (let ((nframe (make-frame opkey)))
		  (add-to-frame (make-key-value typekey proc) nframe)
		  (add-frame-to-table opkey nframe proc-table))
		(add-to-frame (make-key-value typekey proc) frame))))  
  
(define (get opkey typekey)
  ;(display '(get once))
  ;(display opkey)
  ;(display typekey)
  ;(newline)
  (let ((frame (findframe opkey proc-table)))
    (if (null? frame)
	    '()
		(find typekey frame))))
		
;; end of table operations		
 
    
;; data-dircted programming--------------------------------------------------------------------------------
;; install package


(define (install-number-package)
  (define (tag x) (add-type 'number x))
  (define (add x y) (+ x y))
  (define (sub x y) (- x y))
  (define (mul x y) (* x y))
  (define (div x y) (/ x y))
  
  (put 'add '(number number) (lambda(x y) (tag (add x y))))
  (put 'sub '(number number) (lambda(x y) (tag (sub x y))))
  (put 'mul '(number number) (lambda(x y) (tag (mul x y))))
  (put 'div '(number number) (lambda(x y) (tag (div x y))))
  (put 'make 'number (lambda(x) (tag x)))
  (put 'equ? '(number number) (lambda (x y)(= x y)))
  (put '=zero? '(number) (lambda(x) (= x 0)))
  (put 'neg '(number) (lambda(x) (- x)))
  (put 'gcd '(number number) (lambda(x y) (gcd x y)))
  'done)
  
  
  
(define (install-rational-package) 
   (define (tag x) (add-type 'rational x))
   (define (numer x) (car x))
   (define (denom x) (cdr x))
   
   (define (make-rational n d) 
      (let ((g (ggcd n d)))
	      (cons (div n g) (div d g))))
		  
	   
   (define (add-rational x y)
    (make-rational (add (mul (numer x) (denom y))
                        (mul (numer y) (denom x)))
                   (mul (denom x) (denom y))))
				   
   (define (sub-rational x y)
    (make-rational (sub (mul (numer x) (denom y))
                        (mul (numer y) (denom x)))
                   (mul (denom x) (denom y))))

   (define (mul-rational x y)
     (make-rational (mul (numer x)  (numer y))
                    (mul (denom x)  (denom y))))
					
   (define (div-rational x y)
     (make-rational (mul (numer x)  (denom y))
                    (mul (denom x)  (numer y))))

	(put 'add '(rational rational) (lambda(x y) (tag (add-rational x y))))
	(put 'sub '(rational rational) (lambda(x y) (tag (sub-rational x y))))
	(put 'mul '(rational rational) (lambda(x y) (tag (mul-rational x y))))
	(put 'div '(rational rational) (lambda(x y) (tag (div-rational x y))))
	
	(put 'equ? '(rational rational) (lambda(x y) (equ? (div (numer x) (denom x))
	                                                (div (numer y) (denom y)))))
	(put '=zero? '(rational) (lambda(x)(=zero?  (numer x))))
	
	(put 'make 'rational (lambda(n d) (tag (make-rational n d))))
	(put 'numer '(rational) (lambda(x) (numer  x)))
	(put 'denom '(rational) (lambda(x) (denom  x)))
	(put 'neg '(rational) (lambda(x) (make-rational (neg (numer x))
	                                                (denom x))))
	'done)
	
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (numer x)
  (applygenirc 'numer x))
  
;((get 'rnumer '(rational)) x)) ; parameter must strip off type information  
  
(define (denom x)
  (applygenirc 'denom x)) 
  
(define (ggcd x y)
  (applygenirc 'gcd x y))  
 
;;;;;complex package --------------

(define (install-rect-package)
  (define (real-part z) (car z))
  (define (img-part z) (cdr z))
  (define (make-from-real-img r img) (cons r img))
  (define (mag z) (sqrt (add (square (real-part z))
                     (square (img-part z)))))
  (define (angle z) (atan (img-part z) (real-part z)))
  (define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))
  
  (define (tagx data)
    (add-type 'rect data))
  (put 'real-part '(rect) real-part)
  (put 'img-part '(rect)  img-part)
  (put 'mag '(rect) mag)
  (put 'angle '(rect) angle)
  (put 'make-from-real-img 'rect (lambda (x y) (tagx (make-from-real-img x y))))
  (put 'make-from-mag-ang 'rect (lambda (x y) (tagx (make-from-mag-ang x y))))
  'done)
  
  
  
(define (install-polar-package)
  (define (real-part z) (* (mag z) (cos (angle z))))
  (define (img-part z) (* (mag z) (sin (angle z))))
  (define (make-from-real-img r img) (cons (sqrt (add (square r) (square img)))  
                                           (atan img r)))
  (define (mag z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  
  (define (tagx data)
    (add-type 'polar data))
  (put 'real-part '(polar) real-part)
  (put 'img-part '(polar)  img-part)
  (put 'mag '(polar) mag)
  (put 'angle '(polar) angle)
  (put 'make-from-real-img 'polar (lambda (x y) (tagx (make-from-real-img x y))))
  (put 'make-from-mag-ang 'polar (lambda (x y) (tagx (make-from-mag-ang x y))))
  'done)
  
  
 
 
(define (install-complex-package)
   (define (tag x) (add-type 'complex x))
   (define (make-from-mag-ang x y)
      ((get 'make-from-mag-ang 'polar) x y))
   (define (make-from-real-img x y)
     ((get 'make-from-real-img 'rect) x y))
	 
   (define (add-complex x y)
     (make-from-real-img (add (real-part x) (real-part y))
                         (add (img-part x) (img-part y))))

   ;(define (sub-complex x y)
   ; (make-from-real-img (sub (real-part x) (real-part y))
   ;                      (sub (img-part x) (img-part y))))
   (define (sub-complex x y)
     (make-from-real-img (add (real-part x) (neg (real-part y)))
	                     (add (img-part x)  (neg (img-part y)))))
   
   (define (mul-complex x y)
     (make-from-real-img (mul (mag x) (mag y))
                         (add (angle x) (angle y))))
   
   (define (div-complex x y)
     (make-from-real-img (div (mag x) (mag y))
                         (sub (angle x) (angle y))))
   (define (neg-complex x)
      (make-from-real-img (neg (real-part x))
                          (img-part x)))	  
   					
   (put 'add '(complex complex) (lambda(x y) (tag (add-complex x y))))			   
   (put 'sub '(complex complex) (lambda(x y) (tag (sub-complex x y))))	
   (put 'mul '(complex complex) (lambda(x y) (tag (mul-complex x y))))	
   (put 'div '(complex complex) (lambda(x y) (tag (div-complex x y))))
   (put 'make-from-real-img 'complex (lambda (x y) (tag (make-from-real-img x y))))
   (put 'make-from-mag-ang  'complex (lambda (x y) (tag (make-from-mag-ang x y))))
   
   (put 'equ? '(complex complex) (lambda(x y) (and (equ? (real-part x) (real-part y))    ;; what about a rational real part ??
                                                   (equ? (img-part x) (img-part y)))))
												   
   (put '=zero? '(complex) (lambda(x) (and (=zero?  (real-part x))
                                            (=zero?  (img-part x)))))
											
   (put 'neg '(complex) (lambda(x) (tag (neg-complex x))))											
   'done)

(define (make-from-mag-ang x y)
  ((get 'make-from-mag-ang 'complex) x y))

(define (make-from-real-img x y)
  ((get 'make-from-real-img 'complex) x y))  

(define (img-part z)
  (applygenirc 'img-part z))  

(define (real-part z)
  (applygenirc 'real-part z)) 
  
(define (mag z)
  (applygenirc 'mag z))
  
(define (angle z)
  (applygenirc 'angle z))  
  
(install-polar-package)
(install-rect-package)   
(install-number-package)
(install-rational-package)
(install-complex-package)
 

 
(define (add x y)
  (applygenirc 'add x y))  
(define (sub x y)
  (applygenirc 'sub x y))  
(define (mul x y)
  (applygenirc 'mul x y))  
(define (div x y)
  (applygenirc 'div x y))  
  
(define (equ? x y)
  (applygenirc 'equ? x y))  
  
(define (=zero? x)
  (applygenirc '=zero? x))

(define (neg x)
   (applygenirc 'neg x))
   
;; reduce-package --------------------------------------------------

;; use tower representation of number,rational,complex,poly


(define (order x)
(display 'order)
(display '-)	
(display x)
(newline)
   (cond ((equal? (type x) 'number) 1)
         ((equal? (type x) 'rational) 2)
         ((equal? (type x) 'complex) 3)
		 ((equal? (type x) 'poly) 4)
         (else error"unkown order")))



(define (install-reduce-package)  
  (put 'project '(number) (lambda(x) x))
  (put 'project '(rational) (lambda(x) (numer x)))
  (put 'project '(complex) (lambda(x) (real-part x)))
  ; this project operation should in poly package??
  ;(put 'project '(poly) (lambda(x) (coff (first-term (terms x)))))
  
  'done)
  
(install-reduce-package)  



(define (project x)
   (if (= (order x) 1)
    x
   (applygenirc 'project x)))  
   
(define (lower x)
  (let ((t (project x)))
    (let ((r (rise t)))
      ;(if (equal? 'poly (type r))
      ;    (if (equ? x (make-poly (variable x) (terms r)))
      ;        t
      ;        x)			
          (if (equ? x r)
	       t
		   x))))    
 
(define (drop x)  
  (if (= (order x) 1)
    x
    (let ((w (lower x)))
     (if (= (order x) (order w))
	    w
		(lower w)))))
        
  
 
	
;; this can also install to table instead of case 	
(define (rise x)
  (cond ((equal? (type x) 'number) (make-rational x 1))
        ((equal? (type x) 'rational) (make-from-real-img x 0))
        ((equal? (type x) 'complex) (make-poly 'x (make-terms-sparse (list (make-term 0 x)))))
        (else error"unkown type")))


;(define (number->rational x)  (raise x))
;(define (number->complex x) (raise (raise x)))  
;(define (rational->complex x) (raise x))

(define (raise n x)
  (if (= 0 n)
      x
	  (raise (- n 1) (rise x))))
	  

(define (maxlist list)
  (define (f list n)
   (if (null? list)
       n
       (if (> (car list) n)
        (f (cdr list) (car list))
        (f (cdr list) n))))
  (f list 1))		
	  
(define (raiseargs args)
  (display args)
  (define (high args)  
    (let ((orders (map order args)))
	   (maxlist orders )))
  (define (r args n)   
    (if (null? args)
      '()
      (cons (raise (- n (order (car args))) (car args))
            (r (cdr args) n))))
  (r args (high args)))
  



; result of applygenirc may be not number, such as true/false, can not drop  
(define (applygenirc op . args) 
 (define (try op . ags)
  (let ((type-tags (map type args)))
   (let ((proc (get op type-tags)))
     (if (not (null? proc)) 
	     (apply proc (map data args))
		 (let ((aproc  (get op (map type (raiseargs args)))))
		    (if (not (null? aproc))
			     (apply aproc (map data (raiseargs args)))
				(error "can not find procedures")))))))
 (let ((result (try op args)))         ;;reduce the result before return
    ;(if (pair? result)
    ;    (if (or (equal? (type result) 'number)	
	;	        (equal? (type result) 'rational)
    ;            (equal? (type result) 'complex))
    ;        (drop result)
    ;        result)
        result))			


; the problem is how to handle the  args and (list args).
; apply can handle this. how ?

 	


;;  polynomial package -------------------------------------------------
;; high dense package
;; dense terms (high low 0)

(define (install-dense-package)
  (define (tagx x) (add-type 'dense x))
  (define (make-terms l) l)
  
  (define empty-term-token '())
  (define (empty-terms? terms) (null? terms))
  (define (first-term terms) (car terms))
  (define (rest-terms terms) (cdr terms))  
  
  (define (add-terms terms1 terms2)
    (define (addt t1 t2)
      (cond ((empty-terms? t1) t2)
	        ((empty-terms? t2) t1)
		    (else 				
			  (cons (add (first-term t1)
                         (first-term t2))
                    (addt (cdr t1) (cdr t2))))))					

    (let ((r1 (reverse terms1))
		  (r2 (reverse terms2)))
        (reverse (addt r1 r2))))	
		
        	     
  
  (put 'add '(dense dense) (lambda(t1 t2)	(tagx (add-terms t1 t2))))
  (put 'make 'dense (lambda(x) (tagx (make-terms x))))
  
  
'done)





;; low dense package
(define (install-low-dense-package)
 ;;  terms operations
  (define (tagx data) (add-type 'sparse data))
  
;; when add-terms , be sure to have a desceding order of terms !!!  
  (define (add-terms terms1 terms2)    
    (cond ((empty-terms? terms1) terms2)
	      ((empty-terms? terms2) terms1)
		  (else  
		     (let ((t1 (first-term terms1))
			       (t2 (first-term terms2)))
				(cond ((> (termorder t1) (termorder t2)) (adjoin-term t1 (add-terms (rest-terms terms1) terms2)))
                      ((< (termorder t1) (termorder t2)) (adjoin-term t2 (add-terms terms1 (rest-terms terms2))))				
    		          (else (adjoin-term (make-term (termorder t1) (add (coff t1) (coff t2)))
					                     (add-terms (rest-terms terms1) (rest-terms terms2)))))))))
					  
		  
  (define (sub-terms terms1 terms2)   
    (add-terms terms1 (neg-terms terms2)))
					  
  (define (neg-terms  terms)   
    (cond ((empty-terms? terms) empty-term-token)  
          (else (map (lambda(term)(make-term (termorder term) (neg (coff term))))
              		  terms))))		  
		  
   		  
  (define (mul-terms terms1 terms2)
    (cond ((empty-terms? terms1) empty-term-token)
	      ;((null? terms2) empty-term-token)
		  (else  
		       (add-terms (mul-terms (rest-terms terms1) terms2)
			              (mul-term  (first-term terms1) terms2)))))
					  
  (define (mul-term x terms) 
    (if (null? terms) 
	    '() 
        (adjoin-term  (make-term (+ (termorder x) (termorder (first-term terms)))
                                 (mul (coff x) (coff (first-term terms))))
    				  (mul-term x (rest-terms terms)))))
							
  (define (div-terms terms1 terms2)   
    (display terms1)
	(display '--)
	(display terms2)
	(newline)
    (cond ((null? terms1) (list empty-term-token empty-term-token))
	      ((<  (termorder (first-term terms1)) (termorder (first-term terms2))) (list empty-term-token terms1))
		  (else  
		       (let ((new-o  (- (termorder (first-term terms1)) (termorder (first-term terms2))))
			         (new-cof (div (coff (first-term terms1)) (coff (first-term terms2)))))                				 
                        (let ((rest-of-result
				                 (div-terms (sub-terms terms1
					                                  (mul-terms (list (make-term new-o new-cof))
                                                                  terms2))
										     terms2)))
					      (list   (add-terms (list (make-term new-o new-cof)) (car rest-of-result))
					              (cadr rest-of-result)))))))						 
											 

  (define (remainder-terms a b)
    (cadr (div-terms a b)))
	
  (define (quot a b)
    (car (div-terms a b)))  
											 
  (define (gcd-terms a b)
   
    (define (factor a b)
	  (let ((o1 (termorder (first-term a)))
	        (o2 (termorder (first-term b)))
			(c (coff (first-term b))))
		 (expt c (+ 1 (- o1 o2)))))
	(define (bgcd-terms a b)	 
       (if (empty-terms? b)
	    a
		(bgcd-terms b (remainder-terms (mul-terms a  (list (make-term 0 (factor a b))))
                               	  b))))
    (let ((bg (bgcd-terms a b))) 								  
	  (map (lambda(term) (make-term (termorder term) (div (coff term) (apply gcd (map (lambda(t) (coff t))
                                                                                      bg)))))
         bg)))																			
	      
  
  ;; terms object {term}
  (define empty-term-token '())
  (define (empty-terms? terms) (null? terms))
  (define (make-terms terms)  terms)
  (define (first-term terms) (car terms))
  (define (rest-terms terms) (cdr terms))
  (define (last-term terms) 
     (if (null? terms)
	     '()
		 (if (null? (cdr terms))
		     (begin (newline) (display 'last) (display (car terms)) (car terms))
			 (last-term (cdr terms)))))
			 
  (define (adjoin-term x terms) 
    (if (=zero? (coff x))
	    terms 
	    (cons x terms)))

		
  (define (len terms)
    (if (null? terms)
        0
        (+ 1 (len (cdr terms)))))
  
  (define (eqterms? t1 t2)
    (cond ((and (null? t1) (null? t2)) true)
          (else (if (eqterm? (first-term t1) (first-term t2))
                    (eqterms? (rest-terms t1) (rest-terms t2))
                    false))))
					
  (define (eqterm? t1 t2)
    (and (= (termorder t1) (termorder t2))
         (equ? (coff t1) (coff t2))))  	
  
  ;; term object
  (define (make-term order cof) (cons order cof))
  (define (termorder t) (car t))
  (define (coff t) (cdr t))
  
  ;(define (all-zero? l)
   ;  (if (null? l)
	;     true
	;	 (if (=zero? (car l))
	;	     (all-zero? (cdr l))
	;		 false)))
			 

  (put 'make 'sparse (lambda(x) (tagx (make-terms x))))
  (put 'add '(sparse sparse) (lambda(t1s t2s) (tagx (add-terms t1s t2s))))	
  (put 'sub '(sparse sparse) (lambda(t1s t2s) (tagx (sub-terms t1s t2s))))	
  (put 'mul '(sparse sparse) (lambda(t1s t2s) (tagx (mul-terms t1s t2s))))	
  (put 'div '(sparse sparse) (lambda(t1s t2s) (tagx (quot t1s t2s))))
  (put 'neg '(sparse) (lambda(terms) (tagx (neg-terms terms))))  
  (put 'gcd '(sparse sparse) (lambda(t1s t2s) (tagx (gcd-terms t1s t2s))))
  (put 'make 'term make-term)	
  (put '=zero? '(sparse) (lambda(terms) (empty-terms? terms)))
  (put 'equ? '(sparse sparse) (lambda(x y) (eqterms? x y)))
  (put 'project 'sparse (lambda(terms) (coff (first-term terms))))
  
'done)

(install-low-dense-package)
(install-dense-package)
 
(define (add-terms terms1 terms2)
   (applygenirc 'add terms1 terms2))
   
(define (sub-terms terms1 terms2)
   (applygenirc 'sub terms1 terms2))
   
(define (mul-terms terms1 terms2)
   (applygenirc 'mul terms1 terms2))
   
(define (div-terms terms1 terms2)
   (applygenirc 'div terms1 terms2))   
   
(define (gcd-terms terms1 terms2)
   (applygenirc 'gcd terms1 terms2))     

(define (neg-terms terms )
   (applygenirc 'neg terms)) 
   
(define (project-sparse terms)
  ((get 'project 'sparse) terms))   
 
;; poly package -------------------------
;; make an ordering of variables, so it works for multiple variables
;; This is to make multidimension of operation on variable, one variable one dimension
;;


(define (install-poly-package)
  (define (tagx x) (add-type 'poly x))
  
		  
  (define (make-poly var terms)        
    	(cons var terms))		 
		   
  (define (var po) (car po))
  (define (terms po) (cdr po))
  
  (define (add-poly x y)
    (if (equal? (var x) (var y))
	    (make-poly (var x)
		           (add-terms (terms x) (terms y))) 
		(error "can not add poly")))
		
  (define (sub-poly x y)
    (if (equal? (var x) (var y))
	    (make-poly (var x)
		           (sub-terms (terms x) (terms y))) 
		(error "can not sub poly")))	

  (define (neg-poly x )   
	    (make-poly (var x)
		           (neg-terms (terms x) ))) 
				
		
  (define (mul-poly x y)
    (if (equal? (var x) (var y))
	    (make-poly (var x)
		           (mul-terms (terms x) (terms y))) 
		(error "can not mul ploy")))
		
  (define (div-poly x y)
    (if (equal? (var x) (var y))
	    (make-poly (var x)
		           (div-terms (terms x) (terms y))) 
		(error "can not div ploy")))
		
  (define (gcd-poly x y)
    (if (equal? (var x) (var y))
	    (make-poly (var x)
		           (gcd-terms (terms x) (terms y))) 
		(error "can not gcd ploy")))
 	
  (put 'make 'poly (lambda(v t) (tagx (make-poly v t))))
  (put 'variable '(poly) var )
  (put 'terms '(poly) terms )
  
  (put 'add '(poly poly) (lambda (x y) (tagx (add-poly x y))))
  (put 'sub '(poly poly) (lambda (x y) (tagx (sub-poly x y))))
  (put 'mul '(poly poly) (lambda (x y) (tagx (mul-poly x y))))
  (put 'div '(poly poly) (lambda (x y) (tagx (div-poly x y))))
  (put '=zero? '(poly) (lambda(x) (=zero? (terms x))))
  (put 'neg '(poly) (lambda(x) (tagx (neg-poly x))))
  (put 'equ? '(poly poly) (lambda (x y) (and (equal? (var x) (var y))
                                             (equ? (terms x) (terms y)))))
											 
  (put 'project '(poly) (lambda(x) (project-sparse (terms x))))
  (put 'gcd '(poly poly) (lambda(x y) (tagx (gcd-poly x y))))
  
  'done)  
  
(define (make-poly variable terms)
  ((get 'make 'poly) variable terms))
  

(define (make-terms-sparse term)
  ((get 'make 'sparse) term))


(define (make-terms-dense term)
  ((get 'make 'dense) term))  
  
(define (variable p) 
  (applygenirc 'variable p))
  
(define (terms p) 
  (applygenirc 'terms  p))  

  
(define (make-term o c)
  ((get 'make 'term) o c))
   
   
(define (=zero? py)
  (applygenirc '=zero? py)) 

;; bury ploy on 'y into coff
;; extend coff if necessary
  
  
(define (put-multi-var vars)
  (put 'multi 'var vars)) 
  
(define (get-multi-var)
  (get 'multi 'var ))   
  
  
  
(install-poly-package)




