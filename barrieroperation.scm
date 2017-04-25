;; vertical seperation  plus  horizontal seperation
;; multiple representation for abstract data

;; orgnize procedures in a set, table style
;;  generic operations/type   table
;;  op1     type1 type2 
;;   
;;
;; complex number
;; generic operations

(define (add-complex c1 c2)
  (make-from-real-img (+ (real c1) (real c2))
                     (+ (img c1) (img c2))))					 


(define (multi-complex c1 c2)
  (make-from-mag-ang  (* (mag c1) (mag c2))
                    (+ (ang c1) (ang c2))))

(define (sub-complex c1 c2)
  (make-from-real-img (- (real c1) (real c2))
                     (- (img c1) (img c2))))

(define (div-complex c1 c2)
  (make-from-mag-ang  (/ (mag c1) (mag c2))
                    (- (ang c1) (ang c2))))


;; representation of complex
;; generic selector
(define (real complex)
  (applygenirc 'real-part complex))
		
  
(define (img complex)
  (applygenirc 'img-part complex))
  

(define (mag complex)
  (applygenirc 'mag complex))

(define (ang complex)
   (applygenirc 'angle complex))


(define (make-from-real-img x y)
  ((get 'make-from-real-img 'rect) x y))
  
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))  

  
(define (applygenirc op . args)
 (display 'applygenirc)
 (display op)
 (display (map type args))
 (newline)
 (let ((type-tags (map type args)))
   (let ((proc (get op type-tags)))
     (if proc 
	     (apply proc (map data args))
		 (error "not methods")))))



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

(define (rect? z)
   (eq? (type z) 'rect))
   
(define (polar? z)
   (eq? (type z) 'polar))


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

(define cross-table (cons 'cross '()))

(define (put opkey typekey proc)
  (let ((frame (findframe opkey proc-table)))
    (if (null? frame)
	    (let ((nframe (make-frame opkey)))
		  (add-to-frame (make-key-value typekey proc) nframe)
		  (add-frame-to-table opkey nframe proc-table))
		(add-to-frame (make-key-value typekey proc) frame))))  
  
(define (get opkey typekey)
  (display '(get once))
  (display opkey)
  (display typekey)
  (newline)
  (let ((frame (findframe opkey proc-table)))
    (if (null? frame)
	    '()
		(find typekey frame))))
		
;; end of table operations		
 
    
;; data-dircted programming
;; install package

(define (install-rect-package)
  (define (real-part z) (car z))
  (define (img-part z) (cdr z))
  (define (make-from-real-img r img) (cons r img))
  (define (mag z) (sqrt (+ (square (real-part z))
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
  (define (make-from-real-img r img) (cons (sqrt (+ (square r) (square img)))  
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
  'done)
  
(define (make-number x)
  ((get 'make 'number) x))  
  
  
(define (install-rational-package) 
   (define (tag x) (add-type 'rational x))
   (define (numer x) (car x))
   (define (denom x) (cdr x))
   (define (make-rational n d) 
     (let ((g (gcd n d)))
	   (cons (/ n g) (/ d g))))
	   
   (define (add-rational x y)
    (make-rational (+ (* (numer x) (denom y))
                        (* (numer y) (denom x)))
                   (* (denom x) (denom y))))
				   
   (define (sub-rational x y)
    (make-rational (- (* (numer x) (denom y))
                        (* (numer y) (denom x)))
                   (* (denom x) (denom y))))

   (define (mul-rational x y)
     (make-rational (* (numer x)  (numer y))
                    (* (denom x)  (denom y))))
					
   (define (div-rational x y)
     (make-rational (* (numer x)  (denom y))
                    (* (denom x)  (numer y))))

	(put 'add '(rational rational) (lambda(x y) (tag (add-rational x y))))
	(put 'sub '(rational rational) (lambda(x y) (tag (sub-rational x y))))
	(put 'mul '(rational rational) (lambda(x y) (tag (mul-rational x y))))
	(put 'div '(rational rational) (lambda(x y) (tag (div-rational x y))))
	
	(put 'equ? '(rational rational) (lambda(x y) (= (/ (numer x) (denom x))
	                                                (/ (numer y) (denom y)))))
	(put '=zero? '(rational) (lambda(x)(= 0 (numer x))))
	
	(put 'make 'rational (lambda(n d) (tag (make-rational n d))))
	'done)
	
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
   (define (tag x) (add-type 'complex x))
   (define (make-from-mag-ang x y)
      ((get 'make-from-mag-ang 'polar) x y))
   (define (make-from-real-img x y)
     ((get 'make-from-real-img 'rect) x y))
	 
   (define (add-complex x y)
     (make-from-real-img (add (real-part x) (real-part y))
                         (add (img-part x) (img-part y))))

   (define (sub-complex x y)
     (make-from-real-img (sub (real-part x) (real-part y))
                         (sub (img-part x) (img-part y))))

   (define (mul-complex x y)
     (make-from-real-img (mul (mag x) (mag y))
                         (add (angle x) (angle y))))
   
   (define (div-complex x y)
     (make-from-real-img (div (mag x) (mag y))
                         (sub (angle x) (angle y))))						 
   					
   (put 'add '(complex complex) (lambda(x y) (tag (add-complex x y))))			   
   (put 'sub '(complex complex) (lambda(x y) (tag (sub-complex x y))))	
   (put 'mul '(complex complex) (lambda(x y) (tag (mul-complex x y))))	
   (put 'div '(complex complex) (lambda(x y) (tag (div-complex x y))))
   (put 'make-from-real-img 'complex (lambda (x y) (tag (make-from-real-img x y))))
   (put 'make-from-mag-ang  'complex (lambda (x y) (tag (make-from-mag-ang x y))))
   
   (put 'real-part '(complex) real-part)
   (put 'img-part '(complex) img-part)
   (put 'mag '(complex) mag)
   (put 'angle '(complex) angle)
   (put 'equ? '(complex complex) (lambda(x y) (and (equ? (real-part x) (real-part y))    ;; what about a rational real part ??
                                                   (equ? (img-part x) (img-part y)))))
												   
	(put '=zero? '(complex) (lambda(x) (and (=zero?  (real-part x))
                                            (=zero?  (img-part x)))))	
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


(define (rise x)
  (cond ((equal? (type x) 'number) (make-rational x 1))
        ((equal? (type x) 'rational) (make-from-real-img x 0))
        ((equal? (type x) 'complex) x)
        (else error"unkown type")))

(define (order x)	
   (display x)
   (cond ((equal? (type x) 'number) 1)
         ((equal? (type x) 'rational) 2)
         ((equal? (type x) 'complex) 3)
         (else error"unkown order")))

(define (number->rational x)  (raise x))
(define (number->complex x) (raise (raise x)))  
(define (rational->complex x) (raise x))

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
  (define (high args)
    (display args)
    (let ((orders (map order args)))
	   (maxlist orders )))
  (define (r args n)
    (display args)
    (if (null? args)
      '()
      (cons (raise (- n (order (car args))) (car args))
            (r (cdr args) n))))
  (r args (high args)))
  
		 
(define (applygenirc op . args) 
 (let ((type-tags (map type args)))
   (let ((proc (get op type-tags)))
     (if (not (null? proc)) 
	     (apply proc (map data args))
		 (let ((aproc  (get op (map type (raiseargs args)))))
		    (if (not (null? aproc))
			    (begin (display 'aproc) (display aproc) (apply aproc (map data (raiseargs args))))
				(error "can not find procedures")))))))    ;


; the problem is how to handle the  args and (list args).
; apply can handle this. how ?

 	
	
