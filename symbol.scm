;;;; define diff operation on abstract object, 
;; sums products 
;; need constructor, selector, predicate 
;; rule:  




(define (diff exp var)
  (cond ((number? exp) 0)
		((variable? exp) (if (same-variable? exp var) 
		                     1
							 0))
		((sum? exp)  (+ (diff (firstadd exp) var)
                        (diff (secondadd exp) var)))
        ((product? exp) (make-dif '+ 
		                          (make-dif '* (firstadd exp) (diff (secondadd exp) var))
                                  (make-dif '* (secondadd exp) (diff (firstadd exp) var))))
		(else error "unknown type")))
						   
						   
						   
        							 
									 
(define (variable? x)
  (symbol? x))									 
									 
(define (same-variable? x y)
  (and (variable? x) 
       (variable? y)	
       (eq? x y)))

(define (sum? exp)
  (and (pair? exp )
       (eq? '+ (op exp))))
  
(define (product? exp)
  (and (pair? exp)
       (eq? '* (op exp))))  
	   
(define (make-dif op first second)
   (cond ((eq? '+ op) (make-sum first second))
         ((eq? '* op) (make-product first second))
         (else error "unknown")))		 
     
 
(define (make-sum first second)
   (cond ((=number? first 0) second)
         ((=number? second 0) first)
         ((and (number? first) (number? second)) (+ first second))
         (else (list '+ first second))))
		 

(define (make-product first second)
   (cond ((=number? first 0) 0)
         ((=number? second 0) 0)
		 ((=number? first 1) second)
         ((=number? second 1) first)
         ((and (number? first) (number? second)) (* first second))
         (else (list '* first second))))

	
(define (=number? exp num)
  (and (number? exp) (= exp num)))
	
(define (op exp)
  (car exp))

(define (firstadd exp)
  (cadr exp))

(define (secondadd exp)
  (caddr exp))

  
;;; set , operations,   element-of-set? ,  adjoin, union, inter-set
;different representations
 
;;set as unordered list
 
(define (element-of-set? element l)
  (cond ((null? l) false)
        ((equal? element (car l)) true)
        (else (element-of-set? element (cdr l)))))

(define (adjoin element l)
  (cond ((null? l) (cons element l))
        (else (if (element-of-set? element l) 
		           l
                  (cons element l)))))

(define (union set1 set2)
  (cond ((null? set1) set2)
        ((null? set2)  set1)
		(else (if (element-of-set? (car set1) set2)
           		  (union (cdr set1) set2)                                    
                  (cons (car set1) (union (cdr set1) set2))))))
				  
(define (inter-set set1 set2)
  (cond ((null? set1) '())
        ((null? set2) '())
        (else (if (element-of-set? (car set1) set2)
                  (cons (car set1) (inter-set (cdr set1) set2))
                  (inter-set (cdr set1) set2)))))

  				  

;; set as ordered set
(define (element-of-set? element l)
  (if (null? l)
      false  
      (if (< element (car l)) 
         false
         (element-of-set? element (cdr l)))))
		 
		 
(define (adjoin element l)
  (if (null? l)
      (list element)
      (if (< element l)
          (cons element l)
          (adjoin element (cdr l)))))

(define (union set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2))  (cons (car set1) (union (cdr set1) (cdr set2))))
        ((< (car set1) (car set2))  (cons (car set1) (union (cdr set1) set2)))
        ((> (car set1) (car set2))  (cons (car set2) (union set1 (cdr set2))))
        (else error "dd")))
		
		
(define (inter-set set1 set2)
  (cond ((null? set1) '())
		((null? set2) '())
		((< (car set1) (car set2)) (inter-set (cdr set1) set2))
        ((= (car set1) (car set2)) (cons (car set1) (inter-set (cdr set1) (cdr set2))))
        ((> (car set1) (car set2)) (inter-set (cdr set2) set1))
        (else error "unknown")))

;; set as binary tree		
;; tree operation

(define (make-tree entry left right)
  (list entry left right))
  
(define (entry tree)
  (car tree))
(define (left tree)
  (cadr tree))

(define (right tree)
  (caddr tree))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))  (element-of-set? x (left set)))
        ((> x (entry set))  (element-of-set? x (right set)))))


(define (adjoin x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
		((< x (entry set)) (make-tree (entry set) (adjoin x (left set)) (right set)))
        ((> x (entry set)) (make-tree (entry set) (left set) (adjoin x (right set))))))

		

;; tree->list1

(define (tree-list1 tree)
  (if (null? tree) 
      '()
       (append (tree-list1 (left tree)) 
	           (cons (entry tree) (tree-list1 (right tree))))))
			   
			   
(define (tree-list2 tree)
  (define (copy-list tree result)
    (if (null? tree)
        result
        (copy-list (left tree)
                   (cons (entry tree) 
				         (copy-list (right tree) result)))))
  (copy-list tree '()))

  
(define (list->tree l)
  (define (l->t l n)
    (cond ((null? l) '())
          ((= 1 n) (make-tree (car l) '() '()))
          (else (make-tree (ref l (floor (/ n 2)))
                            (list->tree (lef l (floor (/ n 2))))
                            (list->tree (rig l (floor (/ n 2))))))))
  (l->t l (length l)))

(define (ref l n)
  (if (= n 0)
      (car l)
      (ref (cdr l) (- n 1))))
	  
(define (lef l n)
  (if (= 0 n)
      '()
       (cons (car l) 
	         (lef (cdr l) (- n 1)))))

(define (rig l n)
  (if (= n 0)
      (cdr l)
      (rig (cdr l) (- n 1))))
	  

	  
						 
                                    				   
			   
(define n (make-tree 7 
                     (make-tree 3 '() '())	
                     (make-tree 9 '() '())))
					 
  
;; 
 
(define (list->tree2 elements)
  (car (partial elements (length elements))))

;; union-tree,  first convert tree to list, union,inter-set as list
;; convert list to tree


(define (union-tree t1 t2)
  (let ((l (union (tree-list1 t1) (tree-list1 t2))))
    (list->tree l)))

	
;; huffman encoding tree , knowledge in the tree.

(define (element-of-set? element l)
  (cond ((null? l) false)
        ((equal? element (car l)) true)
        (else (element-of-set? element (cdr l)))))


(define (encoding message huffman)
  (define (enc symbol huffman)
    (cond  ((null? symbol) '())
	       ((leaf? huffman) '())
	       ((element-of-set? symbol (symbols huffman))
		     (if (element-of-set? symbol (symbols (left-branch huffman)))
			     (cons 0 
				       (enc symbol (left-branch huffman)))
				 (cons 1 
				       (enc symbol (right-branch huffman)))))
            (else error "no encoding")))
  (if (null? message)
       '()	   
       (append (enc (car message) huffman)
                   (encoding (cdr message) huffman))))
		


(define (decode code huff)
  (define (dec code huffman)
    (cond  ((null? code) '())
	       (else (let ((next-branch (if (= 1 (car code))
                                        (right-branch huffman)
                                  	    (left-branch huffman)))) 								 
	               (if  (leaf? next-branch) 
				        (cons   (symbol-leaf next-branch) 
	                            (dec (cdr code) huff))
						(dec (cdr code) next-branch))))))		
	      
   (dec code huff))		   
  		   


;; make-huffman from  pairs (symbol weight)
;; sort pairs first ,  make-huffman from pairs


  
(define (make-hnode left right)
  (list (append (symbols left) (symbols right))
        (+ (weight left) (weight right))
        left
        right))
		
(define (right-branch hf)
  (cadddr hf))
  
(define (left-branch hf)
  (caddr hf))  

		
(define (symbols node)
  (if (leaf? node)
      (list (cadr node))
      (car node)))	  

(define (weight node)
  (if (leaf? node)
      (caddr node)
      (cadr node)))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
 
(define (symbol-leaf leaf)
  (cadr leaf))
 
(define (leaf? node)
  (and (pair? node )
       (equal? 'leaf (car node))))
  
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))
					
  
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
	  (let ((pair (car pairs)))
	       (adjoin-set (make-leaf (car pair) (cdr pair))
		         (make-leaf-set (cdr pairs))))))
				 
				 
		

	  
(define (make-huffman pairs)	 
   (cond ((null? pairs) '())
         ((null? (cdr pairs)) (car pairs))
		 (else (let ((newnode (make-hnode (car pairs) (cadr pairs))))
                 (make-huffman (adjoin-set newnode (cddr pairs)))))))

				 
	
  
(define dic (list (cons 'a 4) (cons 'b 2) (cons 'c 1) (cons 'd 1)))	

(define message1 '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define huf (make-huffman (make-leaf-set dic)))

	 
(define text '(a d a b b c a))	

(define pop-pairs (list (cons 'A 2) (cons 'BOOM 1) (cons 'GET 2) (cons 'JOB 2) (cons 'NA 16) (cons 'SHA 3) (cons 'YIP 9) (cons 'WAH 1)))

(define phuf (make-huffman (make-leaf-set pop-pairs)))

(define ly '(GET A JOB))
	
	
