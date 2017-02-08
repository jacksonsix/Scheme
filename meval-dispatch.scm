;; my simple draft of evaluator
;; underlying evaluator is working , when input (* 1 2). it eval before this procedure. 

(define (eval exp env) 
;; 2,3 stage of matching, because the is an order of the branches
;(let ((matched 'false))
 (cond  ((self-value? exp)   exp)
        ((variable? exp)   (lookup-variable-value exp env))		
        ((inlist (get-type exp)) ((get (get-type exp)) exp env)) 
		((pair? exp)   (applyx (eval (operator exp) env)  
		                                                (list-of-values (operand exp) env)))
		(else  (error "unknown type" exp))))
		
;;
(define (applyx procedure arguments)
 (cond ((primitive-procedure? procedure) (apply-primitive-procedure procedure arguments))
       ((compound-procedure? procedure)  (eval-sequence (procedure-body procedure)
	                                                    (extend-environment (procedure-parameters procedure)
														                    arguments
																			(procedure-environment procedure))))
		(else (error "unknow apply") procedure arguments)))																	
 
 
(define primitive-procedures 
	(list
		(list 'car car)
		(list 'cdr cdr)
		(list 'cons cons)
		(list 'null? null?)	
		(list '* *)
		(list '+ +)
		))
			
(define (primitive-procedure-names)
  (map car primitive-procedures))
  
(define (primitive-procedure-objects)
  (map (lambda (item) 
           (list 'primitive (cadr item)))  
       primitive-procedures ))

  
			
(define (find name sets)
  (cond ((null? sets) false)
        ((eq? (caar sets) name) (cdar sets))
        (else (find name (cdr sets)))))		
			
(define (eval-lambda exp env)
  (make-procedure (lambda-para exp) (lambda-body exp) env))
	
(define (make-procedure para body env)
  (list 'procedure para body env))
  
(define (procedure-parameters p)
  (cadr p))

(define (procedure-body p)
  (caddr p))  

(define (procedure-environment p)
  (cadddr p))
			
(define (primitive-procedure? procedure) 
  (tagged-list? procedure 'primitive))
  
	  
(define (apply-primitive-procedure procedure arguments)
  (apply-in-underlying-scheme (primitive-implementation procedure) arguments))	  
 
(define (primitive-implementation proc) (cadr proc)) 
(define apply-in-underlying-scheme apply)
 
 
(define (compound-procedure? procedure)
   (tagged-list? procedure 'procedure))
 
 
(define (list-of-values exps env)
  (if (no-operands? exps)
	  '()
	  (cons (eval (first-operand exps) env)
       	    (list-of-values (rest-operand exps) env))))
 
;; 
 
(define (self-value? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
		(else false)))

;; represent expression  
;; need tagged list to represent compound data  

(define (tagged-list? exp type)
 (if (pair? exp)
     (eq? (car exp) type)
	 false))
	 

(define (quoted? exp)
  (tagged-list? exp 'quote))
  
(define (get-quote exp env)
  (cadr exp))

(define (variable? exp)
  (symbol? exp))

;; need lookup , depend on environment. Tbd

(define (assignment? exp)
  (tagged-list? exp 'set!))
  
(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

  
;; define var , define procedure
 
(define (definition? exp)
  (tagged-list? exp 'define))
  
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))
					
					
;; lambda expression
(define (lambda? exp)
  (tagged-list? exp 'lambda))
  
(define (lambda-para exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))

(define (make-lambda para body)
  (cons 'lambda (cons para body)))
 ;; (list 'lambda para body))


  
(define (if-stmt? exp)
  (tagged-list? exp 'if))

(define (if-pred exp)
  (cadr exp))
  
(define (if-consequenct exp)
  (caddr exp))

(define (if-alt exp)
  (if (not (null? (caddr exp)))
      (cadddr exp)
      'false))
	  
(define (make-if pred consequenct alt)
  (list 'if pred consequenct alt))

(define (begin-stmt? exp)
  (tagged-list? exp 'begin))

(define (make-begin exps)
  (list 'begin exps))

(define (begin-actions exp)
  (cdr exp))
  
(define (first-exp sequence)
  (car sequence))  

(define (last-exp? sequence)
  (null? (cdr sequence)))

(define (rest-sequence sequence)
  (cdr sequence))

(define (sequence->exp sequence)
  (cond ((null? sequence) sequence)
		((last-exp? sequence) (first-exp sequence))
		(else (make-begin sequence))))  
		
		

		
;; procedure? is the very last cond, it is not possible to enum all procedure name		
(define (procedure? exp)
  (pair? exp))

(define (operator exp)
  (car exp))

(define (operand exp)
  (cdr exp))

(define (no-operands? ops)
  (null? ops))
(define (first-operand ops)
  (car ops))
(define (last-operand? ops)
  (null? (cdr ops)))

(define (rest-operand ops)
  (cdr ops))

 
;; cond expression
(define (cond? exp)
  (tagged-list? exp 'cond))

(define (cond-clauses exp)
  (cdr exp))

(define (cond-else-clause? exp)
  (eq? (cond-pred exp) 'else))

(define (cond-actions clauses)
  (cdr clauses))
  
;; this the place to make the specail form  =>     
(define (cond->if exp)
  (iter-expand-clauses (cond-clauses exp)))  
  
;;; pure sequence into nested forms

(define (iter-expand-clauses clauses)
  (if (null? clauses)
      'false
	  (let ((first (car clauses))
	        (rest (cdr clauses)))
			
	  (if   (cond-else-clause? first) 
	        (if (null? rest)
			    (sequence->exp (cond-actions first))
				(error "Else clause is not the last" clauses))
			(make-if (cond-pred first)
			;; specail form =>
			               (if (specail? first)
						       (make-procedure  (specail-pred first) ((specail-proc first) (specail-pred first)) env)
						       (sequence->exp (cond-actions first)))
						   (iter-expand-clauses rest))))))
					   
  
  
(define (eval-if exp env)
 (if (true? (eval (if-pred exp) env))
     (eval (if-consequenct exp) env)
	 (eval (if-alt exp) env)))
	 

(define (eval-sequence exps env) 
  (cond ((last-sequence? exps) (eval (first-sequence exps) env))
        (else (eval (first-sequence exps) env)
			  (eval-sequence (rest-sequence exps) env))))
			  

(define (last-sequence? exps)
  (null? (cdr exps)))
  
(define (first-sequence exps)
  (car exps))
(define (rest-sequence exps)
  (cdr exps))  

			  
(define (eval-assignment exp env)
  (let ((variable (assignment-variable exp))
        (value (assignment-value exp)))
    (set-variable-value! variable 
	                    (eval value env)
						env))
   'ok)
   
   
(define (eval-definition exp env)
  (define-variable!  (definition-variable exp)
                     (eval (definition-value exp) env)
                     env)
  'ok)

  ;;((and-stmt? exp) (eval-and-stmt exp env))
(define (make-and-stmt exps)
  (list 'and-stmt exps))

  
(define (and-stmt? exp)
  (tagged-list? exp 'and-stmt))

(define (eval-and-stmt exp env) 
	(define (eval-terms terms)
	  (cond ((null? terms) true)
			((not (eval (car terms) env) false))	 
			(else (eval-terms (cdr terms) env))))
    (eval-terms (cdr exp)))
	
;;
(define (make-or-stmt exps)
  (list 'or-stmt exps))

  
(define (or-stmt? exp)
  (tagged-list? exp 'or-stmt))

(define (eval-or-stmt exp env) 
	(define (eval-terms terms)
	  (cond ((null? terms) false)
			((true? (eval (car terms) env) true))	 
			(else (eval-terms (cdr terms) env))))
    (eval-terms (cdr exp)))	
	
	
;; =>  (list 'specail pred proc)
;;  
;;  (eq? (car (cond-actions first)) '=>))
(define (specail? exp)
  (tagged-list? exp 'specail))

(define (make-specail pred proc)
  (list 'specail pred proc))

(define (specail-pred exp)
  (cadr exp))
  
(define (specail-proc exp)
  (caddr exp))
  
  
;;(define (let->combination exp)

;; in small evalator , it has to eval true/false by explicit call underlying eq?
(define (true? x)
  (not (eq? x false)))
  
(define (false? x)
  (eq? x false))  


;; environment define
;; env -> frame+
;; frame -> pair+
;; pair -> (list 'name value)
  
;;(lookup-variable-value var env)
;;(extend-environment var value envbase)
;;(define-variable! var value env)
;;(set-variable-value! var value env)
;; for the purpose of these operations, represent environment as list of frames, enclosing environment is the cdr of list

(define (enclosing-environment env)
  (cdr env))
  
(define (first-frame env)
  (car env))
  
(define the-empty-environment '())

;; for frame
(define (make-frame vars values)
  (cons vars values))

(define (frame-variables frame)
  (car frame))
(define (frame-values frame)
  (cdr frame))

(define (add-binding-to-frame var value frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons value (cdr frame))))

(define (extend-environment vars values envbase)
  (if (=  (length vars) (length values))
      (cons (make-frame vars values) envbase) 
      (error "variable number is not equal to value number" vars values)))

;;(define (lookup-variable-value var env)
;;  (define (env-loop env)
;;    (define (scan vars values)
;;	  (cond ((null? vars) (env-loop (enclosing-environment env)))
;;	        ((eq? var (car vars)) (car values))
;;	        (else (scan (cdr vars) (cdr values)))))
;;    (if (eq? env the-empty-environment)
;;		(error "Unbound variable"  var env)
;;		(let ((frame (first-frame env)))
;;		   (scan (frame-variables frame) (frame-values frame)))))
;;  (env-loop env))  	


(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
  

(define (set-variable-value! var value env)
  (define (env-loop env)
    (define (scan vars values)
	  (cond ((null? vars) (env-loop (enclosing-environment env)))
	        ((eq? var (car vars)) (set-car! values value))
	        (else (scan (cdr vars) (cdr values)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" env)
    	(let ((frame (first-frame env)))
            (scan (frame-variables frame) (frame-values frame)))))
   (env-loop env))

(define (define-variable! var value env)
  (let ((frame (first-frame env)))
    (define (scan vars values)
     (cond ((null? vars) (add-binding-to-frame var value frame))
           ((eq? var (car vars)) (set-car! values value))
		   (else (scan (cdr vars) (cdr values)))))
    (scan (frame-variables frame) (frame-values frame))))		   
            		   
  
(define the-empty-environment '())		

(define (setup-env)
  (let ((initial-env 
         (extend-environment (primitive-procedure-names)
		                     (primitive-procedure-objects)
							 the-empty-environment)))
    (define-variable! 'true true initial-env)
	(define-variable! 'false false initial-env)
	initial-env))
	
;;(define env0 (setup-env))	
(define (driver)
  (display "Please input expression")
  (newline)
  (let ((input (read)))
    (display "input expression is : ")
	(display input)
	(newline)
    (let ((result (eval input env0)))	
     (newline)	 
	 (display "eval expression is : ")
     (user-print result)
	 (newline)))
  (driver))
 
 
  
(define (user-print object)
  (if (compound-procedure? object)
    (display (list 'compound-procedure
					(procedure-parameters object)
                    (procedure-body object)
                    '<procedure-env>))
    (display object)))
	
  
	;; debug functions

;; dict has 2 list names, values
;; dict =  (cons names values)	
(define namelist  (cons 'names '()))
(define valuelist (cons 'values '()))			
(define dict (cons namelist valuelist))

(define (extend-dict funcname)
  (define (scan names values)
    (cond ((null? names) (set-cdr! namelist (cons funcname  (cdr namelist))) (set-cdr! valuelist (cons 1  (cdr valuelist))))
          ((eq? funcname (car names)) (set-car!  values (+ (car values) 1)))
          (else (scan (cdr names) (cdr values)))))
  (scan (car dict) (cdr dict)))
  
	
(define (clear-dict)
  (set-cdr! namelist '())
  (set-cdr! valuelist '()))  
	  
	  
;; get type of expression
(define (get-type exp)
  (cond ((eq? 'primitive (car exp)) 'procedure)
        ((eq? 'procedure (car exp)) 'procedure)
		(else (car exp))))
        
		 
;;; put, get procedure in a dict

(define funcdict (cons '() '()))


(define (make-node name proc)
   (cons name proc))

(define (put funcname procedure) 
    (define (scan node)	
      (cond ((null? node) (set-car! funcdict (cons (make-node funcname procedure) (car funcdict))))
            ((eq? funcname (car (car node)))  (set-cdr! (car node) procedure))
            (else (scan (cdr node)))))
    (scan (car funcdict)))
	
(define (get funcname)
	(define (scan node)	
      (cond ((null? node) (error "unbound  procedure"))
            ((eq? funcname (car (car node)))  (cdr (car node)))
            (else (scan (cdr node)))))
    (scan (car funcdict)))
   
   
(define (eval-cond exp env)
 (eval (cond->if  exp) env))   
   
(define (eval-begin exp env)   
  (eval-sequence (begin-actions exp) env))   
   
(define (inlist type)
  (cond ((eq? 'quote type) true) 
		((eq? 'set! type) true) 
		((eq? 'define type) true) 
		((eq? 'if type) true) 
		((eq? 'lambda type) true) 
		((eq? 'begin  type) true) 
		((eq? 'cond  type) true) 
		(else false)))
   
(define (install)
  (put 'quote get-quote)
  (put 'set! eval-assignment)
  (put 'define eval-definition)
  (put 'if eval-if)
  (put 'lambda eval-lambda)
  (put 'begin eval-begin)
  (put 'cond eval-cond))
 
  
	
	
