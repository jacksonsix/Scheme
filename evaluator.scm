;; my 2nd draft of evaluator
;; underlying evaluator is working , when input (* 1 2). it eval before this procedure. 

(define (eval exp env) 
 (cond  ((self-value? exp)   exp)
        ((variable? exp)   (lookup-variable-value exp env))		
        ((quoted? exp) (quote-text exp))
		((if-stmt? exp) (eval-if exp env))
		((assignment? exp) (eval-assignment exp env))
		((cond? exp) (eval-cond exp env))
        ((definition? exp) (eval-definition exp env))
        ((begin-stmt? exp) (eval-begin exp env))
        ((lambda? exp) (eval-lambda exp env))		
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
 
  
			
(define (eval-lambda exp env)
  (make-procedure (lambda-para exp) (lambda-body exp) env))
  
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
  
(define (compound-procedure? procedure)
   (tagged-list? procedure 'procedure))  
  
	  
(define (apply-primitive-procedure procedure arguments)
  (apply-in-underlying-scheme (primitive-implementation procedure) arguments))	  
 
(define (primitive-implementation proc) (cadr proc)) 

(define apply-in-underlying-scheme apply)
 
 
 
(define (list-of-values exps env)
  (if (no-operands? exps)
	  '()
	  (cons (eval (first-operand exps) env)
       	    (list-of-values (rest-operand exps) env))))
 
 
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
					
					


(define (eval-if exp env)
 (if (true? (eval (if-pred exp) env))
     (eval (if-consequenct exp) env)
	 (eval (if-alt exp) env)))
  
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

(define (eval-cond exp env)
 (eval (cond->if  exp) env))  
 
(define (cond? exp)
  (tagged-list? exp 'cond))

(define (cond-clauses exp)
  (cdr exp))

(define (cond-else-clause? exp)
  (eq? (cond-pred exp) 'else))

(define (cond-actions clauses)
  (cdr clauses))
  

   
(define (eval-begin exp env)   
  (eval-sequence (begin-actions exp) env))  
	 
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
            		   
  
 ;;; prepare environment for run evaluator
;;;----------------------------------------- 
  
(define the-empty-environment '())		


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


(define (setup-env)
  (let ((initial-env 
         (extend-environment (primitive-procedure-names)
		                     (primitive-procedure-objects)
							 the-empty-environment)))
    (define-variable! 'true true initial-env)
	(define-variable! 'false false initial-env)
	initial-env))
	

(define env0 (setup-env))	

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
	
	

  
	
 
  
	
	
