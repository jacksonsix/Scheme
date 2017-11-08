           (define (liars)
            (let ((B (amb 1 2 3 4 5))
                  (E (amb 1 2 3 4 5))
                  (J (amb 1 2 3 4 5))
                  (K (amb 1 2 3 4 5))
                  (M (amb 1 2 3 4 5)))
				  
                (require (distinct? (list B E J K M)))				  
				(require (or (and (= K 2) (not (= B 3))) 
				             (and (not (= K 2)) (= B 3))))
				(require (or (and (= E 1) (not (= J 2))) 
				             (and (not (= E 1)) (= J 2))))
				(require (or (and (= J 3) (not (= E 5))) 
				             (and (not (= J 3)) (= E 5))))
				(require (or (and (= K 2) (not (= M 4))) 
				             (and (not (= K 2))  (= M 4))))
				(require (or (and (= M 4) (not (= B 1))) 
				             (and (not (= M 4))  (= B 1))))
								  
                (list  (list 'B B)
                       (list 'E E)
		       (list 'J J)
		       (list 'K K)
		       (list 'M M))))))		
             
             
;;------------------------------------------------------------------------------------------------------             
;; complete file to run amb

;Sicp Amb Eval

(define apply-in-underlying-scheme apply)


(define (eval exp env)
  ((analyze exp) env))


(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
	((amb? exp) (analyze-amb exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->lambda exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))



(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for evaluating the predicate
             fail))))


(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))

  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))


(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env                        
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))


(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)        ; *1*
               (let ((old-value
                      (lookup-variable-value var env))) 
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()    ; *2*
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))


(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    ;; success continuation for this aproc
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                env
                                ;; success continuation for recursive
                                ;; call to get-args
                                (lambda (args fail3)
                                  (succeed (cons arg args)
                                           fail3))
                                fail2))
                    fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))


;------------------------------
;AMB

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))


(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))

;-------------------------------
; Syntassi

(define (self-evaluating? exp)
  (cond ((number? exp) true)
	((string? exp) true)
	(else false)))

(define (variable? exp) (symbol? exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;---
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

;----
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;----

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ; formal parameters
		   (cddr exp)))) ; body

;------
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;-------

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;-------------------

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

;----------

(define (sequence->exp seq)
  (cond ((null? seq) seq)
	((last-exp? seq) (first-exp seq))
	(else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

;----------

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;-----------
; cond -> if
;

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; no else clause
      (let ((first (car clauses))
	    (first-actions (cond-actions (car clauses))) ;non posso usare first dentro il let....
	    (rest (cdr clauses)))

	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "M-Eval: ELSE clause isn't last -- COND->IF"
		       clauses))
	    (make-if (cond-predicate first)
		     (if (eq? '=> (car first-actions))
			 (list (cadr first-actions) (cond-predicate first))
			 (sequence->exp (cond-actions first)))
		     (expand-clauses rest))))))

;---------------

(define (let? exp)
  (tagged-list? exp 'let))

(define (named-let? exp)
  (symbol? (cadr exp)))

(define (let-name exp)
  (cadr exp))


(define (let-clauses exp)
  (if (named-let? exp)
      (caddr exp)
      (cadr exp)))

(define (let-body exp)
  (if (named-let? exp)
      (cdddr exp)
      (cddr exp)))

(define (let-vars clauses)
  (map car clauses))

(define (let-values clauses)
  (map cadr clauses))
	        
(define (let->lambda exp)
  (if (named-let? exp)
      (list 'begin 
	    (list 'define 
		  (cons (let-name exp) (let-vars (let-clauses exp)))
		  (let-body exp))
	    (cons (let-name exp) (let-values(let-clauses exp))))

      (cons (cons 'lambda 
		  (cons (let-vars (let-clauses exp))
			(let-body exp)))
	    (let-values(let-clauses exp)))))

;---------------

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

;----------------

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;---

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))


;-----------------
;PRIME
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
;-----------------



(define primitive-procedures
  (list (list 'car car)
	(list 'cdr cdr)
	(list 'cons cons)
	(list 'null? null?)
	(list 'display display)
	(list 'newline newline)
	(list 'assoc assoc)
	(list 'cadr cadr)
	(list 'list list)
	(list 'not not)
	(list '+ +)
	(list '* *)
	(list '- -)
	(list '/ /)
	(list '= =)
	(list '> >)
	(list '< <)
	(list 'abs abs)
	(list 'prime? prime?)
	(list 'member member)
	(list 'process-time-clock process-time-clock)
	;(list 'or or)
	;(list 'and and)
	))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

;--------------

;--------------
;ENV

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
	  (error "M-Eval: Too many arguments supplied" vars vals)
	  (error "M-Eval: Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (car vals))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error ";;M-Eval error: Unbound variable" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
	     (env-loop (enclosing-environment env)))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	(error "M-Eval: Unbound variable -- SET!" var)
	(let ((frame (first-frame env)))
	  (scan (frame-variables frame)
		(frame-values frame)))))
  (env-loop env))

;------------

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
	     (add-binding-to-frame! var val frame))
	    ((eq? var (car vars))
	     (set-car! vals val))
	    (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
	  (frame-values frame))))

;-----------------

(define (setup-environment)
  (let ((initial-env
	 (extend-environment (primitive-procedure-names)
			     (primitive-procedure-objects)
			     the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

;-------------------


(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")
(define (driver-loop prog)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (if (null? prog) (read) prog)))
      (set! prog '())
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     the-global-environment
                     ;; ambeval success
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     ;; ambeval failure
                     (lambda ()
                       (announce-output
                        ";;; There are no more values of")
                       (user-print input)
                       (driver-loop '())))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))


(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
		     (procedure-parameters object)
		     (procedure-body object)
		     ))
      (display object)))




; (define program '(begin	
		   ; (define (require p)
		     ; (if (not p) (amb)))
		   ; (define (distinct? items)
             ; (cond ((null? items) true)
                   ; ((null? (cdr items)) true)
                   ; ((member (car items) (cdr items)) false)
                   ; (else (distinct? (cdr items)))))				   
		   ; (define (md)
		    ; (let ((baker (amb 1 2 3 4 5)))  (require (not (= baker 5))) 
			    ; (let ((cooper (amb 1 2 3 4 5))) (require (not (= cooper 1)))
				  ; (let ((fletcher (amb 1 2 3 4 5)))  (require (not (= fletcher 5)))
				       ; (let ((miller (amb 1 2 3 4 5))) (require (> miller cooper))
				          ; (let ((smith (amb 1 2 3 4 5)))				  
							; (require (not (= (abs (- smith fletcher)) 1)))
							; (require (not (= fletcher 1)))				 
							; (require (not (= (abs (- fletcher cooper)) 1)))
							; (require (distinct? (list baker cooper fletcher miller smith)))
				  ; (list (list 'baker baker)
				        ; (list 'cooper cooper)
						; (list 'fletcher fletcher)
						; (list 'miller miller)
						; (list 'smith smith))))))))))
				  
(define program '(begin	
		   (define (require p)
		     (if (not p) (amb)))
		   (define (distinct? items)
             (cond ((null? items) true)
                   ((null? (cdr items)) true)
                   ((member (car items) (cdr items)) false)
                   (else (distinct? (cdr items)))))
            (define (or e1 e2)
               (if e1
                   true
                   (if e2
                       true
                       false)))
	    (define (and e1 e2)
               (if e1                  
                   (if e2
                       true
                       false)
                    false))					   
           (define (liars)
            (let ((B (amb 1 2 3 4 5))
                  (E (amb 1 2 3 4 5))
                  (J (amb 1 2 3 4 5))
                  (K (amb 1 2 3 4 5))
                  (M (amb 1 2 3 4 5)))
				  
                (require (distinct? (list B E J K M)))				  
				(require (or (and (= K 2) (not (= B 3))) 
				             (and (not (= K 2)) (= B 3))))
				(require (or (and (= E 1) (not (= J 2))) 
				             (and (not (= E 1)) (= J 2))))
				(require (or (and (= J 3) (not (= E 5))) 
				             (and (not (= J 3)) (= E 5))))
				(require (or (and (= K 2) (not (= M 4))) 
				             (and (not (= K 2))  (= M 4))))
				(require (or (and (= M 4) (not (= B 1))) 
				             (and (not (= M 4))  (= B 1))))
								  
                (list  (list 'B B)
                       (list 'E E)
					   (list 'J J)
					   (list 'K K)
					   (list 'M M))))))						   
 				  


 (newline)
 (display program)
 (newline)
 (newline)
 (driver-loop program)
 (newline)
;(driver-loop '())
