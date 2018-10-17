;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOGIC METACIRCULAR EVALUATOR 					        ;;
;; 								        ;;
;; This file can be loaded into Scheme as a whole.		        ;;
;; Then you can initialize and start the evaluator by evaluating        ;;
;; the two commented-out lines at the end of the file (setting up the   ;;
;; global environment and starting the driver loop).		        ;;
;; 								        ;;
;; **WARNING: Don't load this file twice (or you'll lose the primitives ;;
;;  interface, due to renamings of apply).			        ;;
;; 								        ;;
;; must precede def of metacircular apply			        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y)))) 

(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
(job (Bitdiddle Ben) (computer wizard))
(salary (Bitdiddle Ben) 60000)

(address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
(job (Hacker Alyssa P) (computer programmer))
(salary (Hacker Alyssa P) 40000)
(supervisor (Hacker Alyssa P) (Bitdiddle Ben))

(address (Fect Cy D) (Cambridge (Ames Street) 3))
(job (Fect Cy D) (computer programmer))
(salary (Fect Cy D) 35000)
(supervisor (Fect Cy D) (Bitdiddle Ben))

(address (Tweakit Lem E) (Boston (Bay State Road) 22))
(job (Tweakit Lem E) (computer technician))
(salary (Tweakit Lem E) 25000)
(supervisor (Tweakit Lem E) (Bitdiddle Ben))

(address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
(job (Reasoner Louis) (computer programmer trainee))
(salary (Reasoner Louis) 30000)
(supervisor (Reasoner Louis) (Hacker Alyssa P))

(supervisor (Bitdiddle Ben) (Warbucks Oliver))
(address (Warbucks Oliver) (Swellesley (Top Heap Road)))
(job (Warbucks Oliver) (administration big wheel))
(salary (Warbucks Oliver) 150000)

(address (Scrooge Eben) (Weston (Shady Lane) 10))
(job (Scrooge Eben) (accounting chief accountant))
(salary (Scrooge Eben) 75000)
(supervisor (Scrooge Eben) (Warbucks Oliver))

(address (Cratchet Robert) (Allston (N Harvard Street) 16))
(job (Cratchet Robert) (accounting scrivener))
(salary (Cratchet Robert) 18000)
(supervisor (Cratchet Robert) (Scrooge Eben))

(address (Aull DeWitt) (Slumerville (Onion Square) 5))
(job (Aull DeWitt) (administration secretary))
(salary (Aull DeWitt) 25000)
(supervisor (Aull DeWitt) (Warbucks Oliver))

(can-do-job (computer wizard) (computer programmer))
(can-do-job (computer wizard) (computer technician))

(can-do-job (computer programmer)
            (computer programmer trainee))

(and (job ?person (computer programmer))
     (address ?person ?where))

;; (and (job (Hacker Alyssa P) (computer programmer))
;;      (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
;; (and (job (Fect Cy D) (computer programmer))
;;      (address (Fect Cy D) (Cambridge (Ames Street) 3)))

(or (supervisor ?x (Bitdiddle Ben))
    (supervisor ?x (Hacker Alyssa P)))

;; (or (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
;;     (supervisor (Hacker Alyssa P) (Hacker Alyssa P)))
;; (or (supervisor (Fect Cy D) (Bitdiddle Ben))
;;     (supervisor (Fect Cy D) (Hacker Alyssa P)))
;; (or (supervisor (Tweakit Lem E) (Bitdiddle Ben))
;;     (supervisor (Tweakit Lem E) (Hacker Alyssa P)))
;; (or (supervisor (Reasoner Louis) (Bitdiddle Ben))
;;     (supervisor (Reasoner Louis) (Hacker Alyssa P))) 

(and (supervisor ?x (Bitdiddle Ben))
     (not (job ?x (computer programmer))))

(and (salary ?person ?amount)
     (lisp-value > ?amount 30000))

;; rule
(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
	   (address ?person-2 (?town . ?rest-2))
	   (not (same ?person-1 ?person-2))))

(rule (same ?x ?x))

(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))

(lives-near ?x (Bitdiddle Ben))

;; (lives-near (Reasoner Louis) (Bitdiddle Ben))
;; (lives-near (Aull DeWitt) (Bitdiddle Ben)) 

(and (job ?x (computer programmer))
     (lives-near ?x (Bitdiddle Ben)))

(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))

;;; query 
(rule (append-to-form () ?y ?y))

(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))

;;; Query input:
;; (append-to-form (a b) (c d) ?z)
;;; Query results:
;; (append-to-form (a b) (c d) (a b c d))

;;; Query input:
;; (append-to-form (a b) ?y (a b c d))
;;; Query results:
;; (append-to-form (a b) (c d) (a b c d))

;;; Query input:
;; (append-to-form ?x ?y (a b c d))
;;; Query results:
;; (append-to-form () (a b c d) (a b c d))
;; (append-to-form (a) (b c d) (a b c d))
;; (append-to-form (a b) (c d) (a b c d))
;; (append-to-form (a b c) (d) (a b c d))
;; (append-to-form (a b c d) () (a b c d))

(define apply-in-underlying-scheme apply)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; represent the expression  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (variable? exp) (symbol? exp))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

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

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;;;;;;;;;;;;;;;;;;;;;;;
;; implement Thunks  ;;
;;;;;;;;;;;;;;;;;;;;;;;
(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

(define (actual-value exp env)
  (force-it (eval exp env)))

;;;;;;;;;;;;;;;;;;;;;;;
;; Thunk with memory ;;
;;;;;;;;;;;;;;;;;;;;;;;
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk) ; 修改 trunk 为 evaluted-trunk 
           (set-car! (cdr obj) result)  ; 修改表达式为求出的表达式
           (set-cdr! (cdr obj) '())     ; 环境不再需要
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data structure for procedure and envoirment ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (true? x)
  (not (eq? x false))) 

(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

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
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

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

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

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

;;;;;;;;;;;;;;;;;;;;;;;;
;; set up environment ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(define primitive-procedures
  (list (list 'null? null?)
	(list '+ +)
	(list '- -)
	(list '* *)
	(list '/ /)
	(list '> >)
	(list '< <)
	(list '= =)
	;; (list 'car car)
        ;; (list 'cdr cdr)
	;; (list 'cons cons) 
	;;      more primitives
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [moved to start of file] (define apply-in-underlying-scheme apply) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [do later] (define the-global-environment (setup-environment)) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval and apply process  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (eval exp env)
  (cond ((self-evaluating? exp) exp) ; (eval '100 env) 
        ((variable? exp) (lookup-variable-value exp env)) ; (eval x env) 
        ((quoted? exp) (text-of-quotation exp)) ; (eval '(quote a) env) 
        ((assignment? exp) (eval-assignment exp env)) ; (eval '(set! a 10) env)
        ((definition? exp) (eval-definition exp env)) ; (eval '(define a 20) env) 
        ((if? exp) (eval-if exp env)) ; (eval '(if (> a 3) 100 a) env) 
        ((lambda? exp) ; (eval '(lambda (x y) (+ x y)) env)  
         (make-procedure (lambda-parameters exp) 
                         (lambda-body exp)
                         env))
        ((begin? exp) ; (eval '(begin (+ 1 2) (/ 2 1)) env) 
         (eval-sequence (begin-actions exp) env)) 
        ((cond? exp) (eval (cond->if exp) env)) 
	((application? exp)
	 (apply (actual-value (operator exp) env) ;; 强制求值 “复合过程的运算符” 
		(operands exp)
		env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))  ; 强迫求值“基本过程所有参数”
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env) ; 延时求值“复合过程所有参数”
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env) ;; 强迫求值
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env) ;; 延时求值
            (list-of-delayed-args (rest-operands exps)
                                  env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env)) ;; 强迫求值“条件表达式的谓词”
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; read->eval->print loop ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value input the-global-environment))) ;; 强迫求值输入的表达式
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Following are commented out so as not to be evaluated when ;;
;; the file is loaded.					      ;;
;; (define the-global-environment (setup-environment))	      ;;
;; (driver-loop)					      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'LAZY-METACIRCULAR-EVALUATOR-LOADED

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stream implementation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))
