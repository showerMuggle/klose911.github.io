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

;;;;;;;;;;;;;;;;;;
;; simple query ;;
;;;;;;;;;;;;;;;;;;

(define (simple-query query-pattern frame-stream)
  (stream-flatmap ; 把处理各框架得到的流合并为一个流 
   (lambda (frame) 
     (stream-append-delayed ; 组合两个流
      (find-assertions query-pattern frame) ; 找数据库里的匹配断言，生成扩充框架的流 
      (delay (apply-rules query-pattern frame)))) ; 应用可应用的规则，生成扩充框架的流 
   frame-stream))

;;; and queries
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))

(put 'and 'qeval conjoin)

;;; or queries
(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))

(put 'or 'qeval disjoin)

;;; not filters 
(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands) ; 逆条件查询
			      (singleton-stream frame))) ; 框架流中的某个框架是否满足 逆查询
         (singleton-stream frame) ; 满足的话，加入结果流
         the-empty-stream)) ; 不满足的话，过滤掉
   frame-stream))

(put 'not 'qeval negate) 

;;; lisp-value filters
(define (lisp-value call frame-stream) ; call 被应用的谓词
  (stream-flatmap
   (lambda (frame)
     (if (execute ; 处理实例化后的谓词，类似于 eval，但不求值谓词的参数（因它们已经是值）
          (instantiate ; instantiate 用 frame 实例化 call 里的变量，得到所需的谓词表达式
           call
           frame
           (lambda (v f)
             (error "Unknown pat var -- LISP-VALUE" v)))) ; 遇到未约束的变量，就是错误 
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(put 'lisp-value 'qeval lisp-value)

;; 将谓词应用于实际参数
(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))

;;; always-true
(define (always-true ignore frame-stream)
  frame-stream)

(put 'always-true 'qeval always-true)


;;;;;;;;;;;;;;;;;;;
;; Pattern match ;;
;;;;;;;;;;;;;;;;;;;
(define (find-assertions pattern frame)
  (stream-flatmap (lambda (datum)
                    (check-an-assertion datum pattern frame)) ; 丢掉不可能匹配的断言
                  (fetch-assertions pattern frame))) ; 从数据库获取断言的流

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame))) ; 调用匹配器
    (if (eq? match-result 'failed) 
        the-empty-stream ; 失败返回空流
        (singleton-stream match-result)))) ; 成功包含 一个扩充框架的流

(define (pattern-match pat dat frame) 
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame) ; 相同时，匹配成功，直接返回原框架
        ((var? pat) (extend-if-consistent pat dat frame)) ; 模式是个变量，基于 frame 和新约束做扩充，检查是否协调
        ((and (pair? pat) (pair? dat)) 
         (pattern-match (cdr pat) ; 递归匹配模式和数据的cdr 部分
                        (cdr dat)
                        (pattern-match (car pat)
                                       (car dat)
                                       frame))) ; 以匹配 car 部分得到的可能，扩充的框架作为框架
        (else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame))) ; 找出 var 在 frame 里的约束
    (if binding
        (pattern-match (binding-value binding) dat frame) ; 检查是否匹配 
        (extend var dat frame)))) ; var 无约束，把新约束加入frame

;;;;;;;;;;;
;; rules ;;
;;;;;;;;;;;
(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule))) ; 规则里的变量统一改名，使之不会与其他规则冲突
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion clean-rule)
                        query-frame))) ; 做查询模式和规则结论做合一匹配
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result)))))) ; 基于得到的 新框架流 做 规则体 的匹配

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

;;;;;;;;;;;;;;;;;;
;; Unifications ;;
;;;;;;;;;;;;;;;;;;
(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame)) ; 两边都可能是变量
        ((var? p2) (extend-if-possible p2 p1 frame))  
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding 
           (unify-match
            (binding-value binding) val frame)) ; 若var 已有约束，要求其约束值可与 val 合一    
          ((var? val)   ; 匹配的另一方也是变量。如果该变量有约束，则要求 var 可与该变量的约束值合一
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame)     ; val 依赖于 var 时匹配失败
           'failed)
          (else (extend var val frame)))))

;;; exp : 模式
;;; var: 变量
;;; frame: 框架
(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e) ; 是变量
           (if (equal? var e)  ; 和 var 相同
               true
               (let ((b (binding-in-frame e frame))) ; b 是 e 在 frame 里的约束 
                 (if b
                     (tree-walk (binding-value b)) ; 检查约束值 是否有依赖
                     false))))
          ((pair? e) ; 递归检查每个序对的 car 和 cdr 
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))

(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc 
        (qproc (contents query) frame-stream) ; 如果是特殊处理过程，就用该过程处理
        (simple-query query frame-stream)))) ; 非特殊形式的表达式都当作简单查询


(define (instantiate exp frame unbound-var-handler)
  (define (copy exp) ; 使用 frame 里的约束构造 exp 的实例化副本
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame)))) ; 用传入的 过程参数 来 处理未约束变量
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))


(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q)) ; 加入断言或规则
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop)) ; 重启主循环
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map 
             (lambda (frame)
               (instantiate q ; 用结果流中的框架 frame 做查询模式q 的实例化
                            frame
                            (lambda (v f) ; f 的实参是 frame 
                              (contract-question-mark v)))) ;处理未约束变量，产生适当的输出形式
             (qeval q (singleton-stream '())))) ; 从包含一个空框架的流查询出匹配的框架流
           (query-driver-loop))))) ; 重启主循环


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
;; (define (cons x y)
;;   (lambda (m) (m x y)))

;; (define (car z)
;;   (z (lambda (p q) p)))

;; (define (cdr z)
;;   (z (lambda (p q) q)))

;;;;;;;;;;;;;;;;;;;;;;;
;; Logic Programming ;;
;;;;;;;;;;;;;;;;;;;;;;;
;; (define (append x y)
;;   (if (null? x)
;;       y
;;       (cons (car x) (append (cdr x) y)))) 

;; (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
;; (job (Bitdiddle Ben) (computer wizard))
;; (salary (Bitdiddle Ben) 60000)

;; (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
;; (job (Hacker Alyssa P) (computer programmer))
;; (salary (Hacker Alyssa P) 40000)
;; (supervisor (Hacker Alyssa P) (Bitdiddle Ben))

;; (address (Fect Cy D) (Cambridge (Ames Street) 3))
;; (job (Fect Cy D) (computer programmer))
;; (salary (Fect Cy D) 35000)
;; (supervisor (Fect Cy D) (Bitdiddle Ben))

;; (address (Tweakit Lem E) (Boston (Bay State Road) 22))
;; (job (Tweakit Lem E) (computer technician))
;; (salary (Tweakit Lem E) 25000)
;; (supervisor (Tweakit Lem E) (Bitdiddle Ben))

;; (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
;; (job (Reasoner Louis) (computer programmer trainee))
;; (salary (Reasoner Louis) 30000)
;; (supervisor (Reasoner Louis) (Hacker Alyssa P))

;; (supervisor (Bitdiddle Ben) (Warbucks Oliver))
;; (address (Warbucks Oliver) (Swellesley (Top Heap Road)))
;; (job (Warbucks Oliver) (administration big wheel))
;; (salary (Warbucks Oliver) 150000)

;; (address (Scrooge Eben) (Weston (Shady Lane) 10))
;; (job (Scrooge Eben) (accounting chief accountant))
;; (salary (Scrooge Eben) 75000)
;; (supervisor (Scrooge Eben) (Warbucks Oliver))

;; (address (Cratchet Robert) (Allston (N Harvard Street) 16))
;; (job (Cratchet Robert) (accounting scrivener))
;; (salary (Cratchet Robert) 18000)
;; (supervisor (Cratchet Robert) (Scrooge Eben))

;; (address (Aull DeWitt) (Slumerville (Onion Square) 5))
;; (job (Aull DeWitt) (administration secretary))
;; (salary (Aull DeWitt) 25000)
;; (supervisor (Aull DeWitt) (Warbucks Oliver))

;; (can-do-job (computer wizard) (computer programmer))
;; (can-do-job (computer wizard) (computer technician))

;; (can-do-job (computer programmer)
;;             (computer programmer trainee))

;; (and (job ?person (computer programmer))
;;      (address ?person ?where))

;; ;; (and (job (Hacker Alyssa P) (computer programmer))
;; ;;      (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
;; ;; (and (job (Fect Cy D) (computer programmer))
;; ;;      (address (Fect Cy D) (Cambridge (Ames Street) 3)))

;; (or (supervisor ?x (Bitdiddle Ben))
;;     (supervisor ?x (Hacker Alyssa P)))

;; ;; (or (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
;; ;;     (supervisor (Hacker Alyssa P) (Hacker Alyssa P)))
;; ;; (or (supervisor (Fect Cy D) (Bitdiddle Ben))
;; ;;     (supervisor (Fect Cy D) (Hacker Alyssa P)))
;; ;; (or (supervisor (Tweakit Lem E) (Bitdiddle Ben))
;; ;;     (supervisor (Tweakit Lem E) (Hacker Alyssa P)))
;; ;; (or (supervisor (Reasoner Louis) (Bitdiddle Ben))
;; ;;     (supervisor (Reasoner Louis) (Hacker Alyssa P))) 

;; (and (supervisor ?x (Bitdiddle Ben))
;;      (not (job ?x (computer programmer))))

;; (and (salary ?person ?amount)
;;      (lisp-value > ?amount 30000))

;;;;;;;;;;
;; rule ;;
;;;;;;;;;;
;; (rule (lives-near ?person-1 ?person-2)
;;       (and (address ?person-1 (?town . ?rest-1))
;; 	   (address ?person-2 (?town . ?rest-2))
;; 	   (not (same ?person-1 ?person-2))))

;; (rule (same ?x ?x))

;; (rule (wheel ?person)
;;       (and (supervisor ?middle-manager ?person)
;;            (supervisor ?x ?middle-manager)))

;; (lives-near ?x (Bitdiddle Ben))

;; ;; (lives-near (Reasoner Louis) (Bitdiddle Ben))
;; ;; (lives-near (Aull DeWitt) (Bitdiddle Ben)) 

;; (and (job ?x (computer programmer))
;;      (lives-near ?x (Bitdiddle Ben)))

;; (rule (outranked-by ?staff-person ?boss)
;;       (or (supervisor ?staff-person ?boss)
;;           (and (supervisor ?staff-person ?middle-manager)
;;                (outranked-by ?middle-manager ?boss))))

;;;;;;;;;;;
;; query ;;
;;;;;;;;;;;

;; (rule (append-to-form () ?y ?y))

;; (rule (append-to-form (?u . ?v) ?y (?u . ?z))
;;       (append-to-form ?v ?y ?z))

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
