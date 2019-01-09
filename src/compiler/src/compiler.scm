;;;;;;;;;;;;;;;;
;; 编译的入口 ;;
;;;;;;;;;;;;;;;;
(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

;;; 指令序列构造函数
;;; needs: 需要使用的寄存器集合
;;; modifies: 执行是修改的寄存器集合
;;; statements: 语句 
(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

;;; 产生空指令序列
(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

;;;;;;;;;;;;;;;;;;
;; 表达式的编译 ;;
;;;;;;;;;;;;;;;;;;

;;; 编译连接代码
;;; linkage: 连接，return, next, 或者标号
(define (compile-linkage linkage)
  (cond ((eq? linkage 'return) ;; 过程返回
         (make-instruction-sequence '(continue) '() ;; 需要 continue 寄存器，不修改任何寄存器
				    '((goto (reg continue))))) ;; 产生 (goto (reg continue)) 指令
        ((eq? linkage 'next) ;; 下一条语句
         (empty-instruction-sequence)) ;; 不产生任何指令，不需要，也不修改任何寄存器
        (else ;; 连接为标号
         (make-instruction-sequence '() '() ;; 不需要，也不修改任何寄存器
				    `((goto (label ,linkage))))))) ;; 产生跳转到标号的指令

;;; 把连接代码加入到指令序列最后
;;; linkage: 连接代码
;;; instruction-sequence: 指令序列
(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue) ;; comile-linkage 产生的指令可能需要 continue
	      instruction-sequence
	      (compile-linkage linkage)))

;;; 编译自求值语句
;;; exp 自求值表达式
;;; target : 目标寄存器
;;; linkage: 连接目标
(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
		    (make-instruction-sequence '() (list target)
					       `((assign ,target (const ,exp))))))
;;; 编译引用语句
;;; exp: 引用表达式
;;; target : 目标寄存器
;;; linkage: 连接目标
(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
		    (make-instruction-sequence '() (list target)
					       `((assign ,target (const ,(text-of-quotation exp)))))))

;;; 编译变量语句
;;; exp: 引用表达式
;;; target : 目标寄存器
;;; linkage: 连接目标
(define (compile-variable exp target linkage)
  (end-with-linkage linkage
		    (make-instruction-sequence '(env) (list target)
					       `((assign ,target
							 (op lookup-variable-value) (const ,exp) (reg env))))))

;;; 编译赋值语句
(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp)) ;; 获取 赋值表达式的变量
        (get-value-code ;; 编译”赋值表达式的求值表达式“为”指令序列“ 
         (compile (assignment-value exp) 'val 'next))) ;; 目标寄存器 val：生成代码把值放入 val ，连接方式 next : 执行随后的语句
    (end-with-linkage linkage
		      (preserving '(env) ;; 所用拼接方式要求维持 env，因为设置变量都需要当时环境，而产生变量值的代码可能是复杂表达式的编译结果，其中完全可能修改 env 寄存器
				  get-value-code
				  (make-instruction-sequence '(env val) (list target)
							     `((perform (op set-variable-value!) ;; 执行真实的赋值操作
									(const ,var)
									(reg val)
									(reg env)) ;; 把,var 作为变量名，把 val寄存器的值（求值表达式计算的结果），绑定在 env 寄存器指向的环境中  
							       (assign ,target (const ok)))))))) ;; 常量 ok 放入 target 目标寄存器 ，作为返回值

;;; 编译定义语句
(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
	(get-value-code
	 (compile (definition-value exp) 'val 'next)))
    (end-with-linkage linkage
		      (preserving '(env)
				  get-value-code
				  (make-instruction-sequence '(env val) (list target)
							     `((perform (op define-variable!) ;; 这里调用 define-variable! 
									(const ,var)
									(reg val)
									(reg env))
							       (assign ,target (const ok))))))))

;;; 条件表达式
(define (compile-if exp target linkage)
  ;;; 生成三个新标号
  (let ((t-branch (make-label 'true-branch))
	(f-branch (make-label 'false-branch))                    
	(after-if (make-label 'after-if)))
    (let ((consequent-linkage
	   (if (eq? linkage 'next) after-if linkage))) ;; 根据连接确定 then 最后的连接
      (let ((p-code (compile (if-predicate exp) 'val 'next)) ;; 编译谓词表达式代码，求值的结果放入到 val寄存器，连接的方式：next 
	    (c-code 
	     (compile
	      (if-consequent exp) target consequent-linkage)) ;; 编译谓词为真时候的表达式，目标寄存器是target，使用计算出来的 consequent-linkage作为连接方式
	    (a-code
	     (compile (if-alternative exp) target linkage))) ;; 编译谓词为假时候的表达式，目标寄存器仍为target，连接方式和条件表达式的一样: linkage
	(preserving '(env continue) ;; 求谓次条件的值，前后 env,  continue两个寄存器
		    p-code 
		    (append-instruction-sequences
		     ;; 产生如下指令序列：检查 val 寄存器（谓词计算结果存放在此）是否为假，如果为假则执行 f-branch 对应的标号
		     (make-instruction-sequence '(val) '() 
						`((test (op false?) (reg val))
						  (branch (label ,f-branch)))) 
		     (parallel-instruction-sequences ;; 拼接两段不会同时执行的代码
		      (append-instruction-sequences t-branch c-code) 
		      (append-instruction-sequences f-branch a-code))
		     after-if))))))

;;; 编译序列表达式
(define (compile-sequence seq target linkage)
  (if (last-exp? seq) ;; 测试是否是最后一个子表达式
      (compile (first-exp seq) target linkage) ;; 编译最后一个子表达式，目标寄存器：target，连接：整个序列的连接
      (preserving '(env continue) ;; 需要保留 env (下一个子表达式求值需要)， continue（最后一个拼接需要）
		  (compile (first-exp seq) target 'next) ;; 编译下一个子表达式，目标寄存器：target，连接: next 
		  (compile-sequence (rest-exps seq) target linkage)))) ;; 递归编译余下的子表达式，目标寄存器：target，连接：整个序列的连接

;;; 编译 lambda 表达式
(define (compile-lambda exp target linkage)
  ;; 生成 2个新的标号： proc-entry 和 after-lambda 
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    ;; 计算 lambda表达式的连接
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
			  (make-instruction-sequence '(env) (list target)
						     `((assign ,target
							       (op make-compiled-procedure)
							       (label ,proc-entry)
							       (reg env))))) ;; 构建一个过程对象，标号是 proc-entry的值，环境是 env, 把这个对象赋值到 target 寄存器
        (compile-lambda-body exp proc-entry)) ;; 编译 lambda 过程体
       after-lambda)))) ;; after-lambda 标号

 ;;; 编译 lambda 运行过程
(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp))) ;; 获得形参表
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env) ;; 构造过程体需要的寄存器是 env, proc, argl , 构造完成后：修改的寄存器是 env 
				`(,proc-entry ;; 过程体对应的标号
				  (assign env (op compiled-procedure-env) (reg proc)) ;; 调用lambda表达式时候的环境
				  (assign env 
					  (op extend-environment) ;; 扩充环境
					  (const ,formals) ;; 把实参和形参在环境中绑定
					  (reg argl)
					  (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return)))) ;; 编译过程体的指令，执行过程体的目标寄存器是 val, 连接方式: return（直接返回）
