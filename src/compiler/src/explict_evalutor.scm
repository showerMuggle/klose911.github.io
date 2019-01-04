(define eceval-operations
  (list
   ;;primitive Scheme operations
   (list 'read read)

   ;;operations in syntax.scm
   (list 'self-evaluating? self-evaluating?)
   (list 'quoted? quoted?)
   (list 'text-of-quotation text-of-quotation)
   (list 'variable? variable?)
   (list 'assignment? assignment?)
   (list 'assignment-variable assignment-variable)
   (list 'assignment-value assignment-value)
   (list 'definition? definition?)
   (list 'definition-variable definition-variable)
   (list 'definition-value definition-value)
   (list 'lambda? lambda?)
   (list 'lambda-parameters lambda-parameters)
   (list 'lambda-body lambda-body)
   (list 'if? if?)
   (list 'if-predicate if-predicate)
   (list 'if-consequent if-consequent)
   (list 'if-alternative if-alternative)
   (list 'begin? begin?)
   (list 'begin-actions begin-actions)
   (list 'last-exp? last-exp?)
   (list 'first-exp first-exp)
   (list 'rest-exps rest-exps)
   (list 'application? application?)
   (list 'operator operator)
   (list 'operands operands)
   (list 'no-operands? no-operands?)
   (list 'first-operand first-operand)
   (list 'rest-operands rest-operands)

   ;;operations in eceval-support.scm
   (list 'true? true?)
   (list 'make-procedure make-procedure)
   (list 'compound-procedure? compound-procedure?)
   (list 'procedure-parameters procedure-parameters)
   (list 'procedure-body procedure-body)
   (list 'procedure-environment procedure-environment)
   (list 'extend-environment extend-environment)
   (list 'lookup-variable-value lookup-variable-value)
   (list 'set-variable-value! set-variable-value!)
   (list 'define-variable! define-variable!)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'prompt-for-input prompt-for-input)
   (list 'announce-output announce-output)
   (list 'user-print user-print)
   (list 'empty-arglist empty-arglist)
   (list 'adjoin-arg adjoin-arg)
   (list 'last-operand? last-operand?)
   (list 'no-more-exps? no-more-exps?)	;for non-tail-recursive machine
   (list 'get-global-environment get-global-environment))
  )

定义显示求值器模型
(define eceval
  (make-machine
   '(exp env val proc argl continue unev) ;; 7个寄存器
   eceval-operations ;; scheme 原生实现的操作
   ;; 要汇编的指令集代码
   '(
     ;;;;;;;;;;;;;;;;;;;;
     ;; 求值器核心代码 ;;
     ;;;;;;;;;;;;;;;;;;;;
     eval-dispatch 
     (test (op self-evaluating?) (reg exp)) ;; 自求值表达式 
     (branch (label ev-self-eval)) 
     (test (op variable?) (reg exp)) ;; 变量表达式 
     (branch (label ev-variable))
     (test (op quoted?) (reg exp)) ;; 引用表达式
     (branch (label ev-quoted))
     (test (op assignment?) (reg exp)) ;; 赋值表达式
     (branch (label ev-assignment))
     (test (op definition?) (reg exp)) ;; 定义表达式
     (branch (label ev-definition))
     (test (op if?) (reg exp)) ;; 条件表达式
     (branch (label ev-if))
     (test (op lambda?) (reg exp)) ;; lambda 表达式
     (branch (label ev-lambda))
     (test (op begin?) (reg exp)) ;; begin 表达式
     (branch (label ev-begin))
     (test (op application?) (reg exp)) ;; 过程表达式
     (branch (label ev-application))
     (goto (label unknown-expression-type)) ;; 无法求值
     ;;;;;;;;;;;;;;;;;;;;
     ;; 简单表达式求值 ;;
     ;;;;;;;;;;;;;;;;;;;;
     ev-self-eval
     ;; exp 寄存器的值直接放入 val 寄存器
     (assign val (reg exp)) 
     (goto (reg continue))

     ev-variable
     ;; env 环境中查找 exp 变量，结果放入 val 寄存器
     (assign val (op lookup-variable-value) (reg exp) (reg env))
     (goto (reg continue))

     ev-quoted
     ;; exp 寄存器中的表达式 作为参数调用 text-of-quotation 过程，把值放入到 val 寄存器作为结果
     (assign val (op text-of-quotation) (reg exp)) 
     (goto (reg continue))

     ev-lambda
     ;; exp 表达式中获得形参表放入到 unev 寄存器
     (assign unev (op lambda-parameters) (reg exp))
     ;; exp 表达式中获得过程体放入到 exp 寄存器
     (assign exp (op lambda-body) (reg exp))
     ;; 形参表，过程体，当前环境调用 make-procedure 过程，创建一个新的匿名过程，放入到 val 寄存器
     (assign val (op make-procedure)
             (reg unev) (reg exp) (reg env))
     (goto (reg continue))
     ;;;;;;;;;;;;;;;;;;;;
     ;; 过程应用的求值 ;;
     ;;;;;;;;;;;;;;;;;;;;
     
     ;;; 过程应用的求值入口
     ;;; exp 寄存器存放“运算符表达式”和“实参表达式”
     ev-application
     (save continue) ;; 保存续点，因为之后要跳转到 continue 继续执行
     (save env) ;; 保存环境，因为后面对 operands 求值需要
     (assign unev (op operands) (reg exp)) ;; 把 operands 表达式缓存到 unev 
     (save unev) ;; 保存 operands 表达式
     (assign exp (op operator) (reg exp)) ;; 把运算符 operator 放入到 exp寄存器 
     (assign continue (label ev-appl-did-operator)) ;; 对运算符求值后，转去执行 ' ev-appl-did-operator' 
     (goto (label eval-dispatch)) ;; 调用 eval-dispatch 来对运算符进行求值
     ;;; 对某个实参表达式进行求值
     ev-appl-did-operator
     (restore unev)  ;; 恢复过程参数表                
     (restore env) ;; 恢复环境表
     (assign argl (op empty-arglist)) ;; argl 寄存器初始化为空列表
     (assign proc (reg val))  ;; 将运算符过程存入 proc 寄存器
     (test (op no-operands?) (reg unev)) ;; 测试过程参数表是否为空
     (branch (label apply-dispatch)) ;; 如果过程参数表为空，则立刻调用 proc 运算符
     (save proc) ;; 保存求出的运算符过程，而后向下求值运算对象
     ev-appl-operand-loop
     (save argl) ;; 保存实参表
     (assign exp (op first-operand) (reg unev)) ;; 取出第一个运算对象表达式 -> exp 寄存器
     (test (op last-operand?) (reg unev)) ;; 测试这个表达式是否是最后一个表达式
     (branch (label ev-appl-last-arg)) ;; 如果是转而执行 ev-appl-last-arg 
     (save env) ;; 保存当前环境
     (save unev) ;; 保存“整个实参表达式”组成的表
     (assign continue (label ev-appl-accumulate-arg)) ;; 续点设置为 "ev-appl-accumulate-arg"，累加求出的实参值
     (goto (label eval-dispatch)) ;; 求值第一个运算对象
     ;;; 累加求值后的实参值
     ev-appl-accumulate-arg
     (restore unev) ;; 恢复保存的实参表达式组成的表 -> unev 
     (restore env) ;; 恢复环境 -> env 
     (restore argl) ;; 恢复保存的已经求值过的实参值组成的表 -> argl 
     (assign argl (op adjoin-arg) (reg val) (reg argl)) ;; 把求出的实参值添加到“已经求值的实参值组成的表” -> argl 寄存器
     (assign unev (op rest-operands) (reg unev)) ;; “实参表达式组成的表”中去掉已经刚才求值的表达式 -> unev 寄存器
     (goto (label ev-appl-operand-loop)) ;; 求值下一个实参表达式
     ;;; 求值最后一个实参表达式
     ev-appl-last-arg
     (assign continue (label ev-appl-accum-last-arg)) ;; 求值完最后一个运算对象后转到 'ev-appl-accum-last-arg' 
     (goto (label eval-dispatch))
     ev-appl-accum-last-arg 
     (restore argl) ;; 恢复“已经求值的实参值表“ -> argl 
     (assign argl (op adjoin-arg) (reg val) (reg argl)) ;; 把最后一个计算出来的实参值添加到“已经求值的实参值表“  -> argl 
     (restore proc) ;; 恢复求值过的运算符对象 -> proc 
     (goto (label apply-dispatch)) ;; 调用 apply 过程
     ;;; 实际应用的过程
     apply-dispatch
     (test (op primitive-procedure?) (reg proc)) ;; 测试是否是基本过程
     (branch (label primitive-apply))
     (test (op compound-procedure?) (reg proc))  ;; 测试是否是复合过程
     (branch (label compound-apply))
     (goto (label unknown-procedure-type))
     ;;; 应用基本过程
     primitive-apply
     (assign val (op apply-primitive-procedure)
             (reg proc)
             (reg argl))
     (restore continue)
     (goto (reg continue))
     ;;; 应用复合过程
     compound-apply 
     (assign unev (op procedure-parameters) (reg proc)) ;; 取出运算符对象的形参表 -> unev 
     (assign env (op procedure-environment) (reg proc)) ;; 取出运算符对象的环境 -> env 
     (assign env (op extend-environment) 
             (reg unev) (reg argl) (reg env)) ;; 扩充运算符对象的环境：绑定形参表和实参表
     (assign unev (op procedure-body) (reg proc)) ;; 取出运算符对象的过程体 -> unev 
     (goto (label ev-sequence)) ;; 调用“序列求值”代码入口
     ))) 
