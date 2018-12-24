(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
	    ((eq? message 'set)
	     (lambda (value) (set! contents value)))
	    (else
	     (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

;; (define test-register (make-register 'test))
;; (get-contents test-register) ;; *unassigned*
;; (set-contents! test-register 10)
;; (get-contents test-register) ;; 10

(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request -- STACK"
                         message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

;; (define test-stack (make-stack))
;; (pop test-stack) ;;  Empty stack -- POP
;; (push test-stack 1)
;; (pop test-stack) ;;  1
;; (pop test-stack) ;;  Empty stack -- POP
;; (push test-stack 2) ;;
;; (test-stack 'initialize) ;; done
;; (pop test-stack) ;;  Empty stack -- POP

(define (make-new-machine)
  (let ((pc (make-register 'pc)) ;; 指令寄存器
	(flag (make-register 'flag)) ;; 标志寄存器
	(stack (make-stack)) ;; 栈
	(the-instruction-sequence '())) ;; 指令列表
    (let ((the-ops ;; 操作列表
	   (list (list 'initialize-stack
		       (lambda () (stack 'initialize)))))
	  (register-table ;; 寄存器列表
	   (list (list 'pc pc) (list 'flag flag))))
      ;; 添加新的寄存器
      (define (allocate-register name) 
	(if (assoc name register-table)
	    (error "Multiply defined register: " name)
	    (set! register-table
		  (cons (list name (make-register name))
			register-table)))
	'register-allocated)
      ;; 从寄存器列表获得特定寄存器
      (define (lookup-register name)
	(let ((val (assoc name register-table)))
	  (if val
	      (cadr val)
	      (error "Unknown register:" name))))
      ;; 执行指令
      (define (execute)
	(let ((insts (get-contents pc))) ;; 获得 pc 寄存器的值
	  (if (null? insts)
	      'done
	      (begin
		((instruction-execution-proc (car insts)))
		(execute)))))
      (define (dispatch message)
	(cond ((eq? message 'start) ;; 启动机器
	       (set-contents! pc the-instruction-sequence) ;; pc 寄存器指向指令列表
	       (execute)) ;; 执行指令
	      ((eq? message 'install-instruction-sequence) ;; 安装指令列表 
	       (lambda (seq) (set! the-instruction-sequence seq))) 
	      ((eq? message 'allocate-register) allocate-register) ;; 添加寄存器
	      ((eq? message 'get-register) lookup-register) ;; 查询寄存器
	      ((eq? message 'install-operations) ;; 安装操作过程
	       (lambda (ops) (set! the-ops (append the-ops ops))))
	      ((eq? message 'stack) stack) ;; 返回栈
	      ((eq? message 'operations) the-ops) ;; 返回操作列表
	      (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

;; 启动机器
(define (start machine)
  (machine 'start))

;; 获得寄存器中的值
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

;; 设置寄存器中的值
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

;; 取指定寄存器信息
(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

;; 定义寄存器机器
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)    
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))


;;;;;;;;;;;;
;; 指令表 ;;
;;;;;;;;;;;;
;; 构造指令表
(define (make-instruction text)
  (cons text '())) ;; 构造指令表时，执行过程暂时用一个空表，后面将填入实际执行过程
;; 获取指令
(define (instruction-text inst)
  (car inst))
;; 获得指令执行过程
(define (instruction-execution-proc inst)
  (cdr inst))
;; 设置指令执行过程
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

;;;;;;;;;;;;
;; 标号表 ;;
;;;;;;;;;;;;
;; 把标号和指令做关联
(define (make-label-entry label-name insts)
  (cons label-name insts)) ;; 标号表项就是序对 

;; 查询某个标号关联表下某个标号对应的指令
(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))
;;;;;;;;;;;;
;; 汇编器 ;;
;;;;;;;;;;;;
(define (assemble controller-text machine)
  (extract-labels controller-text ;; 构造初始指令表和标号表
                  (lambda (insts labels) ;; 指令表，标号表作为参数
                    (update-insts! insts labels machine) ;; 以指令表、标号表和机器为参数，生成各条指令的执行过程加入指令表
                    insts))) ;; 返回指令表

;; 逐项检查指令表内容，提取其中的标号
;; text: 控制器代码
;; receive: 函数参数 
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      ;; 递归处理控制器正文序列的 cdr
      (extract-labels (cdr text)
		      (lambda (insts labels)
			(let ((next-inst (car text))) 
			  (if (symbol? next-inst) ;; 检查 car 是否是标号
			      (receive insts ;; 如果是标号，加入标号项
				  (cons (make-label-entry next-inst
							  insts)
					labels))
			      (receive (cons (make-instruction next-inst)
					     insts)  ;; 反之加入指令表项
				  labels)))))))

;;; 原来每个位置只有指令正文，执行过程用空表占位，现在加入实际的执行过程
;;; insts: 指令表
;;; labels: 标号关联表
;;; machine: 机器模型
(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
	(flag (get-register machine 'flag))
	(stack (machine 'stack))
	(ops (machine 'operations)))
    (for-each ;; 给一条指令设置执行过程
     (lambda (inst)
       (set-instruction-execution-proc! 
	inst
	(make-execution-procedure ;; 构造一条指令的执行过程
	 (instruction-text inst) labels machine
	 pc flag stack ops)))
     insts)))
