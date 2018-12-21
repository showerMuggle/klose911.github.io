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
