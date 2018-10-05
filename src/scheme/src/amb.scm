(define amb-fail '*)

(define initialize-amb-fail
  (lambda ()
    (set! amb-fail
	  (lambda ()
	    (error "amb tree exhausted")))))

(initialize-amb-fail)

(define call/cc call-with-current-continuation)
;; (call/cc (lambda (k) (+ 1 2))) ;; call/cc 的参数是一个函数，这个函数只有一个参数，调用 call/cc 会调用这个函数，然后把当前的续延传入这个函数

(define-syntax amb
  (syntax-rules ()
    ((_) (amb-fail))
    ((_ a) a)
    ((_ a b ...)
     (let ((prev-amb-fail amb-fail)) ; 把全局变量 amb-fail 赋值给 prev-amb-fail 供回溯 // 续延1 
       (call/cc ;调用下面的匿名函数 lambda (k) ...  
	(lambda (k) ; 续延1 作为参数 k 传入
	  (set! amb-fail ; 设置全局变量 amb-fail 为下面匿名函数
		(lambda () ; 如果 (k a) 调用失败，会调用下面的函数
		  (set! amb-fail prev-amb-fail) ; 恢复全局变量 amb-fail 为续延1时候的值
		  (k (amb b ...)))) ; 在续延1 时候求值 b 表达式
	  (k a))))))) ; 续延1 时候求值 a 表达式，如果求值失败，调用 amb-fail，


(amb 1 2 3) ; => 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 每调用一次 amb 都会触发 (amb-fail) 的调用，转而调用宏中的 (k (amb b) ...) ，这在保存的续延1中去求值下一个表达式 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(amb) ; => 2
(amb) ; => 3
(amb) ;amb tree exhausted

(if (amb (> 1 2) (< 2 1) (> 5 1))
    1
    (amb)) 
;; => 1 
