(define gcd-machine
  (make-machine
   '(a b t) ;; 寄存器列表
   (list (list 'rem remainder) (list '= =)) ;; 操作列表
   '(test-b ;; 控制器代码
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))

(set-register-contents! gcd-machine 'a 206)
;; done
(set-register-contents! gcd-machine 'b 40)
;; done
(start gcd-machine)
;; done
(get-register-contents gcd-machine 'a)
;; => 2


