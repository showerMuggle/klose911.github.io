;;; function 
(defun hello-world (name)
  "Say hello to user whose name is NAME."
  (message "Hello, %s" name))

(hello-world "Emacser")                 ; => "Hello, Emacser"

;;; global variables 
(setq foo "I'm foo")                    ; => "I'm foo"
(message foo)                           ; => "I'm foo"

(defvar foo "Did I have a value?"
  "A demo variable")                    ; => foo

foo                                     ; => "I'm foo"

(defvar bar "I'm bar"
  "A demo variable named \"bar\"")      ; => bar

bar

;;; local varibales 
(defun circle-area (radix)
  (let ((local-pi 3.1415926)
        area)
    (setq area (* local-pi radix radix))
    (message "直径为 %.2f 的圆面积是 %.2f" radix area)))

(circle-area 3) ;; => 直径为 3.00 的圆面积是 28.27

;; (defun circle-area (radix)
;;     (let* ((local-pi 3.1415926)
;; 	   (area (* local-pi radix radix)))
;;       (message "直径为 %.2f 的圆面积是 %.2f" radix area))


;;; lambda expression 
(funcall (lambda (name)
	   (message "Hello, %s!" name)) "Emacser")

(setq foo (lambda (name)
            (message "Hello, %s!" name)))

(funcall foo "Emacser")                   ; => "Hello, Emacser!"

;;;;;;;;;;;;;;;;;;
;; control flow ;;
;;;;;;;;;;;;;;;;;;
;;
