;;;;;;;;;;;;;;;;
;; list       ;;
;;;;;;;;;;;;;;;;

;;; read 
'(1 . 2)                                ; => (1 . 2)
'(?a . 1)                               ; => (97 . 1)
'(1 . "a")                              ; => (1 . "a")
'(1 . nil)                              ; => (1)
'(nil . nil)                            ; => (nil)

(read "(1 . 2)")                        ; => (1 . 2)

;;; empty list 
nil                                     ; => nil
'()                                     ; => nil

(car nil)                               ; => nil
(cdr nil)                               ; => nil

;;; type
'(1 2 3)                                  ; => (1 2 3)
'(1 2 . 3)                                ; => (1 2 . 3)
'(1 . #1=(2 3 . #1#))                     ; => (1 2 3 . #1)


'(1 . (2 . (3 . nil)))                  ; => (1 2 3)

;;; test function
(consp '(1 . 2))                        ; => t
(consp '(1 . (2 . nil)))                ; => t
(consp nil)                             ; => nil
(listp '(1 . 2))                        ; => t
(listp '(1 . (2 . nil)))                ; => t
(listp nil)                             ; => t

(null '()) ;; => t
(null nil) ;; => t
(null '(1 2) ;; => nil

      
;;; constructor

;; cons 
(cons 1 2)                              ; => (1 . 2)
(cons 1 '())                            ; => (1)

(setq foo '(a b))                       ; => (a b)
(cons 'x foo)                           ; => (x a b)
foo
					; => (a b)
(push 'x foo)                           ; => (x a b)
foo                                     ; => (x a b)

;; list 
(list 1 2 3)                            ; => (1 2 3)

;; quote
'((+ 1 2) 3)                            ; => ((+ 1 2) 3)
(list (+ 1 2) 3)                        ; => (3 3)

;; append
(append '(a b) '(c))                    ; => (a b c)
(append '(a b) '(c) '(d))               ; => (a b c d)
(append '(a b) 'c)                      ; => (a b . c)

(append [a b] "cd" nil)                 ; => (a b 99 100)

;;;;;;;;;;;;
;; array  ;;
;;;;;;;;;;;;
