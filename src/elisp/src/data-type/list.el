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
(nth 3 '(0 1 2 3 4 5))                  ; => 3
(nthcdr 2 '(0 1 2 3 4 5))               ; => (2 3 4 5)
(last '(0 1 2 3 4 5) 2)                 ; => (4 5)
(butlast '(0 1 2 3 4 5) 2)              ; => (0 1 2 3)

;;;;;;;;;;;;;
;; modify  ;;
;;;;;;;;;;;;;
(setq foo '(a b c))                     ; => (a b c)
(setcar foo 'x)                         ; => x
foo                                     ; => (x b c)
(setcdr foo '(y z))                     ; => (y z)
foo                                     ; => (x y z)

(setq foo '(a b c))                     ; => (a b c)
(setcdr foo foo)
foo ; => (a . #0) 这里的 #0 代表的其实是foo这个变量在内存中的地址

(setq foo '(1 2 3))                     ; => (1 2 3)
(setcar foo 'a)                         ; => a
(setcar (cdr foo) 'b)                   ; => b
(setcar (nthcdr 2 foo) 'c)              ; => c
foo                                     ; => (a b c)

;;;;;;;;;;;;
;; stack  ;;
;;;;;;;;;;;;
(setq foo nil)                          ; => nil
(push 'a foo)                           ; => (a)
(push 'b foo)                           ; => (b a)
(pop foo)                               ; => b
foo                                     ; => (a)

;;;;;;;;;;;
;; sort  ;;
;;;;;;;;;;;
(setq foo '(a b c))                     ; => (a b c)
(reverse foo)                           ; => (c b a)
foo                                     ; => (a b c)

(nreverse foo)                          ; => (c b a)
foo                                     ; => (a) 原列表已经被破坏了!!!

(setq foo '(3 2 4 1 5))                 ; => (3 2 4 1 5)
(sort foo '<)                           ; => (1 2 3 4 5)
foo                                     ; => (3 4 5)
