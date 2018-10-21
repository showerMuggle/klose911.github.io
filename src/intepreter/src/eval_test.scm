;; meta-eval 
(define (append x y)
  (if (null? x) y
      (cons (car x) (append (cdr x) y))))

(define append (lambda (x y)
		 (if (null? x) y
		     (cons (car x) (append (cdr x) y)))))

(append (list 1 2 3) (list 4 5 6)) 

(append (list (quote a) (quote b) (quote c))
	(list (quote d)  (quote e) (quote f)))


(define factorial (lambda (n)
		    (if (= n 1) 1
			(* (factorial (- n 1)) n)))) 

(factorial 5) 
;; y-combinator
(define Y (lambda (F)
	    (define W (lambda (x)
			(F (lambda arg (apply (x x) arg)))))
	    (W W))) 


(define Y (lambda (F)
	    (begin (define W (lambda (x)
			       (F (lambda (arg) (apply (x x) arg)))))
		   (W W))))

(define A (lambda (arg)
	    (lambda (n)
	      (if (= n 0) 1
		  (* n (arg (- n 1)))))))

((Y A) 5)
((A (Y A)) 5)

;; amb 

(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items))) 
  (amb (car items) (an-element-of (cdr items)))) 



(define (prime-sum-pair list1 list2)
  (begin 
    (define a (an-element-of list1))
    (define b (an-element-of list2))
    (require (prime? (+ a b)))
    (list a b)))
(prime-sum-pair '(1 3 5 8) '(20 35 110))
