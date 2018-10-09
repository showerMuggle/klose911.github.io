(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count) (iter (+ count 1) (+ count accum)))
          (else (iter (+ count 1) accum))))
  (iter a 0))

(define (sum-primes a b)
  (accumulate +
              0
              (filter prime? (enumerate-interval a b))))

;; (car (cdr (filter prime?
;;                   (enumerate-interval 10000 1000000))))


(define the-empty-stream '())
(define (stream-null? s)
  (eq? s the-empty-stream))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))


(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

;; (stream-enumerate-interval 10000 1000000)
;; (cons-stream 10000 (stream-enumerate-interval 10001 1000000))
;; (cons 10000 (delay (stream-enumerate-interval 10001 1000000)) 

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred ; 这里并不求值，只是保存求值的表达式
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


;; (stream-car
;;  (stream-cdr
;;   (stream-filter prime? (stream-enumerate-interval 10000 1000000)))) ; => 10009 

;; (stream-filter prime?  (cons 10000 (delay (stream-enumerate-interval 10001 1000000)))) 
;; (prime? (stream-car  (cons 10000 (delay (stream-enumerate-interval 10001 1000000))))) 
;; (prime? 10000) ; => #f

;; (stream-filter prime? (stream-cdr (stream-enumerate-interval 10001 1000000)))
;; (stream-filter prime? (force (delay (stream-enumerate-interval 10001 1000000))))
;; (stream-filter prime? (stream-enumerate-interval 10001 1000000))

;; ......

;; (stream-filter prime? (stream-enumerate-interval 10007 1000000))
;; (prime? 10007) ; => #t
;; (cons-stream 10007 (stream-filter prime? stream-cdr (stream-enumerate-interval 10007 1000000)))
;; (cons 10007 (delay (stream-filter prime? (stream-cdr (stream-enumerate-interval 10007 1000000)))))

;; (stream-cdr (cons 10007 (delay (stream-filter prime? (stream-cdr (stream-enumerate-interval 10007 1000000))))))
;; (force (delay (stream-filter prime? (stream-cdr (stream-enumerate-interval 10007 1000000)))))
;; (stream-filter prime? (stream-cdr (stream-enumerate-interval 10007 1000000)))
;; (stream-filter prime? (stream-enumerate-interval 10008 1000000))

;; ....

;; (stream-filter prime? (stream-enumerate-interval 10009 1000000))
;; (prime 10009) ; => #t
;; (cons 10009 (delay (stream-filter prime? (stream-cdr (stream-enumerate-interval 10009 1000000)))))

;; (stream-car (cons 10009 (delay (stream-filter prime? (stream-cdr (stream-enumerate-interval 10009 1000000)))))) ; => 10009 

(define-syntax delay
  (syntax-rules ()
    ((delay exp ...)
     (lambda () exp ...))))

(define (force delayed-object)
  (delayed-object))

(define (memo-proc proc)
  (let ((already-run? false)
	(result false))
    (lambda ()
      (if (not already-run?)
	  (begin (set! result (proc))
		 (set! already-run? true)
		 result)
	  result))))

(define-syntax delay
  (syntax-rules ()
    ((delay exp ...)
     (memo-proc (lambda () exp ...)))))

;;; cons-stream
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fib (fibgen 0 1))

(stream-ref fib 20) ; => 6765  
