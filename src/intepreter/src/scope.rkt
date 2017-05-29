#lang racket                                  ; 声明用 Racket 语言

(let ([x 2])
  (let ([f (lambda (y) (* x y))])
    (let ([x 4])
      (f 3))))

;; => 6
