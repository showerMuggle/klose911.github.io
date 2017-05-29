(let ((x 2))
  (let ((f (lambda (y) (* x y))))
    (let ((x 4))
      (funcall f 3))))
;; => 12
