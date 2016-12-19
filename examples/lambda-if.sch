(let ((f (lambda (x y z) 
    (+ x (* y z)))) (a 5))
(if #t (+ a 3) (f 3 4 2)))
