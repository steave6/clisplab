;; We define local functions using the flet command.
;; (flet ((function_name (arguments)
;;           ...function body...))
;;    ...body...)

;; Here is an example:
(flet ((f (n) 
    (+ n 10)))
    (f 5))
;; > 15

;; A single flet command can be used to declare multiple local functions at once.
(flet ((f (n)
            (+ n 10))
          (g (n)
            (- n 3)))
    (g (f 5)))
;; > 12

;; To make function names available in defined functions, we can use the labels command.
(labels ((a (n)
              (+ n 5))
            (b (n)
               (+ (a n) 6)))
    (b 10))
;; > 21