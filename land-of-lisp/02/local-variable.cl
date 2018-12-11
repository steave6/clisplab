
;; (let (variable declarations)
;;    ...body...)

;; The let command is a list of variable declarations.
(let ((a 5)
    (b 6))
    (+ a b))
;;> 11

(let ((a 5)
    (b 6))
    (let ((c 9))
        (+ a b c)))
;;> 20