;; A variable that is defined globally in Lisp is called a top-level definition. 
;; We can create new top-level definitions with the defparameter function:
(defparameter *small* 1)
(defparameter *big* 100)

;; guess-my-number function
(defun guess-my-number() 
    (ash (+ *small* *big*) -1))

(defun smaller ()
    (setf *big* (1- (guess-my-number)))
    (guess-my-number))

(defun bigger ()
    (setf *small* (1+ (guess-my-number)))
    (guess-my-number))

(defun start-over ()
    (defparameter *small* 1)
    (defparameter *big* 100)
    (guess-my-number))