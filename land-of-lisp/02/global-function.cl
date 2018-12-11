(defparameter *small* 1)
(defparameter *big* 100)

;; In Common Lisp, functions are defined with defun, like this:
;; (defun function_name (arguments)
;;   ...)
(defun guess-my-number() 
    (ash (+ *small* *big*) -1))

;; arithmetic shift function
;; 11 => 0b1011 => 0b10110 => 22
(ash 11 1)
;; 11 => 0b1011 => 0b101 => 5
(ash 11 -1)

;; only return 5 function
(defun return-five()
    (+ 2 3))

;; First, we use defun to start the definition of a new global function smaller.
;; Next, we use the setf function to change the value of our global variable *big* .
(defun smaller ()
    (setf *big* (1- (guess-my-number)))
    (guess-my-number))

;; The bigger function works in exactly the same manner,
;; except that it raises the *small* value instead.
(defun bigger ()
    (setf *small* (1+ (guess-my-number)))
    (guess-my-number))

;; As you can see, the start-over function resets the values of *small* and *big*
;;  and then calls guess-my-number again to return a new starting guess.
(defun start-over ()
   (defparameter *small* 1)
   (defparameter *big* 100)
   (guess-my-number))
