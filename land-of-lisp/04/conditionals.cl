;; Empty Equals False

(if '()
    'i-am-true
    'i-am-false)
;; > I-AM-FALSE
(if '(1)
    'i-am-true
    'i-am-false)
;; > I-AM-TRUE
;; This example shows that when we pass the empty list () into an if form, 
;; it evaluates as a false value, whereas a list that contains an item evaluates as true.

;; Because we can easily detect an empty list, we can process lists using recursion.
;; Let’s look at a common list-eating function, which calculates the length of a list.
(defun my-length (list)
    (if list
        (1+ (my-length (cdr list)))
        0))
(my-length '(list with four symbols))
;; > 4

;; The Four Disguises of ()

(eq '() nil) ;; ==> T
(eq '() ())  ;; ==> T
(eq '() 'nil);; ==> T
