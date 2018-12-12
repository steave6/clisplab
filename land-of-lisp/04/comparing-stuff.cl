;; If you want to compare two values in Lisp to find out if they are “the same,”
;; equal, eql, eq, =, string-equal, and equalp are the most commonly used.

;; A Lisper must understand the subtleties of these functions intimately in order to know how to compare values correctly.

;; land-of-lisp/04/httpatomoreillycomsourcenostarchimages782114.png

;; Symbols should always be compared to other symbols with eq:
(defparameter *fruit* 'apple) ;; => *FRUIT*

(cond ((eq *fruit* 'apple) 'its-an-apple)
    ((eq *fruit* 'orange) 'its-an-orange)) ;; => ITS-AN-APPLE
;; If you consider the central role symbols play in Lisp, you’ll realize how useful this function can be.

;; If you’re not dealing with two symbols, just use equal.
;; This command will tell you when two things are isomorphic, meaning they “look the same.”

;;comparing symbols
(equal 'apple 'apple) ;; => T

;;comparing lists
(equal (list 1 2 3) (list 1 2 3)) ;; => T

;;Identical lists created in different ways still compare as the same
(equal '(1 2 3) (cons 1 (cons 2 (cons 3)))) ;; => T

;;comparing integers
(equal 5 5) ;; => T

;;comparing floating point numbers
(equal 2.5 2.5) ;; => T

;;comparing strings
(equal "foo" "foo") ;; => T

;;comparing characters
(equal #\a #\a) ;; => T;; => 

;; The eql command is similar to the eq command, but unlike eq, it also handles comparisons of numbers and characters:
;;comparing symbols
(eql 'foo 'foo) ;; => T

;;comparing numbers
(eql 3.4 3.4) ;; => T

;;comparing characters
(eql #\a #\a) ;; => T

;; The equalp command is essentially the same as the equal command, except that it can handle some difficult comparison cases with a bit of extra sophistication.
;; For instance, it can compare strings with different capitalizations and can compare integers against floating-point numbers:

;;comparing strings with different CAPS
(equalp "Bob Smith" "bob smith") ;; => T
;;comparing integers against floating point numbers
(equalp 0 0.0) ;; => T

;; The remaining comparison commands are just specializations for specific datatypes. Otherwise, they are similar to equal.
;; For instance, the = (equal sign) function handles numbers, string-equal handles strings, and char-equal handles characters.
