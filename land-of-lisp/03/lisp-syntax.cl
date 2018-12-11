;; This function declaration, which creates a function that simply squares a number, consists of nothing more than parentheses and symbols.
(defun square (n)
    (* n n))

;; Lisp only has one way of organizing bits of code: It uses parentheses to organize the code into lists.

;; Symbols
;; Symbols in Common Lisp are case-insensitive (although most Lispers avoid using uppercase). 
;; To illustrate this, we’ll use a function called eq, which lets us see if two symbols are identical:
(eq 'fooo 'FoOo)

;; When you write a number, 
;; the presence of a decimal point determines whether your number is seen as a floating-point number or an integer.
(+ 1 1.0)
;; > 2.0

;; Lisp can perform some amazing feats with numbers.
;; Most languages would choke on a calculation involving such a large number.
(expt 53 53)
;; > 24356848165022712132477606520104725518533453128685640844505130879576720609150223301256150373

;; Finally, you should know that something weird could happen if you divide two integers:
(/ 4 6)
;; > 2/3
;; The division function is dividing the 4 by 6.
;; But instead of returning a fraction (0.66666...) as you might expect, it returns a rational number

;; Note that we get a different answer if there is a floating-point number in our calculation:
(/ 4.0 6)
;; 0.6666667
;; As in the previous example, the number with the decimal point (4.0) has poisoned our numbers to give us a fraction as a result.

;; Strings

;; Notice that printing our text at the REPL[1] will cause the text to appear twice.
(princ "Tutti Frutti")
;; > Tutti Frutti
;; > "Tutti Frutti"

;; A string can also contain so-called escaped characters.
(princ "He yelled \"Stop that thief!\" from the busy street.")
;; > He yelled "Stop that thief!" from the busy street.
;; > "He yelled \"Stop that thief!\" from the busy street."
