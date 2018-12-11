;; Code Mode

;; Whenever you type something into the Lisp REPL, the compiler assumes that you’re entering a command you want to execute.
;; Lisp code should be in a special type of list: a form.

;; When reading a form, Lisp sends all other items in the list to the function as parameters.
;; For example, enter the following into your REPL:
(expt 2 3)
;; This command was entered in the standard way for Lisp: as a form with the function name at the beginning.

;; This example has two nested forms.
(expt 2 (+ 3 4))
;; One of these arguments (+ 3 4) is a form in its own right. This form is then executed, yielding 7.
;; Afterward, this result is passed to the outer expt form, which is then executed.


;; Data Mode

;; As you might imagine, any stuff written in data mode is treated as data.
;; Let’s take a look at data mode in action. We’ll enter the same form that we entered in code mode in the previous example, with one difference:
'(expt 2 3)
;; > (expt 2 3)

;; The single quote tells Lisp to treat the subsequent form as a chunk of data.

;; Placing a quote in front of lists so that they won’t be evaluated as a command is called quoting.
;; By using quoting, you can tell Lisp, “This next part isn’t a command. It’s just a chunk of data for my program.”
