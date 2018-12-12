
;; One Thing at a Time with if

;; The if command can be used to make different things happen when things are true or false.
(if (= (+ 1 2) 3)
    'yup
    'nope) ;; => YUP

(if (= (+ 1 2) 4)
    'yup
    'nope) ;; => NOPE

;; The if command can also be used to check whether a list is empty:
(if '(1)
    'the-list-has-stuff-in-it
    'the-list-is-empty) ;; THE-LIST-HAS-STUFF-IN-IT

(if '()
    'the-list-has-stuff-in-it
    'the-list-is-empty) ;; THE-LIST-IS-EMPTY

;; All we’re doing here is checking whether the number 5 is odd
(if (oddp 5)
    'odd-number
    'even-number) ;; ODD-NUMBER

;; There’s a lot happening in this harmless-looking little command—stuff
;; that’s important to understanding Lisp. Here are two important observations:
    ;; Only one of the expressions after the if is actually evaluated.
    ;; We can only do one thing in an if statement.

(if (oddp 5)
    'odd-number
    (/ 1 0))

;; Any self-respecting, law-abiding Lisp function would kick your butt to the curb if you tried to run this code, because you’re dividing by zero.
;; But if is not just a function. It’s a special form, which gives it special privileges, such as the right to not evaluate all its parameters in the normal way.

;; Conditional commands in Lisp are typically special forms.

;; With progn, only the last evaluation is returned as the value of the full expression.
(defvar *number-was-odd* nil)

(if (oddp 5)
    (progn (setf *number-was-odd* t)
            'odd-number)
    'even-number) ;; => ODD-NUMBER

*number-was-odd* ;; => T


;; Going Beyond if: The when and unless Alternatives

;; Lisp has several other commands that include an implicit progn. The most basic of these are when and unless:
(defvar *arch-enemy* nil)
(when (oddp 5)
        (setf *number-is-odd* t)
        'odd-number) ;; => ODD-NUMBER

*number-is-odd* ;; => T

(unless (oddp 4)
          (setf *number-is-odd* nil)
          'even-number) ;; => EVEN-NUMBER

*number-is-odd* ;; => NIL


;; The Command That Does It All: cond
(defvar *arch-enemy* nil)
(defun pudding-eater (person)
    (cond ((eq person 'henry) (setf *arch-enemy* 'stupid-lisp-alien)
                                '(curse you lisp alien - you ate my pudding))
            ((eq person 'johnny) (setf *arch-enemy* 'useless-old-johnny)
                                '(i hope you choked on my pudding johnny))
            (t                  '(why you eat my pudding stranger ?))))

(pudding-eater 'johnny) ;; => (I HOPE YOU CHOKED ON MY PUDDING JOHNNY)
*arch-enemy* ;; => JOHNNY

;; Branching with case
(defun pudding-eater (person)
        (case person
            ((henry)   (setf *arch-enemy* 'stupid-lisp-alien)
                        '(curse you lisp alien - you ate my pudding))
            ((johnny)  (setf *arch-enemy* 'useless-old-johnny)
                        '(i hope you choked on my pudding johnny))
            (otherwise '(why you eat my pudding stranger ?))))