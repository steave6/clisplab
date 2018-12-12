;; The conditionals and and or are simple mathematical operators

(and (oddp 5) (oddp 7) (oddp 9)) ;; => T

(or (oddp 4) (oddp 7) (oddp 8)) ;; => T

;; They do not look like conditional commands, such as if or cond.
;; However, they can be used for conditional behavior.
(defparameter *is-it-even* nil) ;; => *IS-IT-EVEN*

(or (oddp 4) (setf *is-it-even* t)) ;; => T
*is-it-even* ;; => T

(defparameter *is-it-even* nil) ;; => *IS-IT-EVEN*
(or (oddp 5) (setf *is-it-even* t)) ;; => T
*is-it-even* ;; => NIL

;; Here, the function ask-user-about-saving would ask the user about the file, and then return true or false based on the user’s wishes.
(if *file-modified*
    (if (ask-user-about-saving)
        (save-file)))

;; However, since shortcut Boolean evaluation is guaranteed to be used for Boolean operations under Common Lisp and most other Lisp dialects, we could write this instead:
(and *file-modified* (ask-user-about-saving) (save-file))

;; A third way to write this code, which is a compromise between the previous approaches, is as follows:
(if (and *file-modified*
        (ask-user-about-saving))
    (save-file)))
;; Many experienced Lispers will consider this version a bit clearer than the previous two versions,
;; because only expressions that are expressly designed to return a Boolean value are treated as part of the condition.


;; Using Functions That Return More than Just the Truth

;; As we’ve already discussed, any value in Common Lisp (except for the different variations on nil) is true.
;; This means that functions that are commonly used in conditions have the option of returning more than just the truth.

;; For instance, the Lisp command member can be used to check for list membership for an item:

(if (member 1 '(3 4 1 5))
    'one-is-in-the-list
    'one-is-not-in-the-list) ;; => 'ONE-IS-IN-THE-LIST

(member 1 '(3 4 1 5)) ;; => (1 5)

;; Whenever a Lisper writes a function that returns true and false,
;; she will think to herself, “Is there anything else I could return other than just t?”

;; The implementers of the member function decided that
;; some crazy Lisper somewhere may see the value in having the tail of the list for some calculation that uses this function.

;; One function that really benefits from rich return values is find-if, as follows:
(find-if #'oddp '(2 4 5 6)) ;; => 5

(if (find-if #'oddp '(2 4 5 6))
      'there-is-an-odd-number
      'there-is-no-odd-number) ;; => 'there-is-an-odd-number

;; If we try our edge case again, searching for a nil value, we get a rather disappointing result:
(find-if #'null '(2 4 nil 6)) ;; => NIL
;; The null function, which returns true for any of the nil values, correctly finds the nil.
;; Unfortunately, in this one annoying case, we would not want to use find-if inside a conditional statement,
;; because a correctly found value still returns a result that evaluates as false.
