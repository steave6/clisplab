;; Describing the Paths

;; Now that we have basic descriptions of each location, we need descriptions of paths to other locations as well.
;; We’ll create a second variable, *edges*, that contains the paths that players can take to move between places on our map.

(defparameter *edges* '((living-room (garden west door)
                                     (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

;; Using this structure, we create the describe-path function,
;; which builds a textual description of a given edge using our symbols system.
(defun describe-path (edge)
    `(there is a ,(caddr edge) going ,(cadr edge) from here.))


;; How Quasiquoting Works

;; Both the single quote and backquote in Lisp “flip” a piece of code into data mode,
;; but only a backquote can also be unquoted using the comma character, to flip back into code mode.


;; Describing Multiple Paths at Once
(defun describe-paths (location edges)
    (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;; The describe-paths function takes the following steps:
    ;; Find the relevant edges.
    ;; Convert the edges to descriptions.
    ;; Join the descriptions.

;; Finding the Relevant Edges
(cdr (assoc 'living-room *edges*)) ;; => ((GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER))

;; Converting the Edges to Descriptions
(mapcar #'describe-path '((GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER))) ;; => ((THERE IS A DOOR GOING WEST FROM HERE.)
;;   (THERE IS A LADDER GOING UPSTAIRS FROM HERE.))

;; The mapcar function is used frequently by Lispers. This function takes another function and a list,
;; and then applies this function to every member of a list. Here’s an example:
(mapcar #'sqrt '(1 2 3 4 5)) ;; => (1 1.4142135 1.7320508 2 2.236068)

;; You may be wondering why the function names we pass into mapcar have the #' symbols in front of them.
;; This symbol sequence is a shorthand for the function operator.
(mapcar (function car) '((foo bar) (baz qux))) ;; => (foo baz)
(mapcar (function sqrt) '(1 2 3 4 5)) ;; => (1 1.4142135 1.7320508 2 2.236068)


;; Joining the Descriptions

;; Once we’ve used mapcar to generate a list of descriptions for all the paths and edges, we need to combine them into a single description.
;; We accomplish this with the append function, which joins several lists into one big list:
(append '(mary had) '(a) '(little lamb)) ;; => (MARY HAD A LITTLE LAMB)

;; The apply function pretends that the items in the list are separate objects and passes them to the given function as such.
(apply #'append '((mary had) (a) (little lamb))) ;; => (MARY HAD A LITTLE LAMB)

