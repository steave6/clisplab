;; Describing the Location

;; Now that we’ve created an alist of our game world, we need to create a command to describe a location.
;; To accomplish this, we’ll use the assoc function to find the correct item in the list using a key:

(assoc 'garden *nodes*) ;; => (GARDEN (YOU ARE IN A BEAUTIFUL GARDEN. THERE IS A WELL IN FRONT OF YOU.))

;; Using assoc, we can easily create the describe-location function:

(defun describe-location (location nodes)
   (cadr (assoc location nodes)))

;; To use this function, we pass in a location and the *nodes* list:
(describe-location 'living-room *nodes*) ;; => (YOU ARE IN THE LIVING-ROOM. A WIZARD IS SNORING LOUDLY ON THE COUCH.)

;; Why don’t we just reference the *nodes* variable directly from the describe-location function?
;; Because this function is written in the functional programming style.

;; By writing functions that don’t reference variables in the “outside world” directly and that don’t perform any actions other than returning a value,
;; you can write code that can easily be tested in isolation.
