;; (load #p"./global-variable.cl")

;; When you set the value of a global variable using defparameter, 
;; any value previously stored in the variable will be overwritten:
(defparameter *foo* 5)
(defparameter *foo* 6)
;; execute in command line > *foo*

;; defvar won’t overwrite previous values of a global variable:
(defvar *bar* 5)
(defvar *bar* 6)
;; execute in command line > *bar*
