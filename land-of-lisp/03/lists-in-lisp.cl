;; Lists are a crucial feature in Lisp. They are what hold all your Lisp code (as well as data) together.
(expt 2 3)
;; This piece of code contains a symbol (expt) and two numbers, all tied together as a list, indicated by the parentheses.

;; Cons Cells
;; Lists in Lisp are held together with cons cells.
;; Understanding the relationship between cons cells and lists will give you a better idea of how Lisp works.

;; A cons cell looks like this: land-of-lisp/03/httpatomoreillycomsourcenostarchimages781418.png

;; It’s made of two little connected boxes, both of which can point at other things.
;; In fact, lists in Lisp are just an abstract illusion—all of them are actually composed of cons cells.

;; For instance, suppose we create the list '(1 2 3). Here’s how this list is represented in computer memory:
;; land-of-lisp/03/httpatomoreillycomsourcenostarchimages781207.png


;; List Functions
;; Manipulating lists is extremely important in Lisp programming.
;; There are three basic functions for manipulating cons cells (and hence lists) in Lisp: cons, car, and cdr.

;; The cons Function
;; If you want to link any two pieces of data in your Lisp program (regardless of type), the usual way to do that is with the cons function.
;; For example, let’s link the symbol chicken to the symbol cat:
(cons 'chicken 'cat)
;; > (CHICKEN . CAT)
;; Don’t confuse this with a regular list. 
;; The dot in the middle makes this a cons cell, just linking those two items together.

;; If instead of another piece of data, we attach the symbol nil on the right side of the list, something special happens:
(cons 'chicken ())
;; > (CHICKEN)

;; Unlike with our cat, the nil does not show in the output this time.
;; There’s a simple reason for this: nil is a special symbol that is used to terminate a list in Lisp.

;; The lesson here is that Lisp will always go out of its way to “hide” the cons cells from you.
;; It will show a cons cell (with the dot between the objects) only if there isn’t a way to show your result using lists.

;; The previous example can also be written like this:
(cons 'chicken ())
;; > (CHICKEN)
;; The empty list, (), can be used interchangeably with the nil symbol in Common Lisp.

;; The cons function also can add a new item to the front of the list.
;; For example, to add pork to the front of a list containing (beef chicken), use cons like so:
(cons 'pork '(beef chicken))
;; > (PORK BEEF CHICKEN)

;; Since all lists are made of cons cells, our (beef chicken) list must have been created from its own two cons cells, perhaps like this:
(cons 'beef (cons 'chicken ()))
;; > (BEEF CHICKEN)

;; Combining the previous two examples, we can see what all the lists look like when viewed as conses. This is what is really happening:
(cons 'pork (cons 'beef (cons 'chicken ())))
;; > (PORK BEEF CHICKEN)

;; In Lisp, a chain of cons cells and a list are exactly the same thing.


;; The car and cdr Functions

;; The car function is used for getting the thing out of the first slot of a cell:
(car '(pork beef chicken))
;; > PORK

;; The cdr function is used to grab the value out of the second slot, or the remainder of a list:
(cdr '(pork beef chicken))
;; > (BEEF CHICKEN)

;; You can string together car and cdr into new functions like cadr, cdar, or cadadr.

;; Entering cadr is the same as using car and cdr together—it returns the second item from a list. 
(cdr '(pork beef chicken))
;; > (BEEF CHICKEN)
(car '(beef chicken))
;; > BEEF
(car (cdr '(pork beef chicken)))
;; > BEEF
(cadr '(pork beef chicken))
;; > BEEF


;; The list Function
;; For convenience, Common Lisp has many functions built on top of the basic three—cons, car, and cdr.

;; A useful one is the list function, which does the dirty work of creating all the cons cells and builds our list all at once:
(list 'pork 'beef 'chicken)
;; > (PORK BEEF CHICKEN)

;; Remember that there is no difference between a list created with the list function, one created by specifying individual cons cells, or one created in data mode using the single quote.
;; They’re all the same animal.


;; Nested Lists
;; Lists can contain other lists. Here’s an example:
'(cat (duck bat) ant)

;; 
(cdar '((peas carrots tomatoes) (pork beef chicken)))
;; > (CARROTS TOMATOES)