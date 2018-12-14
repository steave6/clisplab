;; Describing the Scenery with an Association List

;; Let’s first create a top-level variable, *nodes*, to contain descriptions of the locations that exist in our game:
(defparameter *nodes* '((living-room (you are in the living-room.
                            a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                            there is a well in front of you.))
                        (attic (you are in the attic.
                            there is a giant welding torch in the corner.))))

;; it does not actually contain any text strings. Since Common Lisp has a string datatype, we could have written descriptions using quotes.
;; Why wouldn’t we just use strings? As I mentioned at the beginning of this chapter, the manipulation of text is not really a fundamental computing concept.

;; Since the easiest things to manipulate in Lisp are symbols and lists,
;; most experienced Lispers will try to focus on these datatypes in the design of their software whenever possible.
