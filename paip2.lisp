;;PAIP LISP
;;Chapter 2
;;https://github.com/norvig/paip-lisp/blob/master/docs/chapter2.md

;;Text Generator Program

;;First make the function to build the sentences based on sentence rules
(defun sentence () (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (article) (noun)))
(defun verb-phrase () (append (verb) (noun-phrase)))
(defun Article () (one-of '(the a)))
(defun Noun () (one-of '(Michael ball dog Vienna)))
(defun verb () (one-of '(Love Like Poke Kiss)))

;;Picks one element of set, and make a list of it
(defun one-of (set)
  "Pick one element from set and make it a list"
  (list (random-elt set)))

;;Randomly chooses an element from a list
(defun random-elt (choices)
  "Choose an element from a list at random"
  (elt choices (random (length choices))))

;;Elt picks an element out of a list based on the index number
;;Random choices a number between 0 and the n-1 you provide

;;Consider more Complex Rules Such as Adjectives




