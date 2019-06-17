;;PAIP LISP
;;Chapter 2
;;https://github.com/norvig/paip-lisp/blob/master/docs/chapter2.md

;;Text Generator Program

;;First make the function to build the sentences based on sentence rules
(defun sentence () (append (noun-phrase) (verb-phrase)))
;(defun noun-phrase () (append (article) (noun)))
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

(defun Adj* ()
  (if (= (random 2) 0)
      nil
      (append (Adj) (Adj*))))

(defun PP* ()
  (if (random-elt '(t nil))
      (append (PP) (PP*))
      nil))

(defun noun-phrase () (append (Article) (Adj*) (Noun) (PP*)))
(defun PP () (append (Prep) (noun-phrase)))
(defun Adj () (one-of '(big little blue green adiabatic)))
(defun Prep () (one-of '(to in by with on)))

;;Creating the program in a different way
(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (article Noun))
    (verb-phrase -> (verb noun-phrase))
    (article -> the a)
    (Noun -> man ball woman table)
    (verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defvar *grammar* *simple-grammar*
  "The grammar used by generate. Initially, this is
*simple-grammar*, but we can switch to other grammars.")

;;assoc returns the first element of the list of lists that start with a key (ex. noun)
(assoc 'noun *grammar*)

;;Grammar rules are implemented as lists, but better to use functions to operation on parameter
(defun rule-lhs (rule)
  "The left h and side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right hand side of a rule"
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))

;;Creating mappend from section 1 as it used above
(defun mappend (fn the-list)
  "apply fn to each element of list and
append the results"
  (if (null the-list)
      nil
      (append (funcall fn (first the-list))
              (mappend fn (rest the-list)))))

;;Run the program with
(generate 'sentence)
(generate 'sentence)
(generate 'noun-phrase)
(generate 'verb-phrase)

;;Another version of generate made with if instead of cond
(defun generate2 (phrase)
  "Generate a random sentence or phrase"
  (if (listp phrase)
      (mappend #'generate2 phrase)
      (let ((choices (rewrites phrase)))
        (if (null choices)
            (list phrase)
            (generate2 (random-elt choices))))))

;;tip use let instead of setf as it keeps your mess it clean

;;Exercise 2.1

;;Generate remade with cond but without using rewrites
(defun generate3 (phrase)
  "Generate random sentences without useing rewrites function"
  (cond ((listp phrase)
         (mappend #'generate3 phrase))
        ((lambda (phrase) #'))));Maybe use let and category?
