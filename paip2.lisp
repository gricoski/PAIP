;;PAIP LISP
;;Chapter 2
;;https://github.com/norvig/paip-lisp/blob/master/docs/chapter2.md

;;to save git in cmd press esc then type :wq

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
  "Generate a random sentence or phrased"
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
;;Thier Answer
(defun generate3 (phrase)
  "Generate random sentences without useing rewrites function"
  (cond ((listp phrase)
         (mappend #'generate3 phrase))
        ((setf choices (rewrites phrase))
         (generate (random-elt choices)))
        (t (list phrase))))

;;Section 2.5

;;Approach 2 of using the most natural notation to solve the problem
(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (article adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (article -> the a)
    (name -> pat kim lee terry robin)
    (noun -> man ball woman table)
    (verb -> hit took saw liked)
    (pronoun -> he she it these those that)))

;;Update grammar variable to refer to t his new grammar list
(setf *grammar* *bigger-grammar*)

;;Section 2.6

;;ONE DATE MULTIPLE PROGRAM APPROACH (APPROACH #2) (Apprach #1 is direct lisp code)

;;Advantage of representing info in a declarative form as rules
;;We make one function to reuse the words in a new way

;;Generate random sentence structure
(defun generate-tree (phrase)
  "Generate a random sentence or phrase, with
a complete parse tree."
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))))
        (t (list phrase))))

;;Generate all posibilites of phrase
(defun generate-all (phrase)
  "Generate a list of all possible expansions of this phrase."
  (cond ((null phrase) (list nil))
        ((listp phrase)
         (combine-all (generate-all (first phrase))
                      (generate-all (rest phrase))))
        ((rewrites phrase)
         (mappend #'generate-all (rewrites phrase)))
        (t (list (list phrase)))))

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x.
e.g., (combine-all '((a) (b)) '((1) (2)))
-> ((A 1) (B 1) (A 2) (B 2))."
  (mappend #' (lambda (y)
                (mapcar #'(lambda (x) (append x y)) xlist)) ylist))

;;Note
;;Generate-all does not do recursive grammar as 'Adj* -> Adj + Adj* would lead to infinite possibilites

;;Therefore we have to use the old grammar

(setf *grammar* *simple-grammar*)

(length (generate-all 'sentence)) ;->256

;;Redo Exercises - Learn rewrites function

;;Exercise
;;A version of generate that avoids using rewrite twice

(defun generate4 (phrase)
  (cond ((listp phrase);Starting the conditional function
         (mapend3 '#generate4 phrase));This line allows the function to run on all elements of phrase if there are more than one.  
        (())))

;;Relearning basics of Chapter
(defun mapend (fn list)
  (cond ((endp list) nil)
        (t (append (apply fn (car list))
                   (apply fn (rest list))))))
;;This one doesn't work, buecase apply needs a list or something

(defun mapend2 (fn list)
  (cond ((endp list) nil)
        (t (append (funcall fn (car list))
                   (funcall fn (cdr list))))))
;;This one does not work like the real mappend in there is an extra ()

(defun mapend3 (fn list)
  (cond ((endp list) nil)
        (t (append (funcall fn (car list))
                   (mapend3 fn (cdr list))))))
;This seems to work the same as mappend, but with a cond instead of an if


;;Testing Assoc
(defparameter *Operators-Year1*
  '(
    (SAS Thacter Sledge Mute Smoke)
    (GSG9 Bandit IQ Blitz Yeager)
    (FBI Thermite Ash Castle Pules)
    (GIGN Montange Doc Rook Twitch)
    (SPETSNAZ Glaz Fuze Kapkan Tachanka)))

(defvar *Operator* *Operators-Year1*)

(defun rewrites2 (agency)
  "Picks the operator"
  (rest (assoc agency *operator* )))

(defun random-elt2 (choice)
  "randomly choices a choice by first finding a lsit then picks a number randomly ccorresponding to that list and then picks then uses that random number as an index to pick one of the elements out of the list"
  (elt choice (random (length choice))))
