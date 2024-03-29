d;;PAIP3
;;Overview of Lisp

;;Be specific
;;Use Abstractions
;;Be concise
;;Use the provided tools.
;;Don't be Obscure
;;Be Consistent

;;If you define a list of names, then you should define functions
;;to get at the components of each name

;;Create functions
(defun)

;;Create Macros
(defmacro) ;Pg 66

;;Special Variables
(defvar) ;defines special variable may have optional value
(defparameter) ; Similar to defvar, but requires value and is used to change any existing values
(defconstant) ; declares symbol that will always stand for a value

;;All def forms declare global variables
;;Let defines local variables and label


;;Defstruct is used group related data together into a structure
;;Defines structure type
;;Automatically defines functions to get at components of structure

(defstruct name
  "exampe of defstruct"
  first
  (middle nil)
  last)
;;This automatically makes functions
(make-name)
(name-p)
(name-first)
(name-middle)
(name-last)

;;Use case of this
(setf b (make-name :first 'Barney :last 'Rubble))
                                        ;Got an error but it worked, you can access with those automatically made functions

(name-first b)
(name-middle b)
(name-last b)
(name-p b)
(name-p 'Barney)

;;to add middle name that was intially nil by default
(setf (name-middle b) 'Q)

;;b => #S(NAME :FIRST BARNEY :MIDDLE Q :LAST RUBBLE)
;;Structures are implemented as vectors
;;This means structures are more efficient than lists, take up less space, and any element can be accessed in a single step.  Where as lists take n steps to access nth element.

;;Next Section is Special Forms for Conditionals

;;Nil is the only False value when it comes to conditionals like "IF"

;;(cond) clauses
;;As soon as a test expression evalutes to non-nil it returns it's
;;Results expression.  If no REsults expression it returns the value
;;of the test expresssion
;;If all test expressions evalute to nill, then NIL


;;(when) and (Unless)
;;Have only one test expression, but unliminted result expressions

;;(or) and (and)

(and (> n 100)
     (princ "N is large"))

(or (<= n 100)
    (princ "N is large."))

(cond ((> n 100)
       (princ "N is large.")))

(when (> n 100)
  (princ "N is large."))


;;Use cond and if to return values
;;Use when and unless when there is only one possiblity

(defun tax-bracket (income)
  "Determine what percent tax should be paid for this income"
  (cond ((< income 10000.00) 0.00)
        ((< income 30000.00) 0.20)
        ((< income 50000.00) 0.25)
        ((< income 70000.00) 0.30)
        (t 0.35))); no parathesis needed at the end.

;;Examples of Case and their Cond Equivalant
;;Not ont he list is ccase and ctypcase that allow you to enter a new value if error

;CASE and Cond Equivilant
(case x
  (1 10)
  (2 20))

(cond
  ((eql x 1) 10)
  ((eql x 2) 20))

;;Examples to REPL
(funcall #'(lambda (x) (case x
                         (1 10)
                         (2 20)))2)

(funcall #'(lambda (x) (cond ((eql x 1) 10)
                             ((eql x 2) 20))) 1)

;;Or a regular function example
(defun mycase (x)
  "Example of using case.  Pick 1 or 2"
  (case x
    (1 10)
    (2 10)))

;;Creating Tax-Bracket Function with Case
(defun mytax (x)
  "Example of Tax Bracket Function"
  (case
      ((< x 10000.00) 0.00)
    ((> x 10000.00) 0.20)))
;Does not work.

;;Typecase and Cond equivalant
(typecase x
  (number (abs x))
  (list (length x)))

(cond
  ((typep x 'number) (abs x))
  ((typep x 'list) (length x)))

(defun mytype (x)
  (typecase x
    (number (abs x))
    (list (length x))
    (atom x)))

;;ecase and cond equivalent
(ecase x
  (1 10)
  (2 20))

(cond
  ((eql x 1) 10)
  ((eql x 2) 20)
  (t (error "no valid case")))

(funcall #'(lambda (arg) (ecase arg
                           (1 10)
                           (2 20))) 3)
;;Gives Error

(funcall #'(lambda (arg) (ecase arg
                           (1 10)
                           (2 20))) 1)
;;Works

(defun myecase (x)
  (ecase x
    (a 'apple)
    (b 'ball)))
;;If you pick c it says this:
;;"C fell through ECASE expression. Wanted one of (A B)."

(defun myconde (x)
  (cond ((eql x 'a) 'apple)
        ((eql x 'b) 'ball)
        (t (error "Choose a or b dummy"))))
;;If you pick c it says choose a or b dummy

;;etypecase and cond equivilant
(etypecase x
  (number (abs x))
  (list (length x)))

(cond
  ((typep x 'number) (abs x))
  ((typep x 'list) (length x))
  (t (error "no valid typecase")))

(defun myEtypecase (x)
  (etypecase x
    (number (float x))
    (list (length x))))

;;Special Forms for Dealing with Variables and Places

;;setf assign a new value to a variable or place
;;setf is like = or := in other languages

;;Examples
(setf x 0)
(setf (aref A i j) 0)
((setf (rest list) nil))
(setf (name-middle b) 'Q)

 ;;A place or generalized variable is a name for a location that
 ;;can have a value stored in it

 ;;You can extrend expressions allowed in setf with
 ;; defsetf (page 514) and define-setf-method (page 884)

;;rplcd is similar to setf except some differences
(rplacd list nil) ;returns list
(setf (rest list) nil) ;returns nil
;;Most just use Setf

;;Very common for new variables to be established
;;but they never change

;;New Variables are introduced as parameters in functions
;;Or introduce new variables in functions with let

;;Examples

(let ((x 40) (y (+ 1 2)))
  (+ x y))

;;Defining local variables is the same as defining parameters to a anon function
((lambda (x y)
   (+ x y))
 40
 (+ 1 1))

;; let* is appropiate when you want to use one ofthe newly introduced variables
;;in a subsequent value

(let* ((x 6) (y (* x x)))
  (+ x y))
;;If we used regular let instead of let* then y's value would not be what we want
;;As x would not have had it's value already assigned if we only used let

;;Exercise 3.1
(funcall #'(lambda (y) (* y y)) 3)
;;Args must be in paranthesis

(funcall #'(lambda (x y) (+ x y)) 10 15)

(funcall #'(lambda (x y) (+ x y)) 5 (lambda (x) (* x x)))
;;Does not work

(funcall #'(lambda (x y) (+ x y)) 5 ((lambda (x) (* x x)) 5))
;;Works, but...

(funcall #'(lambda (x y) (+ x y)) 5 ((lambda (x) (* x x)) x))
;;Does not work, x unbound

(funcall #'(lambda ((lambda (x) x) 5) (lambda (x) (* x x)) x))
;;Trash

(funcall #'(lambda (x y) (+ x y)) 5 5)
;;Works

(funcall #'(lambda (x y) (+ x y)) 5 ((* x x)))
;;Does not work

(funcall #'(lambda (x y) (+ x y)) ((lambda (x) (* x x)) 5) 5)
;;Works, but...

(funcall #'(lambda (x y) (+ x y)) ((lambda (x) (* x x) x) 5))
;;Does not work

(funcall #'(lambda (x y) (+ x y))
         ((lambda (x) (list (* x x) x)) 5))
;;did not work

(funcall #'(lambda (x y) (+ x y))
         ((lambda (x) (result (* x x)) (result x)) 5))
;;Does not work
;; Going to move on for now

;;Push - will change the first element of a list
(push x list)
;;Example
(funcall #'(lambda (x lit) (push x lit)) 'a '(1 2 3))
;;Equivalent code
(setf list (cons x list))

;;Pop - Returns and Removes first element of a list
(pop list)
;;Example
(funcall #'(lambda (lit) (pop lit)) '(1 2 3))
;;Equivalent code
(let ((result (first list)))
  (setf list (rest list))
  result)

;;incf - increments a value by one
(incf x)
;;decf - decreases a value by one
(decf x)
;;Note value should be a symbol or location

;;Optionally you can increment or decuase by more by adding a number args
(incf x 14)
;;Will increase value by 14
(decf x 5)
;;Will decrease value by 5

;;Create a program that determines who wins based on a player and score

(defstruct player (score 0) (wins 0))

(defun determine-winner (players)
  "increment the wins for the player with the highest score."
  (incf (player-wins (first (sort players #'>
                                  :key #'player-score)))))

(defun determine-winner (players)
  "Increment the wins for the player with the highest score."
  (let ((temp (first (sort players #'> :key #'player-score))))
    (setf (player-wins temp) (+ (player-wins temp) 1))))

;;Next Section is going over Loops
;;List of Loops to Cover
;;Dolist - loop over elements of a list
;;Dotimes - loop over successive integers
;;Do or do* - general loop, sparse syntax
;;loop - general loop, verbose syntax (seem elm)
;;mapc or mapcar - loop over elements of lists
;;Some or every - loop over list until conditional
;;find or reduce - more specific looping functions
;;RECURSION - General Repetition

;;Examples of each recreating the length function

;;Dolist length Example
(defun dolist-length (list)
  "calculates the length yo"
  (let ((len 0))
    (dolist (element list)
      (incf len))
    len))

;;Same example but using the optional result of do list
(defun dolist2-length (list)
  (let ((len 0))
    (dolist (element list len)
      (incf len))))

;;Length using mapc
(defun mapc-length (list)
  (let ((len 0))
    (mapc #'(lambda (element)
              (incf len))
          list)
    len))

;;Mapc is like mapcar except mapcar returns the result in a list
(defun mapcar-length (list)
  (let ((len 0))
    (mapcar #'(lambda (element)
                (incf len))
            list)
    len))
;;Result was not a list in this situation

;;dotimes example (can't be a length)
(defun dotimes1 (x)
  "prints a word 3 times"
  (dotimes (x 3)
    (* 2 2))
  )
;;Did not work

;;Do form:
(do ((variable initial next)...)
    (exit-test results)
  body...)

;;Do - Loop repeats until exit-test is true
(defun do-length (list)
  (do ((len 0) (+ len 1)) 
      (l list (rest l)))
  ((null 1) len))
;;Very unspecific

;;Loop - alien syntax for lisp

(defun loop1-length (list)
  (loop for element in list
        count t))

(defun loop2-length (list)
  (loop for element in list
        summing 1))

(defun loop3-length (list)
  (loop with len = 0
        until (null list)
        for element = (pop list)
        do (incf len)
        finally (return len)))

;;Count-if - counts number of elements satisfyuing a predicate

;;MyTry
(defun mycountif (x)
  "returns the count of numbers in the
first level of a list"
  (count-if #'numberp X))
;;Does not work with string, list, qouted list
;;Saw an example of below using oddp, I tried numberp


;;Their Example
(defun length-countif (list)
  (count-if #'True list))

(defun true (x) t)

;;Position-if - finds position of an element that satisfies predicate
(defun mypositionif (list)
  (if (null list)
      0
      (+ 1 (position-if #'true list :from-end t))))

;;Idioms - things to use over and over as needed

;;Makes numbers negativbe
(mapcar #'- '(1 2 3))
;;Matrix adding below returns (11 22)
(mapcar #'+ '(1 2) '(10 20))
;;Matrix adding more lists still returns still the same on lies
(mapcar #'+ '(1 2) '(10 20) '(100 200))
;;My try of matrix multiplication
(mapcar #'* '(2 2) '(3 5))
;;WORKED!!
;;This can be used to multiple inputs by their weights in neural network or perceptron

;;remove an element from a list
(remove 1 '(1 2 3 2 1 0 -1))

;;Use a key to set paremeters
(remove 1 '(1 2 3 2 1 0 -1) :key #'abs)
;;Removes all 1s even negative

;;removes element if it passess a test
(remove 1 '(1 2 3 2 1 0 -1) :test #'<)
;;removes all numbers greater than 1

;;Set a start postion for when things will be removed
(remove 1 '(1 2 3 2 1 0 -1) : start 4)
;;Removes only the 1 before the 0

;;Remove-if similiar to remove but uses a predicate instaed of an element to match
(remove-if #'oddp '(1 2 3 2 1 0 -1))
;;Removes only the odd numbers

(remove-if-not #'oddp '(1 2 3 2 1 0 -1))
;;Removes only the even numbers

(find-if #'evenp '(1 2 3 2 1 0 -1))
;;Returns the first even number

(setf x '(a b c))
(setf y '(1 2 3))

;;Functions that work on variables above
(every #'oddp y)
;;Test if every element matches a predicate or satisfies a predicate
(some #'oddp y)
;;Test if SOME elements matche or satsify a predicate
(mapcar #'- y)
;;Applies a function to each element and return resutls
(mapc #'print y)
;;performs operation on each elment

;;Following all have -if and -if-not versions, and accept keyword Arguments

(member 2 '(1 2 3))
;;find element and returns it and everything else after it

(count 'b x)
;;Counts the number of matching elements

(delete 1 y)
;;removes that element from that list for that given moment does not actually delete

(find 2 y)
;;Finds first element that matches criteria and returns it

(position 'a x)
;;Returns numerical position of element in list

(reduce #'+ y)
;;applies function to successive elemtents
;;Could work for PERCEPTRON!!

(remove 2 y)
;;Like delete but makes a new copy??  Have not seen difference from delete yet

(substitute 4 2 y)
;;Replace the second element (2) with the first element (4) in list (y)

;;Repition through Recursion
(defun recurlength (list)
  (if (null list)
      0
      (+ 1 (recurlength (rest list)))))

(defun 2forV (x)
  "replaces the number 2 with v in the first level of a list"
  (if (equal (first x) '2)
      (cons 'V (2forV (rest x)))
      (cons (first x) (2forV (rest x)))))
;;Infinite Loop: ERROR

(defun 2forvCONS (x)
  "replaces any 2 with v, first level using cons"
  (cond ((null x) nil)
        ((equal (first x)
                '2)
         (cons 'V (2forVCONS (rest x))))
        (t (cons (first x) (2forvcons (rest x))))))
;;WORKS!!

(defun 2forv2 (x)
  "replaces the number 2 with v
 in first level using nested if statement"
  (if (equal (first x) '2)
      (cons 'V (2forv2 (rest x)))
      (if (null x)
          nil
          (cons (first x) (2forv2 (rest x))))))
;;WORKS!!

(defun 2forv3 (x)
  "replaces the number 2 with v
 in first level using nested if statement"
  (if (equal (first x) '2)
      (cons 'V (2forv2 (rest x)))
      (or
          nil
          (cons (first x) (2forv2 (rest x))))))
;;WORKS!! But, WHY??
;;As it goes deeper into recursion, the or statement goes with the first true
;;Is the rest of x nil? if no then look at the rest
;;Then once it gets to the end, is returning element nil?

;;The recurcsive length uses more memory, but recursion doesn't always.. Like these

(defun length-recur2 (list)
  (length-recur2-aux list 0))

(defun length-recur2-aux (sublist len-so-far)
  (if (null sublist)
      len-so-far
      (length-recur2-aux (rest sublist) (+ 1 len so far))))

;;Making the function above clearner by embedding the function within

(defun length-recur3 (list &optional (len-so-far 0))
  (if (null list)
      len-so-far
      (lenght-recur3 (rest list) (+1 len-so-far))))

;;Another Version

(defun length-recur4 (the-list)
  (labels
      ((length-recur4 (list len-so-far)
         (if (null list)
             len-so-far
             (length-recur4 (rest list) (+ 1 len-so-far))))
       (length-recur4 the-list 0))))

;;Labels or flet are used to introduce one or more local functions

(labels ((function-name (parameter..) function body) ...)
  body-of-lables)

;;Other special forms


;;Progn can be used to evaluate a sequence of forms and return the value of the last one
(progn (setf x 0) (setf x (+ x 1)) x)
;;returns 1

;;Progn Samples
(if (> x 100)
    (progn (print "too big")
           (setf x 100))
    x)

;;This could also work
(cond ((> x 100)
       (print "too big")
       (setf x 100))
      (t x))

;;Writing this one just to get to Macros as next section
(defun product (numbers)
  "Multiply all the numbers together to compute their product."
  (let ((prod 1))
    (dolist (n numbers prod) ;Hate weird loops in lisp
      (if (= n 0)
          (return 0)
          (setf prod (* n prod)))
      )))

;;My simple version of this with controls
(defun myprod (liste)
  "multiply all numbers in list"
  (cond ((endp liste) 1) ;Tried with nil got error
        ((zerop (first liste)) 0)
        ((numberp (first liste))
         (* (first liste) (myprod (rest liste))))
        (t (myprod (rest liste))))) ;Atoms still giving error, but all numbers work!

;;My simple version Second Try to control for letters
(defun myprod2 (liste)
  "multiply all numbers in list"
  (cond ((endp liste) 1) ;Tried with nil got error
        ((zerop (first liste)) 0)
        ((numberp (first liste))
         (* (first liste) (myprod2 (rest liste))))
        (t (or
            (myprod2 (first liste))
            (myprod2 (rest liste)))))) ;Atoms still giving error, but all numbers work!

(defun myprod3 (liste)
  "Multiply numbers no letters"
  (cond ((null liste) 1)
        ((numberp (first liste))
         (* (first liste) (myprod3 (rest liste))))
        (t (myprod3 (rest liste)))))
;;WORKS!!! - Had to get rid of zerop.  Tried a few other things, but need to remember
;;Test the simple things first then go deeper!!

;;MACROS

;;Is it required.. It's writing a new lisp that has your macro...

;;EXAMPLE

(defmacro while (test &rest body)
  "Repeat body while test is treu."
  (list* 'loop
         (list 'unless test '(return nil))
         body))

;;list* is like list except it takes the last argument and appends it to the end.

;;Use Macroexpand to see what it expands into
(macroexpand-1 '(while (< i 10)
                 (print (* i i))
                 (setf i (+ i 1))))

setf i 7
(while (< i 10)
       (print (* i i))
       (setf i (+ i 1)))
;;I don't get this yet... Keep looking

;;Backqoute Notation

;;Building the code that is the expansion of the macro...
;;Using Subst (subst new old tree) substitues new for each occurence of old anywhere within tree
(defmacro while (test &rest body)
  "Repeated body while test is true"
  (let ((code '(loop (unless test (return nil)) . body)))
    (subst test 'test (subst body 'body code))))

;;Using backqoute notation instead of subst 

(defmacro while (test &rest body)
  "Repeat body while test is true"
  `(loop (unless ,test (return nil))
         ,@body))

> (setf testl '(a test)) => (A TEST)

> '(this is ,test1) => (THIS IS (A TEST))

> '(this is ,@test1) => (THIS IS A TEST)

> '(this is . ,test1) => (THIS IS A TEST)

> '(this is ,@test1 -- this is only ,@testl) =>
(THIS IS A TEST -- THIS IS ONLY A TEST)

//Might get the other book or got to chap 4, getting bored...

;;Functions that operate on lists

(setf x '(a b c))
(setf y '(1 2 3))

(first x)
(second x)
(third x)
(nth 0 x) ;;0 can be changed to another number
(rest x) ;;Gives everything after first element
(car x) ;;Another name for first
(cdr x) ;;Another name for rest
(last x) ;;Gives last element
(length x)
(reverse x)
(cons 0 '(1 2 3))
(append x y)
(list x y)
(list* 1 2 x) ;;Adds last element to others
(null nil)  ;;Returns T
(listp x) ;;T, becuase x is the name of a list
(listp 3) ;;nil
(consp x) ;;T becuase x is a non-nil list
(consp nil) ;;nil it's not a non-nil list
(equal x x) ;;T
(equal x y) ;;nil
(sort y #'>) ;; Takes the list y (1 2 3) and returns (3 2 1)
(subseq x 1 2) ;;Returns b as it gives the subsequence between a start and a end

;;Lists are made out of cons cells with two fields
;;First
;;Rest

;;Write a function that writes in dotted piar notation
;;Use cons
(cons 'both_things_must_be 'atoms)

(defun ex1 (liste)
  "print an expression in dotted pair notation"
  (cond ((null liste) nil)
        ((atom (rest liste))
         (cons (first liste) (rest liste)))
        (t (or (cons (first liste) (ex1 (rest liste)))
               (cons (first liste) (ex1 (first (rest liste))))))))

;;Equality and Internal Representation
;;When lisp reads a symbol from 2 places its guaranteed to be the exact same
;;As lisp maintains a symbol table
;;Lists stored in seperate locations are not identical
;;As they are read using cons and lists are composed of different cons cells

(eq 'x 'x) ;t
(eq '0 '0) ;Depends on implementation
(eq '(x) '(x)) ;nil
(eq '"xy" '"xy") ;nil
(eq '"Xy" '"xY"); nil
(eq '0 '0.0) ;nil

(eql 'x 'x) ;T
(eql '0 '0) ;T
(eql '(x) '(x)) ;nil
(eql '"xy" '"xy") ;nil
(eql '"Xy" '"xY") ;nil
(eql '0 '0.0) ;nil
(eql '0 '1) ;nil

(equal 'x 'x) ;T
(equal '0 '0) ;T
(equal '(x) '(x)) ;T
(equal '"xy" '"xy") ;T
(equal '"Xy" '"xY") ;Nil
(equal '0 '0.0) ;Nil
(equal '0 '1) ;Nil

(equalp 'x 'x) ;T
(equalp '0 '0) ;T
(equalp '(x) '(x)) ;T
(equalp '"xy" '"xy") ;T
(equalp '"Xy" '"xY") ;T
(equalp '0 '0.0) ;T
(equalp '0 '1); Nil

;;Functions on Sequences

;;Lips added vector and strings so there are specific new functions that can work on these
(nth 0 '(1 2 3)) ; 1
(nth 0 '"This is a string") ; error
(nth 0 '#(1 2 3)) ; error as this is a vector and not a list


;;Elt works on everything
(elt '(1 2 3) 0) ;1
(elt '#(1 2 3) 0) ;1
(elt '"This is a string" 0) ;#\T

;;other sequences codes
(aref array n)
(char string n)
(bit vector n)
(sbit simple-bit vector n)
(svref simple-vector n)

;;TABLES

;;An associated list is a list used to implement tables
;;a list of dotted pairs with a key and a vlaue.

;;Creating a table
(setf newTable
      '((AL . Alabama) (PA . Pennsylvania) (CA . California) (IN . Indiana) (VN . Vietname)))

;;To serach by Key
(assoc 'PA newtable) ;returns (PA . Pennsylvania)

(cdr (assoc 'PA newtable)) ;returns Pennsylvania

;;To search by Value
(rassoc 'California newtable) ;returns (CA . California)

(car (rassoc 'California newtable)) ;returns CA

;;Assoc searches thourhg the whole list one element at a time
;;THIS CAN BE SLOW

;;Instead make a has table
(setf newtable2 (make-hash-table))

(setf (gethash 'LB newtable2) 'LongBeach)
(setf (gethash 'SF newtable2) 'Frisco)
(setf (gethash 'HB newtable2) 'Harrisburg)

(gethash 'LB newtable2) ;Returns LONGBEACH T
(gethash 'SF newtable2) ;Returns FRISCO T

(remhash) ; used to remove a key/value pair

(remhash 'HB newtable2)
(gethash 'HB newtable2) ;Returns NIL NIL

(maphash) ;can be used map over key value pairs

(clrhash) ;Clears the entire hashtable

(clrhash newtable2)
(gethash 'SF newtable2) ;Nil Nill

;;The other table is property lists
;;p-lists

(setf (get 'PA 'State) 'Pennsylvania)
(setf (get 'CA 'State) 'California)

(get 'CA 'state) ;returns califronia
(get 'pennsylvania 'state) ;Nil - there is no rassoc, you have to create a function if you want to pull abbreviations

(setf (get 'Pennsylvania 'abbrev) 'PA)
(get 'Pennsylvania 'abbrev) ; Returns PA


;;a-lists
(setf 'TX . 'Texas) ;Didn't WORK!!

;;Mimicking a-lists, put all properties under one LIST
(setf (symbol-plist 'Stater)
      '(AL Alabama AK Alaska AZ Arizona AR Arkansas))
(get 'stater 'AL) ;;Returns Alabama

;;Property lists are not used because of mess and no way to clrhash and stuff

;;FUNCTIONS on TREEs

((a b) ((c)) (d e))
;;Most functions treat this as 3 elements

;;Tree functions see it as 3 with 5 non-nill leaves

(setf approvedTREES '((a b) ((c)) (d e)))

(tree-equal approvedtrees (copy-tree approvedtrees)) ;Returns True

(defun same-shape-tree (a b)
  "Are two trees teh same except for the the leaves?"
  (tree-equal a b :test #'true))

(defun true (&rest ignore) t)

(same-shape-tree approvedTREEs '((1 2)) ((3)) (4 5))
;;Didn't Work... Fix

(same-shape-tree approvedtrees '((1 2) ((3)) (4 5)))
;;This works - Parathesis were off

;;Substituing Expressions

(subst 'new 'old '(old ((very old)) (new ((very new))))) ;;Setting things up for substitues

;;Substitute list
(sublis '((old . new)) '(old (very old)));; Returns (new (very new))

(subst 'new 'old 'old) ;;Returns NEW - Why?

(defun english->perot (words)
  (sublis '((are . va) (book . libre) (friend .ami)
            (hello . bonjour) (how . comment) (my . mon)
            (red . rouge) (you . tu))
          words))

(english->perot '(Hello my friend)) ;Returns (bonjour Mon (.AMI))
;;FIXING

(defun english->perot (words)
  (sublis '((are . va) (book . libre) (friend . ami)
            (hello . bonjour) (how . comment) (my . mon)
            (red . rouge) (you . tu))
          words))

(english->perot '(Hello my friend)) ;Returns (bonjour Mon Ami)
;;WORKS!!

;;Reread and see if you can see the structure better


;;Section 3.8 Functions on Numbers
(+ 7 7) ;Add
(- 21 7) ;Subtract
(* 2 7) ;Multiply
(/ 28 2) ;Divide
(> 100 99) ;T Greater Than
(< 99 100) ;T Less Than
(= 10 10) ;T
(random 100) ;Picks a random number from 0 to 99 - i got 92
(expt 3 2) ;Exponentiation (also exp, eX and log)
(sin pi) ;sine function
(asin 0) arcsine or sin-1 function (also acos, atan, etc.)
(min 2 3 4) ;Returns 2 the min number
(max 2 3 4) ;Returns 4 the max Number
(abs -3) ;Returns 3 the absolute value
(sqrt 4) ;2 the square root of 4
(rount 4.1) ;4
(rem 7 3) ;1 remainder or 1

;;Functions on Sets

;;First Set
(setf r '(a b c d))

;;Second Set
(setf s '(c d e))

;; Find what is similar between the first two sets
(intersection r s)

;; Find all elements in either of two sets
(union r s)

;;Find elements in one but not the other
(set-difference r s)

;;Check if an element is a member of a set and returns it and rest after
(member 'd s)

;;see if all elements of one set are in another
(subsetp s r)

;;add an element to a set
(adjoin 'b s)

;;but will not add duplicates
(adjoin 'c s)

;;You can also represent a set with a sequence of bits
;;11110 can be used to represent (a b c d)
;;00000 can be used to represent an empty set
;;11001 to represent (a b e)
;;You can also represent stuff like this as a bit vector
;;(a b e) would be #*11001 or the number 25 or #b11001
;;Dong things this way means less space and faster speed under certain conditions

;;Functions on intergers as bit vectors
(logand #b11110 #b11001)
(bit-and #*11110 #*11001)

;;Destructive functions
(setf x '(a b c))
(setf y '(1 2 3))

x ;;returns (a b c)
y ;;returns (1 2 3)

;;Then I do this function
(nconc x y) ;;Returns (A B C 1 2 3)

x ;;returns (a b c 1 2 3)
y ;;returns (1 2 3)

;;Values were appended together, but then the value of X was changed to appended version
;;Works by changeing changing the rest field of x to point to y
;;Conserves storage, but is destructive

;;Destructive functions begin with N, such as:

;;nreverse
(nreverse y) ;;Returns (3 2 1)
y ;;returns (1)


;;nintersection
(setf z '(1 2 3 b 4))
(setf y '(a b c 2))

(nintersection z y) ;;Returns (B 2)

z ;; Returns (1 2) - New Value
y ;; Returns (A B C 2) - Same value


;;nunion - Unions the data so all is included, but none repeated
(nunion z y) ;;returns (4 3 1 A B C 2)

z ;;returns (1 A B C 2)
y ;;returns (A B C 2)


;;nset-difference
(setf c '(1 2 3 A B C))
(setf d '(2 4 5 B D E))

(nset-difference c d) ;;Returns (C A 3 1) which are all the values that show in C, but not D

C ;; Returns (1)
D ;; Returns (2 4 5 B D E) - No Change

;;nsubst
(setf e '(1 2 3 A B C))
(setf f '(2 4 6 B D e))

(nsubst e f) ;;Error
(nsubst e f e) ;;Returns (1 2 3 A B C)
e ;;returns (1 2 3 A B C)
f ;;returns (2 4 6 B D E)

;;Come Back to Data Types seciont 3.11 as needed

;;3.12
;;Design your application to read lisp exppresions - IDEAL

;;Read

;;read-char

;;read-line

;;print
(print 'a) ;;Print any object in a new likne

;;prin1
(prin1 'a) ;;print any object with the new line and space

;;princ - human readable
(princ 'a) ;;prints the object with the qoutes - read can't recover original

;;Write - can behave link prin1 or princ depanding on parameters
(write 'a)

;;Reading, Writing to files...

;;Create the file test.text and print two expressions
;;Then open the file for reading and read back the frist expression, a single chartecer
;;Then two more expressions

(with-open-file (stream "test.text" :direction :output)
  (print '(I love you Vienna) stream)
  (princ 'Goodbye Stream))
;;Returns GOODBYE
;;If you run twice you get an error, that file exists
;;but in error menu you can choose to overwrite

(with-open-file (stream "test.text" :direction :input)
  (list (read stream) (read-char stream) (read stream)
        (read stream nil 'eof)))
;;Returns ((I Love you Vienna) #/G OODBYE EOF)

;;To Print New Lines
(terpri )
(fresh-line)

;;Formatting Output
(format t "I love my Wife!") ;;Format makes it formatted ;;T prints to terminal

;;Other options are instered with ~
(format t "~&~a Time for bed it is ~s ~f" "Two AM" "Now" 14)
;;Things get mixed about
(format t "~&~a plus ~s is ~f" "two" "two" 4) ;;Their example

;;~& moves to a fresh line
;;~a prints the next argument as princ
;;~s prints the next argument as prin1
;;~f prints a number as a float, or a letter a princ

;;Format always returns nil.  There are 26 different format directives

;;Complex example:
(let ((numbers '(1 2 3 4 5)))
  (format t "~&~{~r~^ plus ~} is ~@r"
          numbers (apply #'+ numbers)))

;;~r prints a number in english
(format t ~r '(14)) ;;Error r is not defined
(format t "~r" '(14)) ;;Err 14 not an integer
(format t "~r" '14) ;;Returns fourteen

;;~{ ... ~} takes the next argument (a list) and formats each element according to whats inside the {}
(format t ~{~f~^ is a float ~} is a float '(1 2 3)) ;;Error -f-^ is not defined... because i forgout ""
(format t "~{~f~^ is a float ~} is a float too" (1 2 3)) ;;Error because I forgot to qoute the list that {} acts upon
(format t "~{~f~^ is a float ~} is a float too" '(1 2 3)) ;;Returns 1.0 is a float 2.0 is a float 3.0 is a float too 
;;Format is like it's own langauge

;;Debugging Tools

;;Different Methods of Debugging problems in your program in lisp

;;1/3 - Insert print statements, recompile, try again to see where the problem exists
;;2/3 - Use a debugger to investigator and possibly alter the internal state of the running program
;;3/3 - Add annotations that are not part of the program, but affect the automatic runnning of the program
;;This is like the first one except you don't have to go back and remove print lines

;;Examples

(defun wife (hubbyArg)
  "Automatically corrects hubbyArg"
  (cond ((endp hubbyarg) nil)
        ((numberp (first hubbyArg))
         (princ "I don't care about your made up numbers"))
        (t (cons 'No (wife (rest hubbyArg))))))


(trace wife) - Turn on a print of every return
(untrace wife) - Turn off trace

;;Step

;;Their Example
(step (+ 3 4 (* 5 6 (/ 7 8)))) ;;Did not work

(step (wife '(Hello Honey)))  ;;Error panel came up and allowed me to continue the program or stop or only go one step forward

(apropos 'string) ;;prints info about all symbols whose name matches the argument
(apropos 'number)

Pick that function or any of them and see more information on it
(describe 'numberp) ;;Tells you about numberp

(documentation 'numberp 'function) ;;Gives you the symbols document string
(documentation 'wife 'function)
(documentation 'pi 'variable)

(inspect 'wife) ;;Look at and alter components of complex structures gives you lots of options to explore

;;The regular debugger happens when you have an error, you can force it with break

;;ANTI-Debugging tools - Check for errors and correct automatically like if-error

;;Error - invokes the error debug screen with the message your write

(defun average (numbers)
  (if (null numbers)
      (error "Average of the empty list is undefined.")
      (/ (reduce #'+ numbers)
         (length numbers))))

;;Cerror - is a continuable error.  User can signal to continue

(defun average2 (numbers)
  (if (null numbers)
      (progn
        (cerror "Use 0 as the average."
                "Average of the empty list is undefined.")
        0)
      (/ (reduce #'+ numbers)
         (length numbers))))
;;Define my own option of 0 to continue with using 0 as average

;;Ecase - exhastive case - gives an error if no cases matches
(defun myecase (tang)
  "Testing the ecase function"
  (ecase tang
    ((mot ) 1)
    ((hai ) 2)
    ((ba ) 3)))

;;Ccase - asks user for new input to continue
(defun decode (x)
  (ccase x
    ((i uno mot) 1)
    ((ii dos hai) 2)
    ((iii tres ba) 3)
    ((iv cuatro bon) 4)))
;;if you give a variable not listed it goes to error screen an there is an option to enter a new number in repl


;;Check-type - Checks the type of an argument and signals a continuable error if argument has wrong type
(defun tby14 (x)
  "Multiply x by 14"
  (check-type x number)
  (* x 14))

;;Assert tests an expression and signals an error if false - No continuation just Error
(defun t14v2 (x)
  "Multiply x by 14"
  (assert (numberp x))
  (* x 14))
;;Gives error if not number

;;An Assert with the option to continue...
(defun t14v3 (x)
  "mulipty by 14"
  (assert (numberp x) (x));;State the argument that should be allowed to be updated
  (* x 14))

;;An Assert with formatting of the Error Message
(defun eat-porridge (bear)
  (assert (< too-cold (temperature (bear-porridge bear)) too-hot)
          (bear (bear-porridge bear))
          "~a's porridge is not just right: ~a"
          bear (hotness (bear-porridge bear)))
  (eat (bear-porridge bear)))

;;Create a consistency cheacker
;;-Checks for all errors in a data structure
;;-When new errors are discovered add it as a check in the consistency cheacker
;;-Fastest way to help isoloate and find bugs


;;Keep a list of difficult test cases on hand.
;;Allows to see if any changes caused an old bug to pop up
;;AKA regression testing

;;Example of a tool for maintaining a suite of regression tests
(defun test-ex ()
  "Test the program EX on a series of examples."
  (init-ex) ;Initialize the EX program first.
  (assert (equal (ex 3 4) 5))
  (assert (equal (ex 5 0) 0))
  (assert (equal (ex 'x 0) 0))
  )

;;Timing Tools - How long it takes a program to complete

;;Example
(defun f (n) (dotimes (i n) nil))
(time (f 1000000))

;;Compare doing in REPL versus compiling first
;;Compiled version is 300 times faster and uses less storage

;;Make a flexible program first
;;Then modify the most used parts to be more efficient

;;Evaluation - apply functions to arguments

;;funcall - apply a function to individual arguments
(funcall #'+ 1 2 3 4)

;;apply - mainly used to apply a function to a list of arguments - can also have arguments before list
(apply #'+ '(1 2 3 4))
(apply #'+ 1 2'(3 4))

;;Eval - passed a single argument or an atom
(eval '(+ 1 2 3 4))
;;People don't use eval anymore. They use lambda instead.

;;Closures - Nesting parameters for lambda functions for them to perform more functions...

(defun adder (c)
  "Return a function that adds c to its args"
  #'(lambda (x) (+ x c)))

(defun bank-account (balance)
  "Open a bank account starting with the given balance"
  #'(lambda (action amount)
      (case action
        (deposit (setf balance (+ balance amount)))
        (withdraw (setf balance (- balance amount))))))

(setf my-account (bank-account 500.00))
(setf your-account (bank-account 250.00))

(funcall my-account 'withdraw 75.00) ;;Returns 425.00
(funcall your-account 'withdraw 36.00) ;;Returns 214.00
(funcall my-account 'withdraw '1000000) ;;Returns -999575.0
(funcall my-account 'deposit '14000000000)
(funcall my-account 'deposit '1000000000) ;;Returns 14999001000

;;Lexical Variables - deposit lives on even though it was created in a function and the
;;function can be closed and deposit can still be refrenced

;;We can call the closure and it can refrence balance because the code that created
;;the closure appeared lexically wihtin the scope of balance

;;Lisp lexical variables are different becuase they can be captured inside closures and referred to even after the flow of control as left their scope

;;Special Variables - made with defvar or defparaments and can be bound both locally
;;and globally

;;Example:
(defun report1 ()
  (format t "Counter = ~d" *counter*))

(report1) ;;Returns an error for counter to be unbound

(let ((*counter* 100))
  (report1)) ;;Returns an error for counter to be unbound

(report1) ;;Same Error

;;However it should have been 0 first and Third and then 100 for the second
;;As the first and third is when it's global and the second is local.  Local ends
;;once the scope of let is closed. 

;;Exercise what will Let push out (GLOBAL vs LOCAL)
(setf a 'Amazon)
(defvar *b* 'BGS-blog)

(defun fun () *b*)

(let ((a 'EDC)
      (*b* 'shipGB))
  (list a *b* (fun) (symbol-value 'a) (symbol-value '*b*)))
;;(EDC SHIPGB SHIPGB AMAZON SHIPGB)


;;Multiple Values
;;Most functions return one value
;;Round returns two values both the whole and the fraction

(round 2.14)
;;Returns 2
;;Returns 0.140001

;;Functions still only work on the first part even if it returns two values
(* 5 (round 2.14))
;;Returns 10

;;Their Example of creating a special function to return multiple values
(defun show-both (x)
  (multiple-value-bind (int rem)
      (round x)
    (format t "~f = ~d + ~f" x int rem)))
;;Reutnrs 2.14 = 2 + 0.140001

;;You can also write functions of your own that return multiple values using values
(values 1 2 3) ;;returns 1 2 3

;;describe is a function that uses values, but doesn't return any values
(describe 'pp);; Symbol no Value

;;More about parameters 3.19
;;Example

(defun math-quiz (op range n)
  "Ask the suer a series of math problems."
  (dotimes (i n)
    (problem (random range) op (random range))))

(defun problem (x op y)
  "Ask a math problem, read a replay and say if it is correct"
  (format t "~&How much is ~d ~a ~d?" x op y)
  (if eql (read) (funcall op x y))
  (princ "Correct!")
  (princ "Sorry, that's not right!"))

;;This is a lot of parameters to be required.  Make optional and give defaults

(defun math-quiz (&optional (op '+) (range 100) (n 10))
  "Ask the user a series of math problems."
  (dotimes (i n)
    (problem (random range) op (random range))))

;;This makes it optional, but if any changes want to be made then all 3 need to be typed.

;;Instead make parameters position-independent.
;;Explicitily name parameters in function call.

(defun math-quiz (&key (op '+) (range 100) (n 10))
  "Ask the user a series of math porblems."
  (dotimes (i n)
    (problem (random range) op (random range))))

(math-quiz :n 5)
(math-quiz :range 100 :n 5 :op '+)
;;Both are the same...

;;Keywords can be used anywhere not just in argument lists
;;&optional - paremeters are optional
;;&rest - take more 1 or more parameters as input
;;&key - assign in any order what parameter to change from defualt

;;Lambda List Keywords are the same thing

;;Differences between : and &
:xyz ;;returns xyz
&optional ;;returns error
'&optional ;;returns &OPTIONAL

((defun myLove (&xyz) (+ &xyz &xyz)))
(myLove '7) ;;Returns 14

((defun noLove (:xyz) (+ :xyz :xyz)))
;;Error - Keyword appears in variable list

(defun fLove (&key x y) (list x y))

(let ((key s '(:x :y :z)))
  (g (second keys) 1 (first keys) 2))
;;Error - Retype???

;;Find using multiple arguments

(find 3 '(1 2 3 4 -5 6.0)) ;;3
(find 6 '(1 2 3 4 -5 6.0)) ;;NIL

(find 6 '(1 2 3 4 -5 6.0) :test #'equalp) ;6.0
;;The added args allowed us to find 6 as a float even though we searched for an Int

(find 4 '(1 2 3 4 -5 6.0) :test #'<) ;6.0
(find 3 '(1 2 3 4 -5 6.0) :test #'<) ;4
;;Only returns one find not all that match...

(find 5 '(1 2 3 4 -5 6.0) :key #'abs)
;;Find even if negative...

