;;PAIP3
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



