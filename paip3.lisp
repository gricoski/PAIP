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
(setf (rest list) nil)
(setf (name-middle b) 'Q)

;;A place or generalized variable is a name for a location that
;;can have a value stored in it

;;You can extrend expressions allowed in setf with
;; defsetf (page 514) and define-setf-method (page 884)







