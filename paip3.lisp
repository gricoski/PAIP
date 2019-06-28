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


