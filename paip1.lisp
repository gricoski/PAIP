;;PAIP
;;C-j to print the code under? if you are in scratch
;;C-c C-c if you saved it as a .lisp to run the code in the repl

;;Variables
(setf p '(john q public))
(john q public)

(setf x 10)
10

(+ x x)
20

(+ 1 x (length p))
14

;;Setf is a speical form, such as IF and Loop
;;Special Forms return values

;;Other special forms
                                        ;defun - defines a function
                                        ;defparameter - defines special variable
                                        ;setf - set variable or field to a new value
                                        ;let - bind local variables
                                        ;case - chose one of serveral alternatives (cond?)
                                        ;if - do one thing or another, depneding on a test
                                        ;function (#') - refer to a function
                                        ;qoute (') - introduce constant data

;;Other List processing functions
(setf p '(Vienna is my Wife))


(first p)
(rest p)
(second p)
(third p)
(fourth p)
(length p)
(last p) ;;creates a list of the last item
(first (last p)) ;;pulls just the element

;;Nil is false and ()

;;Elements can have sublists
(setf x '((1 ele) 2 (element 3) ((4)) 5) )

;;Build up lists
(cons 'Perfect p)
(cons (first p) (rest p))
(setf town (list 'anytown 'usa))
;;List combines lists or elements as is
(list p 'of town ' may 'already 'won!)
;;APpend creates a new list
(append p '(of) town '(may have already won));;Why the fuck does town not need to be qouted.

;;Next Section is 1.5... Go as you want.  Don't rush.  Just do a little a time for fun for passion. 

;;Defun
(defun last-name (name)
  "Select the last name from a name represented list"
  (first (last name)))

;;Even though a first-name function is just a simple first
;;It's good to do so that if we need to change something about how we handle
;;first names we only have to search and change this function below
(defun first-name (name)
  "Returns the first name out of a represented list"
  (first name))

;;Use first first to get into sublists



(first p)
(rest p)
(second p)
(third p)
(fourth p)
(length p)
(last p) ;;creates a list of the last item
(first (last p)) ;;pulls just the element

;;Nil is false and ()

;;Elements can have sublists
(setf x '((1 ele) 2 (element 3) ((4)) 5) )

;;Build up lists
(cons 'Perfect p)
(cons (first p) (rest p))
(setf town (list 'anytown 'usa))
;;List combines lists or elements as is
(list p 'of town ' may 'already 'won!)
;;APpend creates a new list
(append p '(of) town '(may have already won));;Why the fuck does town not need to be qouted.

;;Next Section is 1.5... Go as you want.  Don't rush.  Just do a little a time for fun for passion. 

;;Defun
(defun last-name (name)
  "Select the last name from a name represented list"
  (first (last name)))

;;Even though a first-name function is just a simple first
;;It's good to do so that if we need to change something about how we handle
;;first names we only have to search and change this function below
(defun first-name (name)
  "Returns the first name out of a represented list"
  (first name))

;;Use first first to get into sublists
(setf names '((John Q Public) (malcom x) (admiral Grace Shit) (Spot) (Aristotle)
              (Vienna) (zz top) (Greg) (Miss Scarlet)))

;;Use first first to grab somethign in a sublist
(first-name (first names))

;;Mapcar applies a function to every element of an input list
(mapcar #'first-name names)
;;THe #' notation is needed on functions just like ' is needed on data
(defun whatmapcardoes (liste)
  (list (last-name (first liste))
        (last-name (second liste))
        (last-name (third liste))))
;;Mapcar will work on all elements of a list, but this function will only work on the first three
;;Car is another name for first function

;;Other mapcar examples
(mapcar #'- '(1 2 3 4 5))
(mapcar #'+ '(10 20 30 40) '(1 2 3 4))

;;Back to using mapcar with names
(mapcar #'first-name names)

;;We want to fileter out things like mr ms sir etc..
(defparameter *titles*
  '(Mr Mrs Miss MS Sir Madam Dr Admiral Major General)
  "A list of titles that can appear at the start of names"
  )
;;Astericsk are not needed just formatting
;;Defines a parameter-a variable that does not chagne over computation

(defun first-name2 (name)
  "Pulls first name from lists and avoids pulling shit like mister"
  (if (member (first name) *titles*)
      (first-name2 (rest name))
      (first name)
      ))

;;Works on this and names correctly
(first-name2 '(Madam Major General Paula Jones))

;;We can use Trace to see what is happening here
(trace first-name2)
(untrace first-name2)

;;Next Section 1.7... I'm Learning some pretty cool shit now.  Keep GOING!, but keep it flow
;;Creating a function like mapcar but works on the end of a list

(defun mapend (fn the-list)
  "applies fn to each element the list and append the results"
  (apply #'append (mapcar fn the-list)))

;;How apply works
(apply #'+ '(1 2 3 4))
;;> 10

(apply #'append '((1 2 3) (a b c)))
;;> 1 2 3 A B C

;;States self and it's double value
(defun self-and-double (x) (list x (+ x x)))

;;We can apply this function
(apply #'self-and-double '(3))

;;Mapcar with with self and double
(mapcar #'self-and-double '(1 10 300))
;;returns a  list wiht 3 sublists for each value
(mapend #'self-and-double '(1 100 300))
;;returns one level list with all 6 values
;;It would be error to call mappend with a function that didn't return lists
;;because append expects to see lists as its arguments

(defun number-and-negation (x)
  "if x is a number, return a list of x and -x"
  (if (numberp x)
      (list x (- x))
      nil))

(defun number-and-negations (input)
  "Given a list, return only the numbers and their negations"
  (mapend #'number-and-negation input))

;;Another way to make mapend
(defun mapend (fn the-list)
  "apply fn to each element of list and append the rsults"
  (if (null the-list)
      nil
      (append (funcall fn (first the-list))
              (mapend fn (rest the-list)))) )
;;Funcall is similar to apply
;;Takes function adn applies function to list of args, but args are listed seperately

;;These work
(funcall #'+ 2 3)
(apply #' + '(2 3))

;;This does not work
(funcall #' + '(2 3))
(apply #' + 2 3)

;;Lambda - the Make-function function or make a function wihtout a name
;;Works like this
;;(lambda (*parameters...*) *body...*)

;;Adds two to a number, in this case 4, could also be another function
((lambda (x) (+ x 2)) 4)

;;apply a function without having to define it
(funcall #'(lambda (x) (+ x 2)) 4)

;;Other use cases of lambda

;;Adds the number in list by itself
(mapcar #'(lambda (x) (+ x x)) '(1 2 3 4))

;;States the list and then states in in reverse
(mapend #'(lambda (l) (list l (reverse l))) '((1 2 3) (a b c)))

;;Two reasons why lambda expressions are important
;;First it saves time having to define a function
;;Second it allows you to create new functions are run-time
;;This is known as closures -more in 3.16

;;Next Section is 1.8... Maybe revisit this one.../

;;Strings are to name
;;Symbols used for relatoinships and associations and abstraction

;;Section 1.9
;;Evaluation is done by the interpreter
;;Or it is compiled and turned into machine code

;;***Every Variable is just a memory location.***

;;Exercises 1.11

;;Exercise 1.1
;;Last-name function that can handle suffixes
(defparameter *suffix* '(MD Ph.D Jr. Sr. II III IV V DO Ed MS NP PA))

(defun last-name2 (names)
  (if (member (first (reverse names)) *suffix*)
      (last-name2 (reverse (rest (reverse names))))
      (first (reverse names))))
;;Reversing a recursive parameter leads to weird thigns
;;You can just reverse the reverse of the reverse lol
;;Works

(defun last-name3 (names)
  (if (member (last names) *suffix*)
      (last-name3 (reverse (rest (reverse names))))
      (last names)))
;;Does not work

;;Their Solution


;;Exercise 1.2
;;Exponentiate, raise a number to an integer power
(defun power (n x)
  "Raises n by the exponent of x"
  (cond ((equal x 1) n)
        (t (*
            n
            (power n (1- x))))))
;;First I tried recursive with (* n n)
;;next i tired this but had wrong syntax

(defun power (n x)
  (loop with b = 1
        ));; didn't finish went back to fist

;;Their solution

;;Exercise 1.3
;;Count atoms
(defun count-atoms (liste)
  (cond ((endp liste) 0)
        ((atom (first liste))
         (+ 1 (count-atoms (rest liste))))
        (t (+
            (count-atoms (first liste))
            (count-atoms (rest liste))))))
;;Made before easy

;;Exercise 1.4 (done before)
(defun count-anywhere (x liste)
  (cond ((endp liste) 0)
        ((equal x (first liste))
         (+ 1 (count-anywhere x (rest liste))))
        ((atom (first liste))
         (count-anywhere x (rest liste)))
        (t (+
            (count-anywhere x (first liste))
            (count-anywhere x (rest liste))))))
;;Worked, but say one of the parts that helped
;;However still figured out why it worked.


;;Their solution for Exercise 1.5
(defun dot-product (a b)
  "Compute the matematical do prodcut of two vectors"
  (apply #'+ (mapcar #'* a b)))

;;GOing to PIAP2




