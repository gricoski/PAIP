;;Chapter 4 GPS - General Problem Solver

;;An example over view of a GPS that will be broken down below:
(defvar *state* nil "The current state: a list of conditions.")
(defvar *ops* nil "A list of available operators.")
(defstruct op "An Operation"
           (action nil) (preconds nil) (add-list nil)
           (del-list nil))
(defun GPS (*state* goals *op*)
  "General Problem Solver: achieve all goals using *op*."
  (if (every #'achieve goals) 'solved))
(defun achieve (goal)
  "A goal is achieved if it already holds,
or if there is an appropiate op for it that is applicable."
  (or (member goal *state*)
      (some #'apply-op
            (find-all goal *op* :test #'appropriate-p))))
