;;Perceptron

;; inputs are multiplied with their weights: K
;;Sum All Ks

(defparameter *inputs*
  '((input1 .5)
    (input2 .45)
    (input3 .25)
    (input4 .10))
  ;;Remember to qoute the list of lists
  )

(defvar *inners* *inputs*)

(defvar *inners2* *inputs*)

(defvar inputs '(1 3 5 14))

(defvar weights '(.36 .25 .11 .14))

(defun coolk (inp wgt)
  "multiply each inp with each wgt and return it's product as one list"
  (cond ((endp inp) nil)
        ((numberp inp)
         (* (first inp) (first wgt)) (coolk (rest inp) (rest wgt)))))
;;This did not work

;;Use Mapcar
(setf k (mapcar #'* inputs weights))
;;WORKED!!!

(setf sumk (reduce #'+ k))

(defun sigmoid (x)
  (/ 1.0 (+ 1.0 (exp (- x)))))

;;To process Error Rate:

;;Error = (target - output)*Sigmoid (Sumk)

;;The shape of the derivative of the squashing function reduces the effective calculated error when thesum of
;;product (sumk) for a neuron is very small or very large

;;To apply the error rate?
(defun dSigmoid (x)
  (let ((tedmp (sigmoid x)))
    (* temp (- 1.0 temp))))
