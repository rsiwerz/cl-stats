;;;; cl-stats.lisp
;; Author: Robert Siwerz

;; Description: Simple statistic library for common lisp.
;; Nothing fancy, nor fast. Just gets the job done...

(in-package #:cl-stats)

;; Calculates the sample mean from a random variable X
;; Input:   outcomes - The outcomes of the random variable
;;
;; Output   The sample mean of the outcomes
(defun sample-mean (outcomes)
  (/ (apply #'+ outcomes) (length outcomes)))


;; Calculates the sample variance from a random variable X
;; Input:   outcomes - The outcomes of the random variable
;;
;; Output   The sample variance from a set of outcomes of a random variable
(defun sample-variance (outcomes)
  (let ((m (sample-mean outcomes)))
    (/ (apply #'+ (mapcar (lambda (x) (expt (- x m) 2)) outcomes))
       (- (length outcomes) 1))))

;; Calculates the covariance of two random variables
;; Input:   X - random variable
;;          Y - random variable
;; 
;; Output: The covariance of two random variables
(defun sample-covariance (X Y)
  (let* ((xm (sample-mean X))
	(ym (sample-mean Y))
	(xvar (mapcar (lambda (a) (- a xm)) X))
	(yvar (mapcar (lambda (b) (- b ym)) Y)))
    (apply #'+ (mapcar #'* xvar yvar))))


;; Simple bernoulli distribution function.
;; Input:   p - probability of success
;;          x - the class {0,1}
;;
;; Output:  Probability of class x
(defun bernoulli (p x)
  (* (expt p x) (expt (- 1 p) (- 1 x))))


;; The binomial distribution function which is proportional
;; to the bernoulli distribution. Calculates the probability
;; of k successes in N trials.
;;
;; Example: Probability of getting 4 sixes with a die in
;; a total of 10 trials
;;
;; Input:   k - number of sucesses
;;          N - total amount of trials
;;          p - probabilty of getting one "success" e.g a six on a die.
;; 
;; Output:  Probability of getting k successes in N trials
(defun binomial-dist (k N p)
  (* (bin-coeff N k) (* (expt p k) (expt (- 1 p) (- N k)))))


;; Calculates the binomial coefficent e.g. #of ways of choosing
;; k objects out of a total of N possible
;;
;; Input:   N - possible objects
;;          k - #of objects to choose
;;
;; Output   #of ways to pick k objects out of N
(defun bin-coeff (N k)
  (/ (factorial N) (* (factorial (- N k)) (factorial k))))


;; Classic naive factorial function.

;; TODO: Faster way of calc.
(defun factorial (N &optional (acc 1))
  (if (<= N 1)
      acc
      (factorial (- n 1) (* acc n))))



