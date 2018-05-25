;;;; package.lisp

(defpackage #:cl-stats
  (:use #:cl)
  (:export #:sample-mean
	   #:sample-variance
	   #:sample-covariance
	   #:bernoulli
	   #:binomial-dist
	   #:bin-coeff
	   #:factorial))

