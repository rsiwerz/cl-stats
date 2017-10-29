;;;; cl-stats.lisp

(in-package #:cl-stats)

;;; "cl-stats" goes here. Hacks and glory await!

(defun bernoulli (p x)
  (* (expt p x) (expt (- 1 p) (- 1 x))))
