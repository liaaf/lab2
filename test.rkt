#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "mceval-tests.rkt")
(require "lazy-mceval-tests.rkt")

(provide main)

(define mceval-tests
  (test-suite
   "Metacircular Evaluator Tests"
   basic-tests
   primitive-tests
   and-or-tests
   let-tests
   force-delay-tests
   stream-tests
   lazy-tests
   hybrid-tests))

(define (main . argv)
  (when (not (eq? (run-tests mceval-tests) 0))
    (exit 1)))
