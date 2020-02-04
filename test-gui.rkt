#lang racket
(require rackunit)
(require rackunit/text-ui)
(require rackunit/gui)
(require "mceval-tests.rkt")
(require "lazy-mceval-tests.rkt")

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

(test/gui mceval-tests)
