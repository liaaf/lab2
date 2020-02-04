#lang racket
(require rackunit)
(require "mceval.rkt")

(provide basic-tests
         primitive-tests
         and-or-tests
         let-tests
         force-delay-tests
         stream-tests)

(define (test-mceval exp)
  (with-handlers ([exn:fail? (lambda (exn) exn)])
    (mceval exp (setup-environment))))

(define (test-mceval-exception exp)
  (mceval exp (setup-environment)))

(define basic-tests
  (test-suite
   "Basic tests"
   
   (test-case
    "quote"
    (check-equal?
     (test-mceval '(quote (a b c)))
     '(a b c)))
   
   (test-case
    "set!"
    (check-equal?
     (test-mceval '(begin (define x 0) (set! x 1) x))
     1))
   
   (test-case
    "if"
    (check-equal?
     (test-mceval '(if false false true))
    #t))
   
   (test-case
    "cond"
    (check-equal?
     (test-mceval '(cond (false 0) (true 1)))
    1))
   
   (test-case
    "Lambda application"
    (check-equal?
     (test-mceval '((lambda (x) x) 1))
     1))
   
   (test-case
    "Function application"
    (check-equal?
     (test-mceval '(begin (define (id x) x) (id 1)))
     1))

   ))

(define primitive-tests
  (test-suite
   "Problem 1: Adding primitives"
   (test-case "Implement +"     (check-equal? (test-mceval '(+ 4 5)) 9))
   (test-case "Implement -"     (check-equal? (test-mceval '(- 4 5)) -1))
   (test-case "Implement *"     (check-equal? (test-mceval '(* 4 5)) 20))
   (test-case "Implement /"     (check-equal? (test-mceval '(/ 8 4)) 2))
   (test-case "Implement <"     (check-equal? (test-mceval '(< 4 4)) #f))
   (test-case "Implement <="    (check-equal? (test-mceval '(<= 4 4)) #t))
   (test-case "Implement ="     (check-equal? (test-mceval '(= 4 4)) #t))
   (test-case "Implement >="    (check-equal? (test-mceval '(>= 4 4)) #t))
   (test-case "Implement >"     (check-equal? (test-mceval '(> 4 4)) #f))
   (test-case "Implement error" (check-exn (regexp "^Metacircular Interpreter Aborted$")
                                           (lambda () (test-mceval-exception '(error)))))))

(define and-or-tests
  (test-suite
   "Problem 2: Implementing and and or"
   (test-suite
    "and"
    (test-case "(and (= 2 2) (> 2 1))"
               (check-equal? (test-mceval '(and (= 2 2) (> 2 1))) #t))
    (test-case "(and (= 2 2) (< 2 1))"
               (check-equal? (test-mceval '(and (= 2 2) (< 2 1))) #f))
    (test-case "(and 1 2 'c '(f g)))"
               (check-equal? (test-mceval '(and 1 2 'c '(f g))) '(f g)))
    (test-case "(and false (error))"
               (check-equal? (test-mceval '(and false (error))) #f))
    (test-case "(and)"
               (check-equal? (test-mceval '(and)) #t)))

   (test-suite
    "or"
    (test-case "(or (= 2 2) (> 2 1))"
               (check-equal? (test-mceval '(or (= 2 2) (> 2 1))) #t))
    (test-case "(or (= 2 2) (< 2 1))"
               (check-equal? (test-mceval '(or (= 2 2) (< 2 1))) #t))
    (test-case "(or false false false)"
               (check-equal? (test-mceval '(or false false false)) #f))
    (test-case "(or true (error))"
               (check-equal? (test-mceval '(or true (error))) #t))
    (test-case "(or)"
               (check-equal? (test-mceval '(or)) #f)))))

(define let-tests
  (test-suite
   "Problem 3: Implementing let"
   (test-case "(let ((x 1) (y 2)) (+ x y))"
              (check-equal? (test-mceval '(let ((x 1) (y 2)) (+ x y))) 3))
   (test-case "(let ((x 1) (y 2)) (set! x 2) (+ x y))"
              (check-equal? (test-mceval '(let ((x 1) (y 2)) (set! x 2) (+ x y))) 4))))

(define force-delay-tests
  (test-suite
   "Problem 4: Implementing force and delay"
   (test-case "(begin (delay (error)) 3)"
              (check-equal? (test-mceval '(begin (delay (error)) 3)) 3))
   (test-case "(force (delay 3))"
              (check-equal? (test-mceval '(force (delay 3))) 3))
   (test-case "(let ((x (delay 3))) (force x))"
              (check-equal? (test-mceval '(let ((x (delay 3))) (force x))) 3))
   (test-case "(force (delay (force (delay 3))))"
              (check-equal? (test-mceval '(force (delay (force (delay 3))))) 3))
   (test-case "(let ((x (delay (+ 1 2)))) (+ (force x) (force x)))"
              (check-equal? (test-mceval '(let ((x (delay (+ 1 2)))) (+ (force x) (force x)))) 6))
   (test-case "Delayed expression with side-effect"
              (check-equal? (test-mceval '(let ((x 0))
                                             (let ((y (delay (begin (set! x (+ x 1)) x))))
                                               (+ (force y) (force y)))))
                            2))))

(define stream-tests
  (test-suite
   "Problem 5: Implementing streams"
   (test-case
    "Empty stream is empty"
    (check-equal? (test-mceval '(stream-empty? empty-stream))
                  #t))
   (test-case
    "Can get stream-first of stream-cons "
    (check-equal? (test-mceval '(stream-first (stream-cons 1 empty-stream)))
                  1))
   (test-case
    "Can get stream-rest of stream-cons"
    (check-equal? (test-mceval '(stream-empty? (stream-rest (stream-cons 1 empty-stream))))
                  #t))
   (test-case
    "Stream tail is lazy"
    (check-equal? (test-mceval '(stream-first (stream-cons 1 (error))))
                  1))
   (test-case
    "Stream head is evaluated"
    (check-equal? (test-mceval '(stream-first (stream-cons (+ 2 3) (stream-cons (+ 2 3) (error)))))
                  5))))
