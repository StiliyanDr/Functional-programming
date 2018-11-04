#lang racket/base
(require rackunit)
(require rackunit/text-ui)

(define ++
  (lambda (number)
    (+ number 1)))

(define --
  (lambda (number)
    (- number 1)))

(define id
  (lambda (x) x))

(define accumulate
  (lambda (operation neutralElement from to term next)
    (if (> from to)
        neutralElement
        (operation (term from)
                   (accumulate neutralElement (next from) to term next)))))

(define accumulate-i
  (lambda (operation neutralElement from to term next)
    (if (> from to)
        neutralElement
        (accumulate-i operation
                      (operation neutralElement (term from))
                      (next from)
                      to
                      term
                      next))))

(define sumRange
  (lambda (from to)
    (accumulate-i + 0 from to id ++)))

(module+ test
  (check-equal? (sumRange 1 0) 0)
  (check-equal? (sumRange 0 0) 0)
  (check-equal? (sumRange 1 10) 55)
  )

(define !!
  (lambda (number)
    (accumulate-i * 1 (if (odd? number) 3 2) number id (lambda (x) (+ x 2)))))

(define-test-suite testDoubleFactorial
  [test-case "0!! = 1"
             (check-equal? (!! 0) 1)]

  [test-case "1!! = 1"
             (check-equal? (!! 1) 1)]
  
  [test-case "double factorial for even numbers"
             (check-equal? (!! 10) 3840)]

  [test-case "double factorial for odd numbers"
             (check-equal? (!! 9) 945)]
  )

(run-tests testDoubleFactorial)

(define nChooseK
  (lambda (n k)
    (accumulate-i * 1 1 k (lambda (i) (/ (++ (- n i)) i)) ++)))

(define-test-suite testNChooseK
  [test-case "n choose n is 1"
             (check-equal? (nChooseK 10 10) 1)]

  [test-case "n choose 0 is 1"
             (check-equal? (nChooseK 10 0) 1)]
  
  [test-case "n choose 1 is n"
             (check-equal? (nChooseK 10 1) 10)]

  [test-case "n choose (n - 1) is n"
             (check-equal? (nChooseK 10000 9999) 10000)]
  )

(run-tests testNChooseK)

(define sum1ToN
  (lambda (n)
    (nChooseK (++ n) 2)))

(define-test-suite testSum1ToN
  [test-case "sum of empty range is 0"
             (check-equal? (sum1ToN 0) 0)]

  [test-case "sum range with more than one element"
             (check-equal? (sum1ToN 10) 55)]
  )

(run-tests testSum1ToN)

(define twoPowerNSimple
  (lambda (n)
    (accumulate-i * 1 1 n (lambda (i) 2) ++)))

(define twoPowerN
  (lambda (n)
    (accumulate-i + 0 0 n (lambda (i) (nChooseK n i)) ++)))

(define divisorsSum
  (lambda (number)
    (accumulate-i + 0 1 number (lambda (i) (if (zero? (modulo number i)) i 0)) ++)))

(define count
  (lambda (p? a b)
    (accumulate-i + 0 a b (lambda (i) (if (p? i) 1 0)) ++)))

(define existsSimple?
  (lambda (p? a b)
    (>= (count p? a b) 1)))

(define exists?
  (lambda (from to predicate? mappingFunction step stop?)
    (and (not (stop? from to))
         (or (predicate? (mappingFunction from))
             (exists? (step from) to predicate? mappingFunction step stop?)))))

(define existsInInterval?
  (lambda (a b p?)
    (exists? a b p? id ++ >)))

(define-test-suite testExistsInInterval?
  [test-case "the result is false for the empty set"
             (check-false (existsInInterval? 1 0 (lambda (x) #t)))]

  [test-case "true when an element with the desired property exists"
             (check-true (existsInInterval? 1 999999999 (lambda (x) (= 45 x))))]

  [test-case "false when no element with the desired property exists"
             (check-false (existsInInterval? 1 100 zero?))]
  )

(run-tests testExistsInInterval?)

(define forEachSimple?
  (lambda (p? a b)
    (= (count p? a b) (+ (- b a) 1))))

(define complement
  (lambda (predicate?)
    (lambda (expression)
      (not (predicate? expression)))))

(define forEach?
  (lambda (from to predicate? mappingFunction step stop?)
    (not (exists? from to (complement predicate?) mappingFunction step stop?))))

(define forEachInInterval?
  (lambda (a b p?)
    (forEach? a b p? id ++ >)))

(define-test-suite testForEachInInterval?
  [test-case "true for the empty set"
             (check-true (forEachInInterval? 1 0 (lambda (x) #f)))]

  [test-case "false when at least one element does not have the property"
             (check-false (forEachInInterval? 1 99999999999 (lambda (x) (not (= x 99)))))]

  [test-case "true when each element has the property"
             (check-true (forEachInInterval? 1 100 number?))]
  )

(run-tests testForEachInInterval?)

(define divides?
  (lambda (candidate number)
    (zero? (modulo number candidate))))

(define prime?
  (lambda (number)
    (and (not (= number 1))
         (forEachInInterval? 2 (sqrt number)
                             (complement (lambda (candidate)
                                           (divides? candidate number)))))))

(define-test-suite testPrime?
  [test-case "1 is not a prime"
             (check-false (prime? 1))]

  [test-case "true for prime numbers"
             (check-true (prime? 2))
             (check-true (prime? 3))
             (check-true (prime? 5))
             (check-true (prime? 1213))]

  [test-case "false for nonprime numbers"
             (check-false (prime? 4))
             (check-false (prime? 49))
             (check-false (prime? 169))]
  )

(run-tests testPrime?)

(define constantly
  (lambda (constant)
    (lambda (number) constant)))

(define flip
  (lambda (function)
    (lambda (a b)
      (function b a))))
