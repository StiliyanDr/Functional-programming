#lang racket/base
(require rackunit)

(define ++
  (lambda (number)
    (+ number 1)))

(define --
  (lambda (number)
    (- number 1)))

(define id
  (lambda (x) x))

(define accumulate
  (lambda (operation result from to step mapFunction stop?)
    (if (stop? from to)
        result
        (accumulate operation
                    (operation (mapFunction from) result)
                    (step from)
                    to
                    step
                    mapFunction
                    stop?))))

(define sumList
  (lambda (list)
    (accumulate + 0 list '() cdr car eqv?)))

(define-test-suite testSumList
  [test-case "sum of empty list is zero"
             (check-equal? (sumList '()) 0)]

  [test-case "sum of single element list is the element"
             (check-equal? (sumList '(8)) 8)]

  [test-case "sum of list with more than one element"
             (check-equal? (sumList '(1 2 3 4 5)) 15)]
  )

(define sumRangeSimple
  (lambda (from to)
    (accumulate + 0 from to ++ id >)))

(module+ test
  (check-equal? (sumRangeSimple 1 0) 0)
  (check-equal? (sumRangeSimple 0 0) 0)
  (check-equal? (sumRangeSimple 1 10) 55)
  )

(define !!
  (lambda (number)
    (accumulate * 1 number 1 (lambda (x) (- x 2)) id <)))

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

(define nChooseK
  (lambda (n k)
    (accumulate * 1 1 k ++ (lambda (i) (/ (++ (- n i)) i)) >)))

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

(define sumRange
  (lambda (from to)
    (nChooseK (++ to) 2)))

(define-test-suite testSumRange
  [test-case "sum of empty range is 0"
             (check-equal? (sumRange 1 0) 0)]

  [test-case "sum of single element range is the element"
             (check-equal? (sumRange 10 10) 10)]

  [test-case "sum range with more than one element"
             (check-equal? (sumRange 1 10) 55)]
  )

(define twoPowerNSimple
  (lambda (n)
    (accumulate * 1 1 n ++ (lambda (i) 2) >)))

(define twoPowerN
  (lambda (n)
    (accumulate + 0 0 n ++ (lambda (i) (nChooseK n i)) >)))

(define divisorsSum
  (lambda (number)
    (accumulate + 0 1 number ++ (lambda (i) (if (zero? (modulo number i)) i 0)) >)))

(define count
  (lambda (p? a b)
    (accumulate + 0 a b ++ (lambda (i) (if (p? i) 1 0)) >)))

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

(define forEachSimple?
  (lambda (p? a b)
    (= (count p? a b) (+ (- b a) 1))))

(define forEach?
  (lambda (from to predicate? mappingFunction step stop?)
    (not (exists? from to (lambda (i) (not (predicate? i))) mappingFunction step stop?))))

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

(define divides?
  (lambda (candidate number)
    (zero? (modulo number candidate))))

(define prime?
  (lambda (number)
    (and (not (= number 1))
         (forEachInInterval? 2 (sqrt number)
                             (lambda (candidate) (not (divides? candidate number)))))))

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

(define constantly
  (lambda (constant)
    (lambda (number) constant)))

(define flip
  (lambda (function)
    (lambda (a b)
      (function b a))))

(define complement
  (lambda (predicate?)
    (lambda (expression)
      (not (predicate? expression)))))