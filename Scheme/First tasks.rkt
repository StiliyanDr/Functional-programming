#lang racket/base

(define --
  (lambda (number)
    (- number 1)))

(define square
  (lambda (number)
    (* number number)))

(define fastExpt
  (lambda (number n)
    (cond
      [(= n 0) 1]
      [(< n 0) (/ 1 (fastExpt number (- n)))]
      [(even? n) (square (fastExpt number (/ n 2)))]
      [else (* number (fastExpt number (-- n)))])))

(module+ test
  (require rackunit)

  (check-eq? (fastExpt 2 0) 1)
  (check-eq? (fastExpt 2 5) 32)
  (check-eq? (fastExpt 2 6) 64)
  (check-eqv? (fastExpt 2 -5) 1/32)
  (check-eqv? (fastExpt 2 -6) 1/64))

(define rootsCount
  (lambda (a b c)
    (let ([discriminant (- (* b b) (* (* 4 a) c))])
      (cond
        [(> discriminant 0) 2]
        [(= discriminant 0) 1]
        [else 0]))))

(module+ test
  (check-eq? (rootsCount 2 5 8) 0)
  (check-eq? (rootsCount 1 2 1) 1)
  (check-eq? (rootsCount 2 5 1) 2))

(define factorial
  (lambda (n)
    (letrec ([fact (lambda (n result)
                     (if (= n 0)
                         result
                         (fact (-- n) (* n result))))])
      (fact n 1))))

(module+ test
  (check-eq? (factorial 0) 1)
  (check-eq? (factorial 1) 1)
  (check-eq? (factorial 5) 120))

(define fibonacci
  (lambda (n)
    (letrec ([fib (lambda (a b n)
                    (cond
                      [(= n 0) a]
                      [(= n 1) b]
                      [else (fib b (+ a b) (-- n))]))])
      (fib 0 1 n))))

(module+ test
  (check-eq? (fibonacci 0) 0)
  (check-eq? (fibonacci 1) 1)
  (check-eq? (fibonacci 7) 13))

(define lastDigitOf
  (lambda (number)
    (modulo number 10)))

(define appendDigitTo
  (lambda  (digit number)
    (+ (* 10 number) digit)))

(define removeLastDigitOf
  (lambda (number)
    (quotient number 10)))

(define reverseInteger
  (lambda (number)
    (letrec ([reverse (lambda (rest result)
                        (if (= rest 0)
                            result
                            (reverse (removeLastDigitOf rest)
                                     (appendDigitTo (lastDigitOf rest) result))))])
      (if (>= number 0)
          (reverse number 0)
          (- (reverse (- number) 0))))))

(module+ test
  (check-eq? (reverseInteger 0) 0)
  (check-eq? (reverseInteger 8) 8)
  (check-eq? (reverseInteger 12345) 54321)
  (check-eq? (reverseInteger -12345) -54321))

(define palindrome?
  (lambda (number)
    (= number (reverseInteger number))))

(module+ test
  (check-true (palindrome? 0))
  (check-true (palindrome? 5))
  (check-true (palindrome? 12321))
  (check-true (palindrome? 1221))
  (check-false (palindrome? 12322)))

(define divides?
  (lambda (a b)
    (= (modulo b a) 0)))

(define sumDivisorsOf
  (lambda (number)
    (letrec ([doSum (lambda (candidate sum)
                      (if (= candidate 1)
                          (+ sum 1)
                          (doSum (-- candidate)
                                 (+ (if (divides? candidate number) candidate 0) sum))))])
      (doSum number 0))))

(module+ test
  (check-eq? (sumDivisorsOf 1) 1)
  (check-eq? (sumDivisorsOf 12) 28))

(define perfect?
  (lambda (number)
    (= (* 2 number) (sumDivisorsOf number))))

(module+ test
  (check-true (perfect? 6))
  (check-true (perfect? 28))
  (check-true (perfect? 33550336))
  (check-false (perfect? 2))
  (check-false (perfect? 10)))

(define ++
  (lambda (number)
    (+ number 1)))

(define prime?
  (lambda (number)
    (define sqrtNumber (sqrt number))
    (letrec ([hasDivisor? (lambda (candidate)
                            (and (<= candidate sqrtNumber)
                                 (or (divides? candidate number)
                                     (hasDivisor? (++ candidate)))))])
      (not (or (= number 1)
               (hasDivisor? 2))))))

(module+ test
  (check-false (prime? 1))
  (check-false (prime? 4))
  (check-false (prime? 49))
  (check-true (prime? 2))
  (check-true (prime? 5))
  (check-true (prime? 29)))

(define increasing?
  (lambda (number)
    (or (< number 10)
        (let ([lastDigit (lastDigitOf number)]
              [rest (removeLastDigitOf number)])
          (and (< (lastDigitOf rest) lastDigit)
               (increasing? rest))))))

(module+ test
  (check-true (increasing? 0))
  (check-true (increasing? 12468))
  (check-false (increasing? 12134))
  (check-false (increasing? 12234)))

(define toBinary
  (lambda (number)
    (letrec ([convert (lambda (rest result factor)
                        (if (= rest 0)
                            result
                            (let ([bit (modulo rest 2)])
                              (convert (quotient rest 2)
                                       (+ (* factor bit) result)
                                       (* 10 factor)))))])
      (convert number 0 1))))

(module+ test
  (check-eq? (toBinary 0) 0)
  (check-eq? (toBinary 1) 1)
  (check-eq? (toBinary 12) 1100)
  (check-eq? (toBinary 65789) 10000000011111101))

(define toDecimal
  (lambda (binaryNumber)
    (letrec ([convert (lambda (restOfBinaryNumber powerOfTwo result)
                        (if (= restOfBinaryNumber 0)
                            result
                            (convert (removeLastDigitOf restOfBinaryNumber)
                                     (* 2 powerOfTwo)
                                     (+ result (* powerOfTwo (lastDigitOf restOfBinaryNumber))))))])
    (convert binaryNumber 1 0))))

(module+ test
  (check-eq? (toDecimal 0) 0)
  (check-eq? (toDecimal 1) 1)
  (check-eq? (toDecimal 1100) 12)
  (check-eq? (toDecimal 10000000011111101) 65789)
  (check-eq? (toDecimal (toBinary 123456)) 123456))