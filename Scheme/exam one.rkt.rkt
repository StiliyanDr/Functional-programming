#lang racket/base

(define accumulate
  (lambda (operation neutralElement from to term next)
    (if (> from to)
        neutralElement
        (operation (term from)
                   (accumulate operation neutralElement (next from) to term next)))))

(define foldr
  (lambda (operation neutralElement list)
    (if (null? list)
        neutralElement
        (operation (car list)
                   (foldr operation neutralElement (cdr list))))))

(define foldl
  (lambda (operation neutralElement list)
    (if (null? list)
        neutralElement
        (foldl operation
               (operation neutralElement (car list))
               (cdr list)))))

(define filter
  (lambda (predicate? list)
    (reverse (foldl (lambda (filtered item)
                      (if (predicate? item)
                          (cons item filtered)
                          filtered)) '() list))))

(define ++
  (lambda (number)
    (+ number 1)))

(define --
  (lambda (number)
    (- number 1)))


#| task one |#

(define digitsCount
  (lambda (number)
    (if (< number 10)
        1
        (++ (digitsCount (quotient number 10))))))

(define lastDigit
  (lambda (number)
    (modulo number 10)))

(define sumPowers
  (lambda (number count)
    (if (< number 10)
        (expt number count)
        (+ (expt (lastDigit number) count)
           (sumPowers (quotient number 10) count)))))

(define narcissistic?
  (lambda (number)
    (let ([count (digitsCount number)])
      (= number (sumPowers number count)))))

(define divides?
  (lambda (candidate number)
    (zero? (modulo number candidate))))

(define sumOfDivisors
  (lambda (number)
    (accumulate (lambda (candidate sum)
                  (+ sum (if (divides? candidate number) candidate 0))) 0 1 (-- number) (lambda (x) x) ++)))

(define friendly?
  (lambda (a b)
    (and [= (sumOfDivisors a) b]
         [= (sumOfDivisors b) a])))



#| task three |#

(define intervalLength
  (lambda (interval)
    (- (cdr interval) (car interval))))

(define min
  (lambda (interval1 interval2)
    (if (< (intervalLength interval1) (intervalLength interval2))
        interval1
        interval2)))

(define minInterval
  (lambda (intervals)
    (let ([head (car intervals)])
      (if (null? (cdr intervals))
          head
          (min head (minInterval (cdr intervals)))))))

(define subinterval?
  (lambda (candidate interval)
    (and [>= (car candidate) (car interval)]
         [<= (cdr candidate) (cdr interval)])))

(define shortest-interval-supersets
  (lambda (intervals)
    (let ([min (minInterval intervals)])
      (insertionSort (filter (lambda (candidate) (subinterval? min candidate)) intervals)))))

(define less?
  (lambda (interval1 interval2)
    (< (cdr interval1) (cdr interval2))))

(define insert
  (lambda (item list)
    (define utility
      (lambda (skipped rest)
        (if (or [null? rest] [less? item (car rest)])
            (append (reverse skipped) (cons item rest))
            (utility (cons (car rest) skipped) (cdr rest)))))
    (utility '() list)))

(define insertionSort
  (lambda (intervals)
    (define utility
      (lambda (sorted rest)
        (if (null? rest)
            sorted
            (utility (insert (car rest) sorted) (cdr rest)))))
    (utility '() intervals)))



#| task two |#

(define nestedValue
  (lambda (f i j)
    (if (= (++ i) j)
        (f i j)
        (f i (nestedValue f (++ i) j)))))

(define max
  (lambda (a b)
    (if (> a b) a b)))

(define findMax
  (lambda (f a b)
    (define utility
      (lambda (i j)
        (if (= j b)
            (nestedValue f i j)
            (max (nestedValue f i j)
                 (utility i (++ j))))))
    (if (= (++ a) b)
        (nestedValue f a b)
        (max (utility a (++ a))
             (findMax f (++ a) b)))))