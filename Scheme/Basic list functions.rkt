#lang racket/base
(require rackunit)

(define emptyList?
  (lambda (list)
    (null? list)))

(define headOf
  (lambda (list)
    (car list)))

(define tailOf
  (lambda (list)
    (cdr list)))

(define insertFront
  (lambda (item list)
    (cons item list)))

(define foldr
  (lambda (operation neutralElement list)
    (if (emptyList? list)
        neutralElement
        (operation (headOf list)
                   (foldr operation neutralElement (tailOf list))))))

(define foldl
  (lambda (operation neutralElement list)
    (if (emptyList? list)
        neutralElement
        (foldl operation
               (operation neutralElement (headOf list))
               (tailOf list)))))

(define lengthSimple
  (lambda (list)
    (if (emptyList? list)
        0
        (+ 1 (lengthSimple (tailOf list))))))

(define length
  (lambda (list)
    (define length-i
      (lambda (rest result)
        (if (emptyList? rest)
            result
            (length-i (tailOf rest) (+ 1 result)))))
    (length-i list 0)))

(define Length
  (lambda (list)
    (foldl (lambda (result item) (+ result 1)) 0 list)))

(define-test-suite testLength
  [test-case "lenght of empty list is 0"
             (check-equal? (Length '()) 0)]

  [test-case "length of one element list is 1"
             (check-equal? (Length '(1024)) 1)]

  [test-case "length of list with more than one element"
             (check-equal? (Length '(1 2 3 4)) 4)]
  )

;standart function is more efficient
(define reverse
  (lambda (list)
    (define reverse-i
      (lambda (rest reversed)
        (if (emptyList? rest)
            reversed
            (reverse-i (tailOf rest) (insertFront (headOf rest) reversed)))))
    (reverse-i list '())))

(define Reverse
  (lambda (list)
    (foldl (lambda (reversed headOfRest)
             (insertFront headOfRest reversed)) '() list)))

(define-test-suite testReverse
  [test-case "reverse the empty list"
             (check-equal? (Reverse '()) '())]

  [test-case "reverse nonempty list"
             (check-equal? (Reverse '(5 4 3 2 1)) '(1 2 3 4 5))]
  )

(define square
  (lambda (number)
    (* number number)))

(define map
  (lambda (function list)
    (define map-i
      (lambda (rest mapped)
        (if (emptyList? rest)
            (reverse mapped)
            (map-i (tailOf rest)
                   (insertFront (function (headOf rest)) mapped)))))
    (map-i list '())))

(define Map
  (lambda (function list)
    (reverse (foldl (lambda (mapped headOfRest)
                      (insertFront (function headOfRest) mapped)) '() list))))

(define MAP
  (lambda (function list)
    (foldr (lambda (item mapped)
             (insertFront (function item) mapped)) '() list)))

(define-test-suite testMap
  [test-case "map the empty list"
             (check-equal? (Map square '()) '())]

  [test-case "map a nonempty list"
             (check-equal? (Map square '(1 2 3 4)) '(1 4 9 16))]
  )

(define filter
  (lambda (predicate? list)
    (define filter-i
      (lambda (rest filtered)
        (if (emptyList? rest)
            (reverse filtered)
            (filter-i (tailOf rest)
                      (if (predicate? (headOf rest))
                          (insertFront (headOf rest) filtered)
                          filtered)))))
    (filter-i list '())))

(define Filter
  (lambda (predicate? list)
    (foldr (lambda (item filtered)
             (if (predicate? item)
                 (insertFront item filtered)
                 filtered)) '() list)))

(define FILTER
  (lambda (predicate? list)
    (reverse (foldl (lambda (filtered item)
                      (if (predicate? item)
                          (insertFront item filtered)
                          filtered)) '() list))))

(define-test-suite testFilter
  [test-case "filter the empty list"
             (check-equal? (filter (lambda (x) #t) '()) '())]

  [test-case "filter nonempty list"
             (check-equal? (filter even? '(0 1 2 3 4 5)) '(0 2 4))]
  )

(define append
  (lambda (lhs rhs)
    (if (emptyList? lhs)
        rhs
        (insertFront (headOf lhs)
                     (append (tailOf lhs) rhs)))))

(define Append
  (lambda (lhs rhs)
    (foldr (lambda (item appended)
             (insertFront item appended)) rhs lhs)))

(define append-i
  (lambda (lhs rhs)
    (define utility
      (lambda (rest result)
        (if (emptyList? rest)
            (reverse result)
            (utility (tailOf rest)
                     (insertFront (headOf rest) result)))))
    (utility rhs (reverse lhs))))

(define appendLists
  (lambda lists
    (foldl (lambda (appended list)
             (append appended list)) '() lists)))

(define-test-suite testAppend
  [test-case "append two empty lists"
             (check-equal? (append-i '() '()) '())]

  [test-case "append empty to nonempty list"
             (check-equal? (append-i '(1) '()) '(1))]

  [test-case "append nonempty to empty list"
             (check-equal? (append-i '() '(1)) '(1))]

  [test-case "append two nonempty lists"
             (check-equal? (append-i '(1 2) '(3 4)) '(1 2 3 4))]

  [test-case "append more than two lists"
             (check-equal? (appendLists '(1) '(2 3) '(4 5)) '(1 2 3 4 5))]
  )