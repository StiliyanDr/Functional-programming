#lang racket/base

(define --
  (lambda (number)
    (- number 1)))

(define ++
  (lambda (number)
    (+ number 1)))

(define headOf
  (lambda (list)
    (car list)))

(define tailOf
  (lambda (list)
    (cdr list)))

(define insertFront
  (lambda (item list)
    (cons item list)))

(define emptyList?
  (lambda (list)
    (null? list)))

(define member
  (lambda (item list)
    (cond
      [(emptyList? list) #f]
      [(equal? item (headOf list)) list]
      [else (member item (tailOf list))])))

(module+ test
  (require rackunit)

  (check-equal? (member 1 '()) #f)
  (check-equal? (member 1 '(1 2 3)) '(1 2 3))
  (check-equal? (member 3 '(1 2 3 4)) '(3 4))
  )

(require rackunit)

(define listTail
  (lambda (list n)
    (if (zero? n)
        list
        (listTail (tailOf list) (-- n)))))

(module+ test
  (check-equal? (listTail '(1 2 3) 0) '(1 2 3))
  (check-equal? (listTail '(1 2 3) 2) '(3))
  (check-equal? (listTail '(1 2 3) 3) '())
  )

(define listRef
  (lambda (list n)
    (cond
      [(emptyList? list) #f]
      [(zero? n) (headOf list)]
      [else (listRef (tailOf list) (-- n))])))

(module+ test
  (check-equal? (listRef '(1) 123123) #f)
  (check-equal? (listRef '(1 2) 0) 1)
  (check-equal? (listRef '(1 2 3 4) 2) 3)
  )

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

(define rangeSimple
  (lambda (from to)
    (if (> from to)
        '()
        (insertFront from (rangeSimple (++ from) to)))))

(define Range
  (lambda (from to)
    (define range-i
      (lambda (current result)
        (if (> current to)
            (reverse result)
            (range-i (++ current) (insertFront current result)))))
    (range-i from '())))

(define id
  (lambda (x) x))

(define flippedInsertFront
  (lambda (list item)
    (insertFront item list)))

(define range
  (lambda (start end)
    (reverse (accumulate-i flippedInsertFront '() start end id ++))))

(module+ test
  (check-equal? (range 1 0) '())
  (check-equal? (range 1 1) '(1))
  (check-equal? (range 1 5) '(1 2 3 4 5))
  )

(define lastDigitOf
  (lambda (number)
    (modulo number 10)))

(define removeLastDigitOf
  (lambda (number)
    (quotient number 10)))

(define digitList
  (lambda (number)
    (define utility
      (lambda (rest result)
        (if (< rest 10)
            (insertFront rest result)
            (utility (removeLastDigitOf rest)
                     (insertFront (lastDigitOf rest) result)))))
    (utility number '())))

(module+ test
  (check-equal? (digitList 0) '(0))
  (check-equal? (digitList 9) '(9))
  (check-equal? (digitList 110) '(1 1 0))
  (check-equal? (digitList 12223345) '(1 2 2 2 3 3 4 5))
  )

(define take
  (lambda (list n)
    (define utility
      (lambda (rest n taken)
        (if (zero? n)
            (reverse taken)
            (utility (tailOf rest)
                     (-- n)
                     (insertFront (headOf rest) taken)))))
      (utility list n '())))

(module+ test
  (check-equal? (take '(1 2 3) 0) '())
  (check-equal? (take '(1 2 3) 1) '(1))
  (check-equal? (take '(1 2 3) 2) '(1 2))
  )

(define exists?
  (lambda (from to predicate? mappingFunction step stop?)
    (and (not (stop? from to))
         (or (predicate? (mappingFunction from))
             (exists? (step from) to predicate? mappingFunction step stop?)))))

(define complement
  (lambda (predicate?)
    (lambda (expression)
      (not (predicate? expression)))))

(define forEach?
  (lambda (from to predicate? mappingFunction step stop?)
    (not (exists? from to (complement predicate?) mappingFunction step stop?))))

(define all?
  (lambda (p? list)
    (forEach? list '() p? headOf tailOf equal?)))

(module+ test
  (check-true (all? odd? '()))
  (check-false (all? odd? '(1 3 5 2)))
  (check-true (all? odd? '(1 3 5 7)))
  )

(define any?
  (lambda (p? list)
    (not (all? (complement p?) list))))

(module+ test
  (check-false (any? odd? '()))
  (check-false (any? odd? '(0 2 4)))
  (check-true (any? odd? '(0 2 1 4)))
  )

(define zipWith
  (lambda (function lhs rhs)
    (define utility
      (lambda (restOfLhs restOfRhs result)
        (if (emptyList? restOfLhs)
            (reverse result)
            (utility (tailOf restOfLhs)
                     (tailOf restOfRhs)
                     (insertFront (function (headOf restOfLhs)
                                            (headOf restOfRhs))
                                  result)))))
    (utility lhs rhs '())))

(define zip
  (lambda (lhs rhs)
    (zipWith cons lhs rhs)))

(module+ test
  (check-equal? (zip '() '()) '())
  (check-equal? (zip '(1 2 3) '(1 2 3)) '((1 . 1) (2 . 2) (3 . 3)))
  )

(define sorted?
  (lambda (list)
    (or (emptyList? list)
        (emptyList? (tailOf list))
        (and (<= (headOf list) (headOf (tailOf list)))
             (sorted? (tailOf list))))))

(module+ test
  (check-true (sorted? '()))
  (check-true (sorted? '(123)))
  (check-true (sorted? '(1 2 3 3 3 4 5)))
  (check-true (sorted? '(1 2 3 4 5)))
  (check-false (sorted? '(1 1 2 1 3)))
  )

(define uniquesSimple
  (lambda (list)
    (cond
      [(emptyList? list) list]
      [(member (headOf list) (tailOf list)) (uniquesSimple (tailOf list))]
      [else (insertFront (headOf list) (uniquesSimple (tailOf list)))])))

(define uniques
  (lambda (list)
    (define utility
      (lambda (rest result)
        (if (emptyList? rest)
            result
            (let ([head (headOf rest)]
                  [tail (tailOf rest)])
              (utility tail
                       (if (member head tail)
                           result
                           (insertFront head result)))))))
    (utility list '())))

(define sameOrderUniques
  (lambda (list)
    (reverse (uniques list))))

(module+ test
  (check-equal? (sameOrderUniques '()) '())
  (check-equal? (sameOrderUniques '(1 2 1 3 1 2 3 2 1 3 1 2 3)) '(1 2 3))
  (check-equal? (sameOrderUniques '(1 2 3 4)) '(1 2 3 4))
  )

(define extract-ints
  (lambda (list)
    (filter integer? list)))

(module+ test
  (check-equal? (extract-ints '()) '())
  (check-equal? (extract-ints '(1 #f "123")) '(1))
  )

(define insert
  (lambda (item sortedList)
    (define utility
      (lambda (skipped rest)
        (if (or [emptyList? rest] [< item (headOf rest)])
            (append (reverse skipped) (insertFront item rest))
            (utility (insertFront (headOf rest) skipped)
                     (tailOf rest)))))
    (utility '() sortedList)))

(module+ test
  (check-equal? (insert 1 '()) '(1))
  (check-equal? (insert 1 '(2 3)) '(1 2 3))
  (check-equal? (insert 3 '(1 2 4)) '(1 2 3 4))
  (check-equal? (insert 3 '(1 2)) '(1 2 3))
  )

(define insertionSort
  (lambda (list)
    (define utility
      (lambda (sorted rest)
        (if (emptyList? rest)
            sorted
            (utility (insert (headOf rest) sorted)
                     (tailOf rest)))))
    (utility '() list)))

(module+ test
  (check-equal? (insertionSort '()) '())
  (check-equal? (insertionSort '(1)) '(1))
  (check-equal? (insertionSort '(5 4 3 2 1)) '(1 2 3 4 5))
  )

(define arity
  (lambda arguments
    (length arguments)))

(define compose
  (lambda functions
    (foldr (lambda (f composition)
             (lambda (argument)
               (f (composition argument)))) id functions)))

(define square
  (lambda (number)
    (* number number)))

(define plusOneSquared (compose square ++))
(define f (compose -- square ++ (lambda (x) (* 2 x))))

(module+ test
  (check-eqv? (f 5) 120)
  (check-eqv? (f 0) 0)
  )

(define removeAssoc
  (lambda (key list)
    (filter (lambda (pair)
              (not (equal? (car pair) key))) list)))

(define associativeList '((a . 1) (b . 2) (c . 3)))

(module+ test
  (check-equal? (removeAssoc 'z associativeList) associativeList)
  (check-equal? (removeAssoc 'a associativeList) (cdr associativeList))
  )

(define addAssoc
  (lambda (key value list)
    (let ([newPair (cons key value)])
      (cons newPair (removeAssoc key list)))))

(module+ test
  (check-equal? (addAssoc 'd 0 associativeList) (cons '(d . 0) associativeList))
  (check-equal? (addAssoc 'a 10 associativeList) (cons '(a . 10) (cdr associativeList)))
  )

(define keyOf car)
(define valueOf cdr)

(define groupBy
  (lambda (function inputValues)
    (define makePair
      (lambda (value)
        (cons (function value) value)))
    (define makeGroupValue list)
    (define addToGroupValue
      (lambda (value group)
        (let ([groupValue (cadr group)])
          (makeGroupValue (cons value groupValue)))))
    (define operation
      (lambda (grouped pair)
        (let* ([group (assoc (keyOf pair) grouped)]
               [groupValue (if (not group)
                               (makeGroupValue (list (valueOf pair)))
                               (addToGroupValue (valueOf pair) group))])
          (addAssoc (keyOf pair) groupValue grouped))))
    (let ([pairs (map makePair inputValues)])
      (foldl operation '() pairs))))

(define map
  (lambda (function list . lists)
    (define mapSingle
      (lambda (function list)
        (reverse (foldl (lambda (mapped item)
                          (cons (function item) mapped)) '() list))))
    (if (or (null? list) (null? lists))
        (mapSingle function list)
        (let ([heads (cons (car list)
                           (mapSingle car lists))]
              [tails (cons (cdr list)
                           (mapSingle cdr lists))])
          (cons (apply function heads)
                (apply map (cons function tails)))))))