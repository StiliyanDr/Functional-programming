#lang racket/base

(define nonempty?
  (lambda (list)
    (not (null? list))))

(define prefix?
  (lambda (lhs rhs)
    (or (null? lhs)
        (and (nonempty? rhs)
             (equal? (car lhs) (car rhs))
             (prefix? (cdr lhs) (cdr rhs))))))

(module+ test
  (require rackunit)
  
  (check-true (prefix? '() '()))
  (check-true (prefix? '() '(1 2 3)))
  (check-true (prefix? '(1) '(1)))
  (check-true (prefix? '(1 2) '(1 2 3)))
  (check-false (prefix? '(1 2 3) '(1 2)))
  (check-false (prefix? '(1 2 2) '(1 2 3))))

(define infix?
  (lambda (lhs rhs)
    (or (prefix? lhs rhs)
        (and (nonempty? rhs)
             (infix? lhs (cdr rhs))))))

(module+ test
  (check-true (infix? '() '()))
  (check-true (infix? '() '(a b c)))
  (check-true (infix? '(a) '(a b c)))
  (check-true (infix? '(b) '(a b c)))
  (check-true (infix? '(c) '(a b c)))
  (check-true (infix? '(b c) '(a b c)))
  (check-false (infix? '(a d) '(a b c)))
  (check-false (infix? '(d) '(a b c)))
  (check-false (infix? '(a b c d) '(a b c)))
  )

(define makeSet
  (lambda (items)
    (define selectUniques
      (lambda (selected item)
        (if (member item selected)
            selected
            (cons item selected))))
    (foldl selectUniques '() items)))

(define occurancesCountOf
  (lambda (item items)
    (foldl (lambda (count current)
             (+ count (if (equal? current item) 1 0))) 0 items)))

(define histogram
  (lambda (items)
    (define makePair
      (lambda (item)
        (cons item (occurancesCountOf item items))))
    (let ([uniques (makeSet items)])
      (foldl (lambda (pairs item)
                        (cons (makePair item) pairs)) '() uniques))))

(define descartes
  (lambda (lhs rhs)
    (define makePairs
      (lambda (item items)
        (foldl (lambda (pairs current)
                 (cons (cons item current) pairs)) '() items)))
    (apply append (map (lambda (item)
                         (makePairs item rhs)) lhs))))