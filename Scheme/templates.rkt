#lang racket/base

(map (lambda (x)
       (map (lambda (f) (filter f x))
            (list negative? zero? positive?)))
     '((-2 1 0) (1 4 -1) (0 0 1)))

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

(define foldr1
  (lambda (operation nonEmptyList)
    (if (null? (cdr nonEmptyList))
        (car nonEmptyList)
        (operation (car nonEmptyList)
                   (foldr1 operation (cdr nonEmptyList))))))

(define foldl1
  (lambda (operation nonEmptyList)
    (foldl operation (car nonEmptyList) (cdr nonEmptyList))))