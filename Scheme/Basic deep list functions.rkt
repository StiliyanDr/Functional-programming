#lang racket/base

(define id
  (lambda (x) x))

(define atom?
  (lambda (x)
    (not (or [null? x] [pair? x]))))

(module+ test
  (require rackunit)
  (check-false (atom? '()))
  (check-false (atom? '(1 2 3)))
  (check-true (atom? 1))
  )

(define count-atoms
  (lambda (list)
    (cond
      [(null? list) 0]
      [(atom? list) 1]
      [else (+ (count-atoms (car list)) (count-atoms (cdr list)))])))

(define countAtoms
  (lambda (list)
    (deepFoldr + 0 (lambda (x) 1) list)))

(module+ test
  (check-eqv? (countAtoms '()) 0)
  (check-eqv? (countAtoms '(8)) 1)
  (check-eqv? (countAtoms '(1 ((1 2) 1 ()) ((() 2 3) (4)))) 7)
  )

(define Flatten
  (lambda (deepList)
    (cond
      [(null? deepList) '()]
      [(atom? deepList) (list deepList)]
      [else (append (Flatten (car deepList)) (Flatten (cdr deepList)))])))

(define flatten
  (lambda (deepList)
    (deepFoldr append '() list deepList)))

(module+ test
  (check-equal? (flatten '()) '())
  (check-equal? (flatten '(1)) '(1))
  (check-equal? (flatten '(1 ((2) (((3) () 4) 5)))) '(1 2 3 4 5))
  )

(define deep-reverse
  (lambda (deepList)
    (cond
      [(null? deepList) '()]
      [(atom? deepList) deepList]
      [else (append (deep-reverse (cdr deepList))
                    (list (deep-reverse (car deepList))))])))

(define deepReverse
  (lambda (deepList)
    (deepFoldr (lambda (reversedHead reversedTail)
                 (append reversedTail (list reversedHead))) '() id deepList)))

(module+ test
  (check-equal? (deepReverse '()) '())
  (check-equal? (deepReverse '(1 2 3)) '(3 2 1))
  (check-equal? (deepReverse '(1 ((2) (((3) () 4) 5)))) '(((5 (4 () (3))) (2)) 1))
  )

(define deep-foldr
  (lambda (operation nv term deepList)
    (cond
      [(null? deepList) nv]
      [(atom? deepList) (term deepList)]
      [else (operation (deep-foldr operation nv term (car deepList))
                       (deep-foldr operation nv term (cdr deepList)))])))

(define branch
  (lambda (p? f g)
    (lambda (x) ((if (p? x) f g) x))))

(define foldr
  (lambda (operation nv list)
    (if (null? list)
        nv
        (operation (car list)
                   (foldr operation nv (cdr list))))))

(define deepFoldr
  (lambda (operation nv term deepList)
    (foldr operation
           nv
           (map (branch atom? term (lambda (list) (deepFoldr operation nv term list))) deepList))))

(define deepFoldl
  (lambda (operation nv term deepList)
    (foldl operation
           nv
           (map (branch atom? term (lambda (list) (deepFoldl operation nv term list))) deepList))))

(define sumAtoms
  (lambda (deepList)
    (deepFoldl + 0 id deepList)))

(module+ test
  (check-eqv? (sumAtoms '()) 0)
  (check-eqv? (sumAtoms '(1 2 3 4)) 10)
  (check-eqv? (sumAtoms '(() 1 ((2) (3 4 (())((5)))))) 15)
  )
