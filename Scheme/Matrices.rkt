#lang racket/base

(define --
  (lambda (number)
    (- number 1)))

(define ++
  (lambda (number)
    (+ number 1)))

(define append
  (lambda lists
    (cond
      [(null? lists) '()]
      [(null? (car lists)) (apply append (cdr lists))]
      [else (cons (caar lists) (apply append (cons (cdar lists) (cdr lists))))])))

(module+ test
  (require rackunit)
  
  (check-equal? (append) '())
  (check-equal? (append '()) '())
  (check-equal? (append '(a) '(b)) '(a b))
  (check-equal? (append '(1) '() '(2 3)) '(1 2 3))
  )

(define m '((1 2 3) (4 5 6) (7 8 9)))

(define all?
  (lambda (p? list)
    (or (null? list)
        (and (p? (car list))
             (all? p? (cdr list))))))

(define nonemptyList?
  (lambda (ls)
    (and (list? ls) (nonempty? ls))))

(define nonempty?
  (lambda (list)
    (not (null? list))))

(define matrix?
  (lambda (m)
    (and (nonemptyList? m)
         (all? nonemptyList? m)
         (let ([columnsCount (length (car m))])
           (all? (lambda (row) (= columnsCount (length row))) m)))))

(module+ test
  (check-false (matrix? '()))
  (check-false (matrix? '(() ())))
  (check-false (matrix? '((1 2 3) (4 5))))
  (check-false (matrix? '((1 2 3) (5 . 4))))
  (check-true (matrix? '((1 2 3)
                        (4 5 6)
                        (7 8 9))))
  )

(define rowsCount length)

(define columnsCount
  (lambda (matrix)
    (length (car matrix))))

(define getRow
  (lambda (index matrix)
    (list-ref matrix index)))

(define getColumn
  (lambda (index matrix)
    (map (lambda (row) (list-ref row index)) matrix)))

(define list-remove
  (lambda (index list)
    (define utility
      (lambda (skipped rest i)
        (if (zero? i)
            (append (reverse skipped) (cdr rest))
            (utility (cons (car rest) skipped) (cdr rest) (- i 1)))))
    (utility '() list index)))

(module+ test
  (check-equal? (list-remove 0 '(0 1 2)) '(1 2))
  (check-equal? (list-remove 1 '(0 1 2)) '(0 2))
  (check-equal? (list-remove 2 '(0 1 2)) '(0 1))
  )

(define removeRow list-remove)

(define removeColumn
  (lambda (index matrix)
    (map (lambda (row) (list-remove index row)) matrix)))

(module+ test
  (check-equal? (removeColumn 0 m) '((2 3) (5 6) (8 9)))
  (check-equal? (removeColumn 2 m) '((1 2) (4 5) (7 8)))
  )

(define transponse-rec
  (lambda (matrix)
    (if (null? (getRow 0 matrix))
        '()
        (cons (getColumn 0 matrix)
              (transponse-rec (removeColumn 0 matrix))))))

(define accumulate-i
  (lambda (operation nv start end term step)
    (if (> start end)
        nv
        (accumulate-i operation
                      (operation nv (term start))
                      (step start)
                      end
                      term
                      step))))

(define transponse
  (lambda (matrix)
    (reverse (accumulate-i (lambda (tail head) (cons head tail))
                           '()
                           0
                           (-- (columnsCount matrix))
                           (lambda (i) (getColumn i matrix))
                           ++))))


(module+ test
  (check-equal? (transponse '((1 2 3))) '((1) (2) (3)))
  (check-equal? (transponse m) '((1 4 7) (2 5 8) (3 6 9)))
  )

(define sumVectors
  (lambda (u v)
    (map + u v)))

(define sumMatrices
  (lambda (a b)
    (map sumVectors a b)))

(module+ test
  (check-equal? (sumMatrices m '((0 0 0) (0 0 0) (0 0 0))) m)
  (check-equal? (sumMatrices m '((1 1 1) (0 0 0) (1 2 3))) '((2 3 4) (4 5 6) (8 10 12)))
  )

(define scaleVector
  (lambda (scalar vector)
    (map (lambda (i) (* scalar i)) vector)))

(define scaleMatrix
  (lambda (scalar matrix)
    (map (lambda (row) (scaleVector scalar row)) matrix)))

(module+ test
  (check-equal? (scaleMatrix 1 m) m)
  (check-equal? (scaleMatrix 10 m) '((10 20 30) (40 50 60) (70 80 90)))
  )

(define multiplyVectors
  (lambda (u v)
    (apply + (map * u v))))

(define multiplyMatrices
  (lambda (a b)
    (let ([b^t (transponse b)])
      (map (lambda (row-i)
             (map (lambda (col-j)
                    (multiplyVectors row-i col-j))
                  b^t))
           a))))

(define E '((1 0 0) (0 1 0) (0 0 1)))

(module+ test
  (check-equal? (multiplyMatrices m E) m)
  (check-equal? (multiplyMatrices E m) m)
  (check-equal? (multiplyMatrices (scaleMatrix 5 E) m) (scaleMatrix 5 m))
  )