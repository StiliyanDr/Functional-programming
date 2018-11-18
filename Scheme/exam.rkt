#lang racket/base

(define ++
  (lambda (number)
    (+ number 1)))

(define --
  (lambda (number)
    (- number 1)))

(define removeLastDigitOf
  (lambda (number)
    (quotient number 10)))

(define lastDigitOf
  (lambda (number)
    (modulo number 10)))

(define digitsCount
  (lambda (number)
    (define utility
      (lambda (rest count)
        (if (< rest 10)
            (++ count)
            (utility (removeLastDigitOf rest)
                     (++ count)))))
    (utility number 0)))

(define digitN
  (lambda (number n)
    (if (= n 1)
        (lastDigitOf number)
        (digitN (removeLastDigitOf number) (-- n)))))

(define middleDigit
  (lambda (number)
    (let* ([count (digitsCount number)]
           [middle (++ (quotient count 2))])
      (if (odd? count)
          (digitN number middle)
          -1))))

(module+ test
  (require rackunit)
  
  (check-eqv? (middleDigit 0) 0)
  (check-eqv? (middleDigit 12) -1)
  (check-eqv? (middleDigit 12345) 3)
  )

(define id
  (lambda (x) x))

(define negate
  (lambda (x) (- x)))

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

(define meetTwice?
  (lambda (f g start end)
    (>= (accumulate-i + 0 start end (lambda (i) (if (= (f i) (g i)) 1 0)) ++)
        2)))

(module+ test
  (check-false (meetTwice? id negate 0 9))
  (check-true (meetTwice? id (lambda (x) (* x x)) 0 5))
  (check-true (meetTwice? id id 1 5))
  )

(define member
  (lambda (item list)
    (cond
      [(null? list) #f]
      [(equal? item (car list)) list]
      [else (member item (cdr list))])))

(define ElementsMapInList?
  (lambda (list f)
    (foldl (lambda (result element)
             (and result (member (f element) list))) #t list)))

(define elementsMapInList?
  (lambda (list f)
    (all? (lambda (element) (member (f element) list)) list)))

(define all?
  (lambda (p? list)
    (or (null? list)
        (and (p? (car list))
             (all? p? (cdr list))))))

(define complement
  (lambda (p?)
    (lambda (argument)
      (not (p? argument)))))

(define any?
  (lambda (p? list)
    (not (all? (complement p?) list))))

(define condition?
  (lambda (list f g)
    (define predicate?
      (lambda (element1)
        (all? (lambda (element2)
                (= [g (f element1) (f element2)]
                   [f (g element1 element2)])) list)))
    (all? predicate? list)))

(define is-em?
  (lambda (list g f)
    (and (elementsMapInList? list f)
         (condition? list f g))))

(define foldl
  (lambda (operation neutralElement list)
    (if (null? list)
        neutralElement
        (foldl operation
               (operation neutralElement (car list))
               (cdr list)))))

(define insertTwo
  (lambda (x y list)
    (cons x (cons y list))))

(define lookAndSay
  (lambda (list)
    (define generate
      (lambda (rest current length generated)
        (cond
          [(null? rest) (insertTwo current length generated)]
          [(equal? current (car rest)) (generate (cdr rest) current (++ length) generated)]
          [else (generate rest (car rest) 0 (insertTwo current length generated))])))
    (if (null? list)
        list
        (reverse (generate list (car list) 0 '())))))

(module+ test
  (check-equal? (lookAndSay '()) '())
  (check-equal? (lookAndSay '(9)) '(1 9))
  (check-equal? (lookAndSay '(1 1 2 3 3 3 1 4 4 4)) '(2 1 1 2 3 3 1 1 3 4))
  )