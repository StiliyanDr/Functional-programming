#lang racket

(define ++
  (lambda (number)
    (+ number 1)))

(define --
  (lambda (number)
    (- number 1)))

(define theEmptyStream '())

(define emptyStream? null?)

(define headOf car)

(define tailOf
  (lambda (stream)
    (force (cdr stream))))

(define-syntax consStream
  (syntax-rules ()
    ((consStream h t) (cons h (delay t)))))

(define rangeToStream
  (lambda (start end)
    (if (> start end)
        theEmptyStream
        (consStream start
                    (rangeToStream (++ start) end)))))

(define take
  (lambda (number stream)
    (if (or [emptyStream? stream] [zero? number])
        theEmptyStream
        (cons (headOf stream)
              (take (-- number) (tailOf stream))))))

(define searchStream
  (lambda (p stream)
    (and (not (emptyStream? stream))
         (or (p (headOf stream))
             (searchStream p (tailOf stream))))))

(define from
  (lambda (number)
    (consStream number
                (from (++ number)))))

(define naturalNumbers (from 0))

(define generateFibs
  (lambda (a b)
    (consStream a (generateFibs b (+ a b)))))

(define fibonacciNumbers (generateFibs 0 1))

(define filterStream
  (lambda (p? stream)
    (cond
      [(emptyStream? stream) theEmptyStream]
      [(p? (headOf stream)) (consStream (headOf stream)
                                        (filterStream p? (tailOf stream)))]
      [else (filterStream p? (tailOf stream))])))

(define mapStreams
  (lambda (function stream . streams)
    (define mapSingle
      (lambda (function stream)
        (if (emptyStream? stream)
            theEmptyStream
            (consStream (function (headOf stream))
                        (mapSingle function (tailOf stream))))))
    (if (or (emptyStream? stream) (null? streams))
        (mapSingle function stream)
        (let ([heads (cons (headOf stream) (map headOf streams))]
              [tails (cons (tailOf stream) (map tailOf streams))])
          (consStream (apply function heads)
                      (apply mapStreams (cons function tails)))))))

(define ones (consStream 1 ones))

(define fibs
  (consStream 0
              (consStream 1
                          (mapStreams + fibs (tailOf fibs)))))

(define nats (consStream 0 (mapStreams ++ nats)))

(define divides
  (lambda (candidate number)
    (zero? (modulo number candidate))))

(define notDivides
  (lambda (number)
    (lambda (candidate)
      (not (divides number candidate)))))

(define sieve
  (lambda (stream)
    (consStream (headOf stream)
                (sieve (filterStream (notDivides (headOf stream)) (tailOf stream))))))

(define primes (sieve (from 2)))

(define repeat
  (lambda (item)
    (consStream item
                (repeat item))))

(define cycle
  (lambda (items)
    (define iterate
      (lambda (remaining)
        (if (null? remaining)
            (iterate items)
            (consStream (car remaining)
                        (iterate (cdr remaining))))))
    (if (null? items)
        theEmptyStream
        (iterate items))))

(define iterate
  (lambda (function x)
    (consStream x
                (mapStreams function
                            (iterate function x)))))