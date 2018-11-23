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
  (lambda (p? stream)
    (cond
      [(emptyStream? stream) #f]
      [(p? (headOf stream)) stream]
      [else (searchStream p? (tailOf stream))])))


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
        (consStream (apply function (cons (headOf stream) (map headOf streams)))
                    (apply mapStreams (cons function (cons (tailOf stream) (map tailOf streams))))))))