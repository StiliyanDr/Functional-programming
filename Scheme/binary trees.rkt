#lang racket/base

(define t '(((() 2 ()) 3 (() 4 ())) 5 ((() 6 ()) 7 (() 8 ()))))

(define --
  (lambda (number)
    (- number 1)))

(define ++
  (lambda (number)
    (+ number 1)))

(define emptyTree '())

(define emptyTree? null?)

(define tree?
  (lambda (tree)
    (or (emptyTree? tree)
        (and (list? tree)
             (= (length tree) 3)
             (tree? (car tree))
             (tree? (caddr tree))))))

(define makeTree
  (lambda (leftSubtree root rightSubtree)
    (list leftSubtree root rightSubtree)))

(define makeLeaf
  (lambda (item)
    (makeTree emptyTree item emptyTree)))

(define rootOf cadr)
(define leftSubtreeOf car)
(define rightSubtreeOf caddr)

(define depth
  (lambda (tree)
    (if (emptyTree? tree)
        0
        (+ 1 (max (depth (leftSubtreeOf tree))
                  (depth (rightSubtreeOf tree)))))))

#| Assuming it is a BST |#

(define subtreeWithRoot
  (lambda (item tree less?)
    (cond
      [(emptyTree? tree) #f]
      [(equal? (rootOf tree) item) tree]
      [else (subtreeWithRoot item ((if (less? item (rootOf tree))
                                       leftSubtreeOf
                                       rightSubtreeOf)
                                   tree) less?)])))

#| Just binary trees |#

(define path
  (lambda (item tree)
    (cond
      [(emptyTree? tree) #f]
      [(equal? item (rootOf tree)) (list item)]
      [else (safeCons (rootOf tree) (or (path item (leftSubtreeOf tree))
                                        (path item (rightSubtreeOf tree))))])))

(define safeCons
  (lambda (head tail)
    (and tail (cons head tail))))

(define sumTree
  (lambda (tree)
    (if (emptyTree? tree)
        0
        (+ (rootOf tree)
           (sumTree (leftSubtreeOf tree))
           (sumTree (rightSubtreeOf tree))))))

(define treeLevel
  (lambda (level tree)
    (cond
      [(emptyTree? tree) '()]
      [(zero? level) (list (rootOf tree))]
      [else (append (treeLevel (-- level) (leftSubtreeOf tree))
                    (treeLevel (-- level) (rightSubtreeOf tree)))])))

(define accumulate-i
  (lambda (operation neutralElement start end term step)
    (if (> start end)
        neutralElement
        (accumulate-i operation
                      (operation neutralElement (term start))
                      (step start)
                      end
                      term
                      step))))

(define allLevels
  (lambda (tree)
    (reverse (accumulate-i (lambda (accumulated level) (cons level accumulated))
                           '()
                           0
                           (-- (depth tree))
                           (lambda (i) (treeLevel i tree))
                           ++))))

(define mapTree
  (lambda (function tree)
    (if (emptyTree? tree)
        emptyTree
        (makeTree (mapTree function (leftSubtreeOf tree))
                  (function (rootOf tree))
                  (mapTree function (rightSubtreeOf tree))))))

(define treeToList
  (lambda (tree)
    (if (emptyTree? tree)
        '()
        (let ([left (treeToList (leftSubtreeOf tree))]
              [right (treeToList (rightSubtreeOf tree))])
          (append left (cons (rootOf tree) right))))))

(define insert
  (lambda (item tree less?)
    (cond
      [(emptyTree? tree) (makeLeaf item)]
      [(less? item (rootOf tree)) (let ([leftSubtree (insert item (leftSubtreeOf tree) less?)])
                                    (makeTree leftSubtree (rootOf tree) (rightSubtreeOf tree)))]
      [else (let ([rightSubtree (insert item (rightSubtreeOf tree) less?)])
              (makeTree (leftSubtreeOf tree) (rootOf tree) rightSubtree))])))

(define foldl
  (lambda (operation neutralElement list)
    (if (null? list)
        neutralElement
        (foldl operation
               (operation neutralElement (car list))
               (cdr list)))))

(define listToTree
  (lambda (list)
    (foldl (lambda (tree item) (insert item tree <))
           emptyTree
           list)))

(define treeSort
  (lambda (list)
    (treeToList (listToTree list))))

(define makeTreeOptimal
  (lambda (compare neutralElement)
    (letrec ([treeOptimal (lambda (tree)
                            (if (emptyTree? tree)
                                neutralElement
                                (compare (rootOf tree)
                                         (treeOptimal (leftSubtreeOf tree))
                                         (treeOptimal (rightSubtreeOf tree)))))])
      treeOptimal)))

(define optimalOfThree
  (lambda (comparator)
    (lambda (a b c)
      (comparator a (comparator b c)))))

(define treeMax (makeTreeOptimal (optimalOfThree max) -inf.0))
(define treeMin (makeTreeOptimal (optimalOfThree min) +inf.0))

(define binarySearchTree?
  (lambda (tree)
    (and (tree? tree)
         (or (emptyTree? tree)
             (let ([max (treeMax (leftSubtreeOf tree))]
                   [min (treeMin (rightSubtreeOf tree))])
               (and (>= (rootOf tree) max)
                    (<= (rootOf tree) min)
                    (binarySearchTree? (leftSubtreeOf tree))
                    (binarySearchTree? (rightSubtreeOf tree))))))))

(define makeBSTOptimal
  (lambda (direction neutralElement)
    (letrec ([bstOptimal (lambda (tree)
                           (cond
                             [(emptyTree? tree) neutralElement]
                             [(emptyTree? (direction tree)) (rootOf tree)]
                             [else (bstOptimal (direction tree))]))])
      bstOptimal)))

(define bstMin (makeBSTOptimal leftSubtreeOf +inf.0))
(define bstMax (makeBSTOptimal rightSubtreeOf -inf.0))

(define splitAt
  (lambda (index items)
    (define utility
      (lambda (skipped i rest)
        (if (or [null? rest] [zero? i])
            (let ([left (reverse skipped)])
              (cons left rest))
            (utility (cons (car rest) skipped)
                     (-- i)
                     (cdr rest)))))
    (utility '() index items)))

(define bstFromSortedList
  (lambda (items)
    (let ([count (length items)])
      (if (zero? count)
          emptyTree
          (let* ([middle (quotient count 2)]
                 [split (splitAt middle items)]
                 [left (car split)]
                 [right (cddr split)]
                 [root (cadr split)])
            (makeTree (bstFromSortedList left)
                      root
                      (bstFromSortedList right)))))))
