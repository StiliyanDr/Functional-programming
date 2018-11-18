#lang racket/base

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

(define t '((( () 0 ()) 2 (() 2 ())) 3 ()))

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