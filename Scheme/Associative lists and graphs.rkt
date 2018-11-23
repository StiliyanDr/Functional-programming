#lang racket/base

(define search
  (lambda (p list)
    (and (not (null? list))
         (or (p (car list))
             (search p (cdr list))))))

(define all?
  (lambda (p? list)
    (not (search (lambda (element) (not (p? element))) list))))

(define keyOf car)
(define valueOf cdr)

(define keysOf
  (lambda (list)
    (map keyOf list)))

(define valuesOf
  (lambda (list)
    (map valueOf list)))

(define associativeSearch
  (lambda (comparator?)
    (lambda (key list)
      (search (lambda (key-value)
                (and (comparator? key (keyOf key-value)) key-value)) list))))

(define assv (associativeSearch eqv?))
(define assq (associativeSearch eq?))
(define assoc (associativeSearch equal?))

(define removeAssociation
  (lambda (comparator?)
    (lambda (key list)
      (filter (lambda (key-value)
                (not (comparator? key (keyOf key-value)))) list))))

(define removeAssv (removeAssociation eqv?))
(define removeAssoc (removeAssociation equal?))
(define removeAssq (removeAssociation eq?))

(define makeKeyValue cons)

(define addAssociation
  (lambda (comparator?)
    (lambda (key value list)
      (cons (makeKeyValue key value)
            ((removeAssociation comparator?) key list)))))

(define addAssoc (addAssociation equal?))
(define addAssq (addAssociation eq?))
(define addAssv (addAssociation eqv?))


#| Graphs |#

(define verticesOf keysOf)

(define childrenOf
  (lambda (vertex graph)
    (valueOf (assv vertex graph))))

(define edge?
  (lambda (u v graph)
    (search (lambda (vertex)
              (eqv? vertex v)) (childrenOf u graph))))

(define vertex?
  (lambda (label graph)
    (search (lambda (vertex)
              (eqv? label vertex)) (verticesOf graph))))

(define mapChildren
  (lambda (function vertex graph)
    (map function (childrenOf vertex graph))))

(define searchChild
  (lambda (p vertex graph)
    (search p (childrenOf vertex graph))))

(define childlessVerticesOf
  (lambda (graph)
    (filter (lambda (vertex)
              (null? (childrenOf vertex graph))) (verticesOf graph))))

(define parentsOf
  (lambda (vertex graph)
    (filter (lambda (candidate)
              (edge? candidate vertex graph)) (verticesOf graph))))

(define symmetric?
  (lambda (graph)
    (all? (lambda (predecessor)
            (all? (lambda (successor)
                    (edge? successor predecessor))
                  (childrenOf predecessor graph)))
          (verticesOf graph))))

(define dfs-path
  (lambda (u v graph)
    (define dfs
      (lambda (path)
        (let ([current (car path)])
          (cond
            [(eqv? v current) (reverse path)]
            [(memv current (cdr path)) #f]
            [else (searchChild (lambda (child)
                                 (dfs (cons child path)))
                               current
                               graph)]))))
    (dfs (list u))))

(define safeCons
  (lambda (head tail)
    (and tail (cons head tail))))

(define DFS-path
  (lambda (u v graph)
    (define dfs
      (lambda (current visited)
        (cond
          [(eqv? current v) (list current)]
          [(memv current visited) #f]
          [else (let ([subpath (searchChild (lambda (child)
                                              (dfs child (cons current visited)))
                                            current
                                            graph)])
                  (safeCons current subpath))])))
    (dfs u '())))

(define acyclicGraph '((1 3 2)
                       (2 4)
                       (3 4 2)
                       (4)))

(define cyclicGraph '((1 2 3)
                      (2 1 3)
                      (3 1 4)
                      (4 1)
                      (5)))

(module+ test
  (require rackunit)

  (check-equal? (dfs-path 1 4 acyclicGraph) '(1 3 4))
  (check-equal? (dfs-path 1 4 cyclicGraph) '(1 2 3 4))
  (check-false (dfs-path 1 5 cyclicGraph))
  
  (check-equal? (DFS-path 1 4 acyclicGraph) '(1 3 4))
  (check-equal? (DFS-path 1 4 cyclicGraph) '(1 2 3 4))
  (check-false (DFS-path 1 5 cyclicGraph))
  )

(define bfsPath
  (lambda (start end graph)
    (define extendPath
      (lambda (path)
        (let ([vertex (car path)])
          (mapChildren (lambda (child)
                         (cons child path)) vertex graph))))
    (define remainsAcyclic?
      (lambda (path)
        (not (memv (car path) (cdr path)))))
    (define extendPathAcyclic
      (lambda (path)
        (filter remainsAcyclic? (extendPath path))))
    (define extend
      (lambda (paths)
        (apply append (map extendPathAcyclic paths))))
    (define targetPath
      (lambda (path)
        (and (eqv? (car path) end) (reverse path))))
    (define bfsLevel
      (lambda (level)
        (and (not (null? level))
             (or (search targetPath level)
                 (bfsLevel (extend level))))))
    (bfsLevel (list (list start)))))

(module+ test
  (check-equal? (bfsPath 1 4 acyclicGraph) '(1 3 4))
  (check-equal? (bfsPath 1 4 cyclicGraph) '(1 3 4))
  (check-false (bfsPath 1 5 cyclicGraph))
  )