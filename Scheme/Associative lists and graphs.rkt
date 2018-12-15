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

(define makeAssociativeList
  (lambda (function keys)
    (map (lambda (key)
           (makeKeyValue key (function key)))
         keys)))

#| Graphs |#

(define emptyGraph '())
(define emptyGraph? null?)
(define edgeStart car)
(define edgeEnd cdr)
(define verticesOf keysOf)

(define makeGraph
  (lambda (vertices)
    (makeAssociativeList (lambda (vertex) '())
                         vertices)))

(define makeSet
  (lambda (items)
    (foldl (lambda (uniques item)
             (if (member item uniques)
                 uniques
                 (cons item uniques)))
           '()
           items)))

(define insertTwo
      (lambda (lhs rhs items)
        (cons lhs (cons rhs items))))

(define verticesInEdges
  (lambda (edges)
    (let ([vertices (foldl (lambda (vertices edge)
                             (insertTwo (edgeStart edge) (edgeEnd edge) vertices))
                           '()
                           edges)])
      (makeSet vertices))))

(define makeFromEdges
  (lambda (edges)
    (let ([subgraph (makeGraph (verticesInEdges edges))])
      (foldl (lambda (graph edge)
               (addEdge (edgeStart edge)
                        (edgeEnd edge)
                        graph))
             subgraph
             edges))))

(define addEdge
  (lambda (start end graph)
    (if (edge? start end graph)
        graph
        (addAssoc start
                  (cons end (childrenOf start graph))
                  graph))))

(define removeEdge
  (lambda (start end graph)
    (let ([newChildren (filter (lambda (vertex)
                                 (not (eqv? vertex end)))
                               (childrenOf start graph))])
      (addAssoc start newChildren graph))))

(define edge?
  (lambda (u v graph)
    (memv? v (childrenOf u graph))))

(define memv?
  (lambda (item list)
    (search (lambda (candidate)
              (eqv? candidate item)) list)))

(define childrenOf
  (lambda (vertex graph)
    (valueOf (assv vertex graph))))

(define vertex?
  (lambda (v graph)
    (memv? v (verticesOf graph))))

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

(define addVertex
  (lambda (v graph)
    (if (vertex? v graph)
        graph
        (addAssoc v '() graph))))

(define removeVertex
  (lambda (v graph)
    (define filterPair
      (lambda (pair)
        (makeKeyValue (keyOf pair)
                      (filter (lambda (vertex)
                                (not (eqv? v vertex)))
                              (valueOf pair)))))
    (if (vertex? v graph)
        (let ([filteredGraph (map filterPair graph)])
          (removeAssv v filteredGraph))
        graph)))

(define outDegreeOf
  (lambda (vertex graph)
    (length (childrenOf vertex graph))))

(define inDegreeOf
  (lambda (vertex graph)
    (length (parentsOf vertex graph))))

(define edgesLeaving
  (lambda (vertex graph)
    (map (lambda (child)
           (cons vertex child))
         (childrenOf vertex graph))))

(define edgesOf
  (lambda (graph)
    (let ([listsOfLists (map (lambda (vertex)
                               (edgesLeaving vertex graph))
                             (verticesOf graph))])
      (apply append listsOfLists))))

(define symmetric?
  (lambda (graph)
    (all? (lambda (predecessor)
            (all? (lambda (successor)
                    (edge? successor predecessor graph))
                  (childrenOf predecessor graph)))
          (verticesOf graph))))

(define foldl
  (lambda (operation neutralElement list)
    (if (null? list)
        neutralElement
        (foldl operation
               (operation neutralElement (car list))
               (cdr list)))))

(define complementOf
  (lambda (graph)
    (define addInvertedEdge
      (lambda (subgraph edge)
        (addEdge (edgeEnd edge) (edgeStart edge) subgraph)))
    (foldl addInvertedEdge
           (makeGraph (verticesOf graph))
           (edgesOf graph))))

(define dfs-path
  (lambda (source target graph)
    (define dfs
      (lambda (path)
        (let ([current (car path)])
          (cond
            [(eqv? target current) (reverse path)]
            [(memv current (cdr path)) #f]
            [else (searchChild (lambda (child)
                                 (dfs (cons child path)))
                               current
                               graph)]))))
    (dfs (list source))))

(define safeCons
  (lambda (head tail)
    (and tail (cons head tail))))

(define DFS-path
  (lambda (source target graph)
    (define dfs
      (lambda (current visited)
        (cond
          [(eqv? current target) (list current)]
          [(memv current visited) #f]
          [else (let ([subpath (searchChild (lambda (child)
                                              (dfs child (cons current visited)))
                                            current
                                            graph)])
                  (safeCons current subpath))])))
    (dfs source '())))

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

(define path?
  (lambda (source target graph)
    (pair? (dfs-path source target graph))))

(define acyclic?
  (lambda (graph)
    (define dfsCycle?
      (lambda (current visited)
        (or (memv? current visited)
            (searchChild (lambda (child)
                           (dfsCycle? child (cons current visited)))
                         current
                         graph))))
    (or (emptyGraph? graph)
        (dfsCycle? (car (verticesOf graph)) '()))))

(define singleton?
  (lambda (list)
    (and (not (null? list))
         (null? (cdr list)))))

(define containsPath?
  (lambda (path graph)
    (let ([start (car path)])
      (if (singleton? path)
          (vertex? start graph)
          (and (vertex? start graph)
               (edge? start (cadr path) graph)
               (containsPath? (cdr path) graph))))))
