#lang racket

(require "../testing-2.rkt")

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai13

(define (sum-of-distances-in-tree-ai13 n edges)
  ;; Convert the edge list into an adjacency list to represent the tree.
  (define tree (make-hash))
  (for-each (lambda (edge)
              (for-each (lambda (node index)
                          (hash-update! tree (list-ref edge index)
                                        (lambda (lst) (cons (list-ref edge (- 1 index)) lst))
                                        '()))
                        edge
                        '(0 1)))
            edges)

  ;; Initialize counters for each node.
  (define counts (make-vector n 1))
  (define results (make-vector n 0))

  ;; First DFS to populate counts and results with preliminary values.
  (define (dfs node parent)
    (for-each (lambda (child)
                (when (not (= child parent))
                  (dfs child node)
                  (vector-set! counts node (+ (vector-ref counts node) (vector-ref counts child)))
                  (vector-set! results node (+ (vector-ref results node) (vector-ref results child) (vector-ref counts child)))))
              (hash-ref tree node '())))
  
  ;; Second DFS to adjust the results based on the subtree sizes.
  (define (dfs2 node parent)
    (for-each (lambda (child)
                (when (not (= child parent))
                  (vector-set! results child (- (+ (vector-ref results node) (- n (vector-ref counts child))) (vector-ref counts child)))
                  (dfs2 child node)))
              (hash-ref tree node '())))

  ;; Perform the two depth-first searches.
  (dfs 0 -1)
  (dfs2 0 -1)

  ;; Convert the result vector to a list for the output.
  (vector->list results))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai12

(define (sum-of-distances-in-tree-ai12 n edges)
  ;; Create an adjacency list to represent the tree
  (define tree (make-hash))
  (for-each (lambda (edge)
              (for-each (lambda (node index)
                          (hash-update! tree (list-ref edge index)
                                        (lambda (l) (cons (list-ref edge (- 1 index)) l))
                                        '()))
                        '(0 1) '(0 1)))
            edges)

  ;; Arrays to store the count of nodes in the subtree rooted at each node
  ;; and the sum of distances to all nodes in the subtree
  (define count (make-vector n 1))
  (define res (make-vector n 0))

  ;; First DFS to populate `count` and partial `res`
  (define (dfs node parent)
    (for-each (lambda (child)
                (when (not (= child parent))
                  (dfs child node)
                  (vector-set! count node (+ (vector-ref count node) (vector-ref count child)))
                  (vector-set! res node (+ (vector-ref res node) (vector-ref res child) (vector-ref count child)))))
              (hash-ref tree node '())))
  
  ;; Second DFS to calculate final `res` values
  (define (dfs2 node parent)
    (for-each (lambda (child)
                (when (not (= child parent))
                  (vector-set! res child (- (+ (vector-ref res node) (- n (vector-ref count child))) (vector-ref count child)))
                  (dfs2 child node)))
              (hash-ref tree node '())))

  ;; Start DFS traversals from node 0
  (dfs 0 -1)
  (dfs2 0 -1)

  ;; Convert the result vector to a list and return
  (vector->list res))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai11 

(define (sum-of-distances-in-tree-ai11 n edges)
  ;; Build the tree as an adjacency list
  (define tree (make-hash))
  (for ([edge edges])
    (let ([u (first edge)]
          [v (second edge)])
      (hash-update! tree u (lambda (x) (cons v x)) '())
      (hash-update! tree v (lambda (x) (cons u x)) '())))

  ;; Initialize count and result arrays
  (define count (make-vector n 1))
  (define res (make-vector n 0))

  ;; First DFS to populate counts and partial results
  (define (dfs node parent)
    (for ([child (hash-ref tree node '())])
      (when (not (= child parent))
        (dfs child node)
        (vector-set! count node (+ (vector-ref count node) (vector-ref count child)))
        (vector-set! res node (+ (vector-ref res node) (+ (vector-ref res child) (vector-ref count child)))))))
  
  ;; Second DFS to adjust results based on subtree sizes and total node count
  (define (dfs2 node parent)
    (for ([child (hash-ref tree node '())])
      (when (not (= child parent))
        (vector-set! res child (- (+ (vector-ref res node) (- n (vector-ref count child))) (vector-ref count child)))
        (dfs2 child node))))
  
  ;; Execute both DFS traversals starting from the root (node 0)
  (dfs 0 -1)
  (dfs2 0 -1)
  
  ;; Convert the result vector to a list for the output
  (vector->list res))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai10

(define (sum-of-distances-in-tree-ai10 n edges)
  ;; Create an empty graph represented as an adjacency list
  (define tree (make-hash))
  
  ;; Helper function to add an edge to the graph
  (define (add-edge u v)
    (hash-update! tree u (lambda (x) (cons v x)) '())
    (hash-update! tree v (lambda (x) (cons u x)) '()))
  
  ;; Populate the graph with edges
  (for-each (lambda (edge)
              (add-edge (first edge) (second edge)))
            edges)
  
  ;; Arrays (lists) to store the count of nodes and result
  (define count (make-vector n 1))
  (define res (make-vector n 0))
  
  ;; Depth-First Search (DFS) to populate count and res
  (define (dfs node parent)
    (for-each (lambda (child)
                (when (not (= child parent))
                  (dfs child node)
                  (vector-set! count node (+ (vector-ref count node) (vector-ref count child)))
                  (vector-set! res node (+ (vector-ref res node) (vector-ref res child) (vector-ref count child)))))
              (hash-ref tree node '())))
  
  ;; Second DFS to adjust res based on the first DFS results
  (define (dfs2 node parent)
    (for-each (lambda (child)
                (when (not (= child parent))
                  (vector-set! res child (- (+ (vector-ref res node) (- n (vector-ref count child))) (vector-ref count child)))
                  (dfs2 child node)))
              (hash-ref tree node '())))
  
  ;; Run DFS starting from node 0
  (dfs 0 -1)
  ;; Adjust the results with a second DFS
  (dfs2 0 -1)
  
  ;; Convert the result vector to a list before returning
  (vector->list res))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai9

(define (sum-of-distances-in-tree-ai9 n edges)
  ;; Build the tree as an adjacency list
  (define tree (make-hash))
  (for ([edge edges])
    (let ([u (first edge)]
          [v (second edge)])
      (hash-update! tree u (lambda (v-list) (cons v v-list)) '())
      (hash-update! tree v (lambda (u-list) (cons u u-list)) '())))
  
  ;; Initialize count and result lists
  (define count (make-vector n 1))
  (define res (make-vector n 0))
  
  ;; First DFS to populate count and partial res
  (define (dfs node parent)
    (for ([child (hash-ref tree node '())])
      (when (not (= child parent))
        (dfs child node)
        (vector-set! count node (+ (vector-ref count node) (vector-ref count child)))
        (vector-set! res node (+ (vector-ref res node) (+ (vector-ref count child) (vector-ref res child)))))))
  
  ;; Second DFS to update res based on count
  (define (dfs2 node parent)
    (for ([child (hash-ref tree node '())])
      (when (not (= child parent))
        (vector-set! res child (- (+ (vector-ref res node) (- n (vector-ref count child))) (vector-ref count child)))
        (dfs2 child node))))
  
  ;; Perform DFS traversals starting from the root node (0)
  (dfs 0 -1)
  (dfs2 0 -1)
  
  ;; Convert the result vector to a list
  (vector->list res))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai8

(define (sum-of-distances-in-tree-ai8 n edges)
  ;; Initialize the adjacency list to represent the tree
  (define tree (make-hash))
  ;; Populate the tree with edges, ensuring bi-directional mapping
  (for-each (lambda (edge)
              (for-each (lambda (dir)
                          (hash-update! tree (list-ref edge dir) (lambda (v) (cons (list-ref edge (- 1 dir)) v)) '()))
                        '(0 1)))
            edges)

  ;; Initialize the counts and result lists
  (define counts (make-vector n 1)) ; Count of nodes in the subtree rooted at each node
  (define results (make-vector n 0)) ; Sum of distances for each node

  ;; Define DFS to populate counts and partial results
  (define (dfs node parent)
    (for-each (lambda (child)
                (when (not (= child parent))
                  (dfs child node)
                  (vector-set! counts node (+ (vector-ref counts node) (vector-ref counts child)))
                  (vector-set! results node (+ (vector-ref results node) (+ (vector-ref results child) (vector-ref counts child))))))
              (hash-ref tree node '())))

  ;; Define DFS to finalize the results based on the root node's results
  (define (dfs2 node parent)
    (for-each (lambda (child)
                (when (not (= child parent))
                  (vector-set! results child (- (+ (vector-ref results node) (- n (vector-ref counts child))) (vector-ref counts child)))
                  (dfs2 child node)))
              (hash-ref tree node '())))

  ;; Start the DFS from the root node (0) with no parent (-1)
  (dfs 0 -1)
  ;; Recalculate the results based on the first DFS
  (dfs2 0 -1)
  
  ;; Convert the results vector to a list before returning
  (vector->list results))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai7

(define (sum-of-distances-in-tree-ai7 n edges)
  ;; Create an empty adjacency list to represent the tree
  (define tree (make-hash))
  ;; Populate the tree with edges, creating an undirected graph representation
  (for-each (lambda (edge)
              (for-each (lambda (i)
                          (hash-update! tree (list-ref edge i)
                                        (lambda (v) (cons (list-ref edge (- 1 i)) v))
                                        '()))
                        '(0 1)))
            edges)
  ;; Initialize lists to store counts of nodes below each node and result distances
  (define counts (make-vector n 1))
  (define res (make-vector n 0))

  ;; Define a DFS function to populate counts and partial results
  (define (dfs node parent)
    (for-each (lambda (child)
                (when (not (= child parent))
                  (dfs child node)
                  (vector-set! counts node (+ (vector-ref counts node) (vector-ref counts child)))
                  (vector-set! res node (+ (vector-ref res node) (+ (vector-ref res child) (vector-ref counts child))))))
              (hash-ref tree node '())))
  
  ;; Define a second DFS function to update the results based on counts and previous results
  (define (dfs2 node parent)
    (for-each (lambda (child)
                (when (not (= child parent))
                  (vector-set! res child (- (+ (vector-ref res node) (- n (vector-ref counts child))) (vector-ref counts child)))
                  (dfs2 child node)))
              (hash-ref tree node '())))

  ;; Perform the first DFS from the root node
  (dfs 0 -1)
  ;; Perform the second DFS to update results correctly
  (dfs2 0 -1)

  ;; Convert the result vector to a list before returning
  (vector->list res))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai6

(define (sum-of-distances-in-tree-ai6 n edges)
  ;; Initialize a hash table to represent the tree
  (define tree (make-hash))
  ;; Populate the tree with edges, creating an undirected graph
  (for-each (lambda (edge)
              (define u (car edge))
              (define v (cadr edge))
              (hash-update! tree u (lambda (x) (cons v x)) '())
              (hash-update! tree v (lambda (x) (cons u x)) '()))
            edges)
  
  ;; Initialize count and result lists
  (define count (make-vector n 1))
  (define res (make-vector n 0))
  
  ;; Define the first depth-first search to populate count and res
  (define (dfs node parent)
    (for-each (lambda (child)
                (when (not (= child parent))
                  (dfs child node)
                  (vector-set! count node (+ (vector-ref count node) (vector-ref count child)))
                  (vector-set! res node (+ (vector-ref res node) (vector-ref res child) (vector-ref count child)))))
              (hash-ref tree node '())))
  
  ;; Define the second depth-first search to adjust res based on count
  (define (dfs2 node parent)
    (for-each (lambda (child)
                (when (not (= child parent))
                  (vector-set! res child (- (+ (vector-ref res node) (- n (vector-ref count child))) (vector-ref count child)))
                  (dfs2 child node)))
              (hash-ref tree node '())))
  
  ;; Execute the first DFS from node 0
  (dfs 0 -1)
  ;; Adjust the result for each node based on the first DFS result
  (dfs2 0 -1)
  
  ;; Convert the result vector to a list and return it
  (vector->list res))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai5

(define (sum-of-distances-in-tree-ai5 n edges)
  ;; Build the tree as an adjacency list
  (define tree (make-hash))
  (for ([edge edges])
    (let ([u (first edge)] [v (second edge)])
      (hash-update! tree u (lambda (l) (cons v l)) '())
      (hash-update! tree v (lambda (l) (cons u l)) '())))
  
  ;; Initialize count and result vectors
  (define count (make-vector n 1))
  (define res (make-vector n 0))
  
  ;; First DFS to populate count and res vectors
  (define (dfs node parent)
    (for ([child (hash-ref tree node '())])
      (when (not (= child parent))
        (dfs child node)
        (vector-set! count node (+ (vector-ref count node) (vector-ref count child)))
        (vector-set! res node (+ (vector-ref res node) (vector-ref res child) (vector-ref count child))))))
  
  ;; Second DFS to adjust res based on subtree sizes and total distance
  (define (dfs2 node parent)
    (for ([child (hash-ref tree node '())])
      (when (not (= child parent))
        (vector-set! res child (- (+ (vector-ref res node) (- n (vector-ref count child))) (vector-ref count child)))
        (dfs2 child node))))
  
  ;; Start the DFS processes
  (dfs 0 -1)
  (dfs2 0 -1)
  
  ;; Convert the result vector to a list to match the function's return type
  (vector->list res))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai4

(define (sum-of-distances-in-tree-ai4 n edges)
  ;; Create a map (hash table) to represent the tree, initializing each node to an empty list
  (define tree (make-hash))
  (for ([edge edges])
    (let ([u (first edge)] [v (second edge)])
      ;; For each edge, add the other vertex to the adjacency list of one vertex, and vice versa
      (hash-update! tree u (lambda (x) (cons v x)) '())
      (hash-update! tree v (lambda (x) (cons u x)) '())))

  ;; Initialize count and result arrays
  (define count (make-vector n 1)) ; Count of nodes subtree rooted with the node
  (define res (make-vector n 0)) ; Sum of distances

  ;; First DFS to populate count and res arrays
  (define (dfs node parent)
    (for ([child (hash-ref tree node '())])
      (when (not (= child parent))
        (dfs child node)
        ;; Update count and res for the current node using results from its children
        (vector-set! count node (+ (vector-ref count node) (vector-ref count child)))
        (vector-set! res node (+ (vector-ref res node) (vector-ref res child) (vector-ref count child))))))
  
  ;; Second DFS to update the res array based on the first DFS results
  (define (dfs2 node parent)
    (for ([child (hash-ref tree node '())])
      (when (not (= child parent))
        ;; The key idea is to update the result for a child node based on its parent's result
        (vector-set! res child (- (+ (vector-ref res node) (- n (vector-ref count child))) (vector-ref count child)))
        (dfs2 child node))))
  
  ;; Perform the two DFS traversals starting from node 0
  (dfs 0 -1)
  (dfs2 0 -1)

  ;; Convert the result vector to a list and return it
  (vector->list res))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai3

(define (sum-of-distances-in-tree-ai3 n edges)
  ;; Create a tree as a hash table where keys are node labels and values are sets of adjacent nodes
  (define tree (make-hash))
  (for ([edge edges])
    (let ([u (first edge)]
          [v (second edge)])
      (hash-update! tree u (lambda (x) (set-add (or x (set)) v)) (set))
      (hash-update! tree v (lambda (x) (set-add (or x (set)) u)) (set))))
  
  ;; Initialize counts and result lists
  (define counts (make-vector n 1))
  (define results (make-vector n 0))
  
  ;; First DFS to populate counts and partial results
  (define (dfs node parent)
    (for ([child (in-set (hash-ref tree node (set)))])
      (when (not (= child parent))
        (dfs child node)
        (vector-set! counts node
                     (+ (vector-ref counts node)
                        (vector-ref counts child)))
        (vector-set! results node
                     (+ (vector-ref results node)
                        (+ (vector-ref results child)
                           (vector-ref counts child)))))))
  
  ;; Second DFS to adjust results based on subtree sizes
  (define (dfs2 node parent)
    (for ([child (in-set (hash-ref tree node (set)))])
      (when (not (= child parent))
        (vector-set! results child
                     (+ (- (vector-ref results node)
                           (vector-ref counts child))
                        (- n (vector-ref counts child))))
        (dfs2 child node))))
  
  ;; Execute DFS traversals
  (dfs 0 -1)
  (dfs2 0 -1)
  
  ;; Convert the results vector to a list
  (vector->list results))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai2

(define (sum-of-distances-in-tree-ai2 n edges)
  ;; Convert the edge list into a tree representation using adjacency lists
  (define tree (make-hash))
  (for ([edge edges])
    (let ([u (first edge)]
          [v (second edge)])
      (hash-update! tree u (lambda (x) (cons v x)) '())
      (hash-update! tree v (lambda (x) (cons u x)) '())))
  
  ;; Initialize count and result lists
  (define count (make-vector n 1))
  (define res (make-vector n 0))
  
  ;; First DFS to compute the count and res for each node
  (define (dfs node parent)
    (for ([child (hash-ref tree node '())])
      (when (not (= child parent))
        (dfs child node)
        (vector-set! count node (+ (vector-ref count node) (vector-ref count child)))
        (vector-set! res node (+ (vector-ref res node) (+ (vector-ref res child) (vector-ref count child)))))))
  
  ;; Second DFS to update the res based on the tree structure
  (define (dfs2 node parent)
    (for ([child (hash-ref tree node '())])
      (when (not (= child parent))
        (vector-set! res child (- (+ (vector-ref res node) (- n (vector-ref count child))) (vector-ref count child)))
        (dfs2 child node))))
  
  ;; Run both DFS traversals starting from node 0
  (dfs 0 -1)
  (dfs2 0 -1)
  
  ;; Convert the result vector to a list to match the expected output
  (vector->list res))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai1

(define (sum-of-distances-in-tree-ai1 n edges)
  ;; Convert edge list to a tree represented as adjacency list.
  (define tree (make-hash))
  (for ([edge edges])
    (let ([u (car edge)]
          [v (cadr edge)])
      (hash-update! tree u (lambda (x) (cons v x)) '())
      (hash-update! tree v (lambda (x) (cons u x)) '())))

  ;; Initialize count and result lists with 0 and 1, respectively.
  (define count (make-vector n 1))
  (define res (make-vector n 0))

  ;; Depth-first search to populate count and res.
  (define (dfs node parent)
    (for ([child (hash-ref tree node '())])
      (when (not (= child parent))
        (dfs child node)
        (vector-set! count node (+ (vector-ref count node) (vector-ref count child)))
        (vector-set! res node (+ (vector-ref res node) (+ (vector-ref res child) (vector-ref count child)))))))

  ;; Second depth-first search to adjust res based on subtree sizes.
  (define (dfs2 node parent)
    (for ([child (hash-ref tree node '())])
      (when (not (= child parent))
        (vector-set! res child (- (+ (vector-ref res node) (- n (vector-ref count child))) (vector-ref count child)))
        (dfs2 child node))))

  ;; Execute the defined DFS functions starting from the root (0).
  (dfs 0 -1)
  (dfs2 0 -1)

  ;; Convert result vector to list for the output.
  (vector->list res))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai0

(define (sum-of-distances-in-tree-ai0 n edges)
  ;; Convert the edges list into a tree representation using adjacency lists.
  (define tree (make-hash))
  (for ([edge edges])
    (for ([dir (in-list (list (first edge) (second edge)))]
          #:when (not (hash-has-key? tree dir)))
      (hash-set! tree dir '()))
    (hash-set! tree (first edge) (cons (second edge) (hash-ref tree (first edge) '())))
    (hash-set! tree (second edge) (cons (first edge) (hash-ref tree (second edge) '()))))

  ;; Initialize count and res lists with n elements, count with 1s and res with 0s.
  (define count (make-vector n 1))
  (define res (make-vector n 0))

  ;; Depth-first search to populate count and res.
  ;; count[node] will hold the number of nodes in the subtree rooted at node.
  ;; res[node] will hold the sum of distances from node to all nodes in its subtree.
  (define (dfs node parent)
    (for ([child (in-list (hash-ref tree node '()))]
          #:when (not (= child parent)))
      (dfs child node)
      (vector-set! count node (+ (vector-ref count node) (vector-ref count child)))
      (vector-set! res node (+ (vector-ref res node) (+ (vector-ref res child) (vector-ref count child))))))

  ;; Second DFS to adjust res values based on the first DFS results.
  ;; It redistributes the distances from the perspective of each child node.
  (define (dfs2 node parent)
    (for ([child (in-list (hash-ref tree node '()))]
          #:when (not (= child parent)))
      (vector-set! res child (- (+ (vector-ref res node) (- n (vector-ref count child))) (vector-ref count child)))
      (dfs2 child node)))

  ;; Start the DFS traversals from the root (node 0).
  (dfs 0 -1)
  (dfs2 0 -1)

  ;; Convert the result vector to a list before returning.
  (vector->list res))


;; ---------------------------------------------------------------------------------------------------
(define MIN-NODES 1)
(define MAX-NODES (* 3 104))

;; ---------------------------------------------------------------------------------------------------
;; MODULE base

(define (find-children-base current es)
  (for/list ([e es]
             #:when (member current e))
    (match-define (list a b) e)
    (if (= current a) b a)))
 
(define (find-path-length-base start end es seen)
  (cond [(= start end) 0]
        [else
         (define maybe-path-length
           (for/or ([child (find-children-base start es)]
                    #:unless (member child seen))
             (find-path-length-base child end es (cons start seen))))
         (and maybe-path-length (+ 1 maybe-path-length))])) 
           


(define (sum-of-distances-in-tree-HIGH n es) ;; contract  sdt/c
  (for/list ([i (in-range n)])
    (for/sum ([j (in-range n)])
      (find-path-length-base i j es empty))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE accumulator

(define (find-children-accumulator current es)
  (for/fold ([children empty]              
             #:result children)
            ([e es]
             #:when (member current e))
    (match-define (list a b) e)
    (values (cons (if (= current a) b a) children))))
 
(define (find-path-length-accumulator start end es seen path-length)
  (cond [(= start end) path-length]
        [else
         (for/fold ([found #f]
                    #:result found)
                   ([child (find-children-accumulator start es)]
                    #:unless (member child seen))
           (values
            (or found
                (find-path-length-accumulator
                 child end es
                 (cons start seen)
                 (+ 1 path-length)))))]))

(define (sum-of-distances-in-tree-accumulator n es) ;; contract  sdt/c
  (for/fold ([sums empty]
             #:result sums)
            ([i (in-range n)])
    (define one-more-sum
      (for/fold ([sum 0]
                 #:result sum)
                ([j (in-range n)])
        (values (+ sum (find-path-length-accumulator i j es empty 0)))))
    (values (append sums (list one-more-sum)))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE assembly

(define (sum-of-distances-in-tree-assembly n es) ;; contract  sdt/c

  (define children (make-hash))

  (for ([e es])
    (hash-update! children
                  (first e)
                  (λ (v) (cons (second e) v))
                  empty)
    (hash-update! children
                  (second e)
                  (λ (v) (cons (first e) v))
                  empty)) 

   
  (define distances-descendents (make-hash))
  (define number-of-descendents (make-hash))
   
  (define parents (make-hash))
   
  (define distances-total (make-hash))


  (define seen empty)
  (define current 0)
  (define to-do (list current))
  (define child-d-d 0)
  (define child-n-of-d 0)     
  (define cs empty)
   
  (define (pull-info-assembly current seen)
    (define cs (hash-ref children current empty))
    (for ([child cs]
          #:unless (member child seen))
      (pull-info-assembly child (cons current seen))
      (set! child-d-d (hash-ref distances-descendents child 0))
      (set! child-n-of-d (hash-ref number-of-descendents child 0))
      (hash-update! distances-descendents
                    current
                    (λ (s) (+ s 1 child-n-of-d child-d-d ))
                    0)
      (hash-update! number-of-descendents
                    current
                    (λ (s) (+ s 1 child-n-of-d))
                    0)
      (hash-set! parents child current)))

  (pull-info-assembly 0 '())   

  (set! seen empty)
  (set! current 0)
  (set! to-do (list current))
  (define maybe-parent #f)
  (define d-d 0)
  (define n-of-d 0)
  (define t-d 0)
  (set! cs empty)
  
  (do () [(empty? to-do)]

    (set! current (first to-do))
     
    (if (member current seen)
         
        (set! to-do (rest to-do))

        (begin
          (set! seen (cons current seen))
          (set! maybe-parent
                (hash-ref parents current #f))
          (set! d-d
                (hash-ref distances-descendents current 0))
          (set! n-of-d
                (hash-ref number-of-descendents current 0))
          (set! t-d
                (if maybe-parent
                    (+ d-d
                       (- (hash-ref distances-total maybe-parent)
                          (+ 1 n-of-d d-d))
                       (- n 1 n-of-d))
                    d-d))
          (hash-set! distances-total current t-d)
          (set! cs (hash-ref children current empty))
          (set! to-do (append cs (rest to-do))))))
   

  (for/list ([i (in-range n)])
    (hash-ref distances-total i)))

;; ---------------------------------------------------------------------------------------------------
;; MODULE c

(define (sum-of-distances-in-tree-c n es) ;; contract  sdt/c

  (define children (make-hash))

  (for ([e es])
    (hash-update! children
                  (first e)
                  (λ (v) (cons (second e) v))
                  empty)
    (hash-update! children
                  (second e)
                  (λ (v) (cons (first e) v))
                  empty)) 

   
  (define distances-descendents (make-hash))
  (define number-of-descendents (make-hash))
   
  (define parents (make-hash))
   
  (define distances-total (make-hash))

  (define (pull-info-c current seen)
    (define cs (hash-ref children current empty))
    (for ([child cs]
          #:unless (member child seen))
      (pull-info-c child (cons current seen))
      (define child-d-d (hash-ref distances-descendents child 0))
      (define child-n-of-d (hash-ref number-of-descendents child 0))
      (hash-update! distances-descendents
                    current
                    (λ (s) (+ s 1 child-n-of-d child-d-d ))
                    0)
      (hash-update! number-of-descendents
                    current
                    (λ (s) (+ s 1 child-n-of-d))
                    0)
      (hash-set! parents child current)))

  (define (push-info-c current seen)
    (define maybe-parent
      (hash-ref parents current #f))
    (define d-d
      (hash-ref distances-descendents current 0))
    (define n-of-d
      (hash-ref number-of-descendents current 0))
    (define t-d
      (if maybe-parent
          (+ d-d
             (- (hash-ref distances-total maybe-parent)
                (+ 1 n-of-d d-d))
             (- n 1 n-of-d))
          d-d))
    (hash-set! distances-total current t-d)
    (define cs (hash-ref children current empty))
    (for ([child cs]
          #:unless (member child seen))
      (push-info-c child (cons current seen))))
 
 
     
  (pull-info-c 0 '())    
  (push-info-c 0 '())
   

  (for/list ([i (in-range n)])
    (hash-ref distances-total i)))
  
;; ---------------------------------------------------------------------------------------------------
;; MODULE functional-recursive

(define (find-children-functional-recursive current es)
  (cond [(empty? es) empty]
        [(member current (first es))
         (define e (first es))
         (define a (first e))
         (define b (second e))
         (define child
           (if (= current a) b a))
         (cons child (find-children-functional-recursive current (rest es)))]
        [else (find-children-functional-recursive current (rest es))]))
 
(define (find-path-length-functional-recursive start end es seen)
  (cond [(= start end) 0]
        [(member start seen) #f]
        [else
         (define children (find-children-functional-recursive start es))
         (ormap 
          (λ (child)
            (define maybe-path
              (find-path-length-functional-recursive child end es (cons start seen)))
            (and maybe-path (+ 1 maybe-path)))
          children)]))
           
 
(define (sum-of-distances-in-tree-functional-recursive nodes es) ;; contract  sdt/c
  (define (inner-functional-recursive source n)
    (if (= n 0)
        0
        (+ (find-path-length-functional-recursive source (- n 1) es empty)
           (inner-functional-recursive source (- n 1)))))
  (define (outter-functional-recursive n)
    (if (= n 0)
        empty
        (cons (inner-functional-recursive (- n 1) nodes) (outter-functional-recursive (- n 1)))))
  (reverse (outter-functional-recursive nodes)))

;; ---------------------------------------------------------------------------------------------------
;; MODULE accumulator-recursive

(define (find-children-accumulator-recursive current es children)
  (cond [(empty? es) children]
        [(member current (first es))
         (define e (first es))
         (define a (first e))
         (define b (second e))
         (define child
           (if (= current a) b a))
         (find-children-accumulator-recursive current (rest es) (cons child children))]
        [else (find-children-accumulator-recursive current (rest es) children)]))
 
(define (find-path-length-accumulator-recursive start end es seen path-length)
  (cond [(= start end) path-length]
        [(member start seen) #f]
        [else
         (define children (find-children-accumulator-recursive start es empty))
         (ormap 
          (λ (child)
            (find-path-length-accumulator-recursive child end es (cons start seen) (+ 1 path-length)))
          children)]))
 
(define (sum-of-distances-in-tree-accumulator-recursive nodes es) ;; contract  sdt/c
  (define (inner-accumulator-recursive source n)
    (if (= n 0)
        0
        (+ (find-path-length-accumulator-recursive source (- n 1) es empty 0)
           (inner-accumulator-recursive source (- n 1)))))
  (define (outter-accumulator-recursive n)
    (if (= n 0)
        empty
        (cons (inner-accumulator-recursive (- n 1) nodes) (outter-accumulator-recursive (- n 1)))))
  (reverse (outter-accumulator-recursive nodes)))

;; ---------------------------------------------------------------------------------------------------
(test
 sum-of-distances-in-tree
 in

 ai13 ai12 ai11 ai10 ai9 ai8 ai7 ai6 ai5 ai4 ai3 ai2 ai1 ai0


 HIGH
 accumulator
 functional-recursive
 accumulator-recursive
 c
 assembly
 [#:show-graph #true]
 with
    
 ;; llms's tests:
 (check-equal? (sum-of-distances-in-tree 3 (list (list 0 1) (list 0 2))) (list 2 3 3))
 (check-equal? (sum-of-distances-in-tree 1 empty) (list 0))
 (check-equal? (sum-of-distances-in-tree 2 (list (list 1 0))) (list 1 1))
 (check-equal? (sum-of-distances-in-tree 6 (list (list 0 1) (list 0 2) (list 2 3) (list 2 4) (list 2 5))) (list 8 12 6 10 10 10))

 (check-equal? (sum-of-distances-in-tree 5 (list (list 0 1) (list 0 2) (list 0 3) (list 0 4))) (list 4 7 7 7 7))
 (check-equal? (sum-of-distances-in-tree 5 (list (list 0 1) (list 1 2) (list 2 3) (list 3 4) )) (list 10 7 6 7 10))
 (check-equal? (sum-of-distances-in-tree 7 (list (list 0 1) (list 0 2) (list 1 3) (list 1 4) (list 2 5) (list 2 6)))
               (list 10 11 11 16 16 16 16))
 )


 
