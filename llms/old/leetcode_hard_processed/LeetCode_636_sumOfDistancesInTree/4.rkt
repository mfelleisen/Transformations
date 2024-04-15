#lang racket

;; There is an undirected connected tree with n nodes labeled from 0 to n - 1 and n - 1 edges.
;; You are given the integer n and the list edges where edges[i] = [ai, bi] indicates that there is an edge between nodes ai and bi in the tree.
;; Return a list answer of length n where answer[i] is the sum of the distances between the ith node in the tree and all other nodes.
;; Example 1:
;; Input: n = 6, edges = [[0,1],[0,2],[2,3],[2,4],[2,5]]
;; Output: [8,12,6,10,10,10]
;; Explanation: The tree is shown above.
;; We can see that dist(0,1) + dist(0,2) + dist(0,3) + dist(0,4) + dist(0,5)
;; equals 1 + 1 + 2 + 2 + 2 = 8.
;; Hence, answer[0] = 8, and so on.
;; Example 2:
;; Input: n = 1, edges = []
;; Output: [0]
;; Example 3:
;; Input: n = 2, edges = [[1,0]]
;; Output: [1,1]
;; Constraints:
;; 1 <= n <= 3 * 104
;; edges.length == n - 1
;; edges[i].length == 2
;; 0 <= ai, bi < n
;; ai != bi
;; The given input represents a valid tree.
(define (sumOfDistancesInTree n edges)
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

;; Examples (you can test the function with these or your own examples)
;; Note: Since Racket is a functional programming language, this solution emphasizes immutability where possible and uses Racket-specific data structures and idioms.
(require rackunit)

(define (test-humaneval) 

  (let (( candidate sumOfDistancesInTree))
    (check-within (candidate 3 (list (list 0 1) (list 0 2))) (list 2 3 3) 0.001)
    (check-within (candidate 1 (list )) (list 0) 0.001)
    (check-within (candidate 2 (list (list 1 0))) (list 1 1) 0.001)
    (check-within (candidate 6 (list (list 0 1) (list 0 2) (list 2 3) (list 2 4) (list 2 5))) (list 8 12 6 10 10 10) 0.001)
))

(test-humaneval)