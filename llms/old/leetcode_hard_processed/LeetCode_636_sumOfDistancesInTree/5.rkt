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

;; The function sumOfDistancesInTree is now defined and ready to use.
;; It first constructs the tree, then uses two depth-first search (DFS) passes to calculate
;; the sum of distances from each node to all other nodes.
;; This approach efficiently computes the desired sums by leveraging the properties of the tree.
(require rackunit)

(define (test-humaneval) 

  (let (( candidate sumOfDistancesInTree))
    (check-within (candidate 3 (list (list 0 1) (list 0 2))) (list 2 3 3) 0.001)
    (check-within (candidate 1 (list )) (list 0) 0.001)
    (check-within (candidate 2 (list (list 1 0))) (list 1 1) 0.001)
    (check-within (candidate 6 (list (list 0 1) (list 0 2) (list 2 3) (list 2 4) (list 2 5))) (list 8 12 6 10 10 10) 0.001)
))

(test-humaneval)