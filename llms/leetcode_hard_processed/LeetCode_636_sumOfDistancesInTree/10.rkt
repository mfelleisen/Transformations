#lang racket

;; Function to calculate the sum of distances in a tree
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

;; Example usage is not included as per the instructions, 
;; but you can easily test the function by calling it with the provided examples.
(require rackunit)

(define (test-humaneval) 

  (let (( candidate sumOfDistancesInTree))
    (check-within (candidate 3 (list (list 0 1) (list 0 2))) (list 2 3 3) 0.001)
    (check-within (candidate 1 (list )) (list 0) 0.001)
    (check-within (candidate 2 (list (list 1 0))) (list 1 1) 0.001)
    (check-within (candidate 6 (list (list 0 1) (list 0 2) (list 2 3) (list 2 4) (list 2 5))) (list 8 12 6 10 10 10) 0.001)
))

(test-humaneval)