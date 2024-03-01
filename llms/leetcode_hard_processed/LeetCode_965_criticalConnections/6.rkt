#lang racket

;; Defines a function to find all critical connections in a network.
;; There are n servers numbered from 0 to n - 1 connected by undirected server-to-server connections forming a network where connections[i] = [ai, bi] represents a connection between servers ai and bi. Any server can reach other servers directly or indirectly through the network.
;; A critical connection is a connection that, if removed, will make some servers unable to reach some other server.
;; Return all critical connections in the network in any order.
;; Example 1:
;; Input: n = 4, connections = [[0,1],[1,2],[2,0],[1,3]]
;; Output: [[1,3]]
;; Explanation: [[3,1]] is also accepted.
;; Example 2:
;; Input: n = 2, connections = [[0,1]]
;; Output: [[0,1]]
;; Constraints:
;; 2 <= n <= 105
;; n - 1 <= connections.length <= 105
;; 0 <= ai, bi <= n - 1
;; ai != bi
;; There are no repeated connections.
(define (criticalConnections n connections)
  ;; Initializes an adjacency list to represent the graph.
  (define graph (make-hash))
  (for ([conn connections])
    (for ([u (in-list conn)])
      (hash-update! graph u (lambda (v) (cons (if (equal? u (first conn)) (second conn) (first conn)) v)) '())))
  
  ;; Variables to track discovery times and low values of nodes.
  (define disc (make-vector n -1))
  (define low (make-vector n -1))
  (define time (box 0)) ; Box is used to allow mutation.
  (define ans '()) ; List to store critical connections.
  
  ;; Depth-first search function to explore the graph.
  (define (dfs u parent)
    (vector-set! disc u (unbox time))
    (vector-set! low u (unbox time))
    (set-box! time (+ 1 (unbox time)))
    (for ([v (in-list (hash-ref graph u '()))])
      (cond
        [(equal? v parent) (void)] ; Skip if parent node.
        [(equal? (vector-ref disc v) -1) ; If unvisited.
         (dfs v u) ; Recurse with v as the current node and u as its parent.
         (vector-set! low u (min (vector-ref low u) (vector-ref low v))) ; Update low[u].
         (when (> (vector-ref low v) (vector-ref disc u)) ; Check for critical connection.
           (set! ans (cons (list u v) ans)))]
        [else ; For visited vertices.
         (vector-set! low u (min (vector-ref low u) (vector-ref disc v)))])) ; Update low[u].
    )
  
  ;; Start DFS from the first node assuming the graph is connected.
  (dfs 0 -1)
  
  ;; Return the list of critical connections.
  ans
)

;; Example usage
(define n 4)
(define connections '((0 1) (1 2) (2 0) (1 3)))
(criticalConnections n connections)
(require rackunit)

(define (test-humaneval) 

  (let (( candidate criticalConnections))
    (check-within (candidate 3 (list (list 0 1) (list 1 2) (list 2 0))) (list ) 0.001)
    (check-within (candidate 4 (list (list 0 1) (list 1 2) (list 2 0) (list 1 3))) (list (list 1 3)) 0.001)
    (check-within (candidate 8 (list (list 0 1) (list 1 2) (list 2 3) (list 3 4) (list 4 5) (list 5 6) (list 6 7) (list 7 0))) (list ) 0.001)
    (check-within (candidate 8 (list (list 0 1) (list 1 2) (list 2 3) (list 3 4) (list 4 5) (list 5 6) (list 6 7) (list 7 0) (list 1 7))) (list ) 0.001)
    (check-within (candidate 5 (list (list 0 1) (list 1 2) (list 2 3) (list 3 4) (list 4 0) (list 0 3))) (list ) 0.001)
    (check-within (candidate 4 (list (list 0 1) (list 1 2) (list 2 3) (list 3 0) (list 2 0))) (list ) 0.001)
    (check-within (candidate 6 (list (list 0 1) (list 1 2) (list 2 3) (list 3 4) (list 4 5) (list 5 0))) (list ) 0.001)
    (check-within (candidate 5 (list (list 0 1) (list 1 2) (list 2 3) (list 3 0) (list 0 4) (list 4 1) (list 1 4))) (list ) 0.001)
    (check-within (candidate 7 (list (list 0 1) (list 1 2) (list 2 3) (list 3 4) (list 4 5) (list 5 6) (list 6 0))) (list ) 0.001)
    (check-within (candidate 5 (list (list 0 1) (list 1 2) (list 2 3) (list 3 4) (list 4 0))) (list ) 0.001)
))

(test-humaneval)