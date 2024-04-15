#lang racket

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
  ;; Convert list of connections into a graph represented as an adjacency list
  (define graph (make-hash))
  (for ([conn connections])
    (let ([u (first conn)]
          [v (second conn)])
      (hash-update! graph u (lambda (lst) (cons v lst)) '())
      (hash-update! graph v (lambda (lst) (cons u lst)) '())))
  
  ;; Initialize variables to keep track of discovery times and low values
  (define disc (make-vector n -1))
  (define low (make-vector n -1))
  (define time (box 0)) ;; Use a box for mutable state
  (define ans '()) ;; List to collect critical connections
  
  ;; Depth-first search function to explore the graph
  (define (dfs u parent)
    (let ([t (unbox time)])
      (vector-set! disc u t)
      (vector-set! low u t)
      (set-box! time (+ t 1)))
    (for ([v (hash-ref graph u '())])
      (cond
        [(= v parent) (void)] ;; Skip the parent node
        [(= (vector-ref disc v) -1)
         ;; If v is not visited, recurse on it
         (dfs v u)
         ;; Update the low-link value
         (vector-set! low u (min (vector-ref low u) (vector-ref low v)))
         ;; Check if the edge is a critical connection
         (when (> (vector-ref low v) (vector-ref disc u))
           (set! ans (cons (list u v) ans)))]
        [else
         ;; Update the low value for back edges
         (vector-set! low u (min (vector-ref low u) (vector-ref disc v)))])))
  
  ;; Start DFS from the first server (assuming all servers are connected)
  (dfs 0 -1)
  
  ;; Return the list of critical connections
  ans)

;; Example calls:
(criticalConnections 4 '((0 1) (1 2) (2 0) (1 3))) ;; Output: '((1 3))
(criticalConnections 2 '((0 1))) ;; Output: '((0 1))
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