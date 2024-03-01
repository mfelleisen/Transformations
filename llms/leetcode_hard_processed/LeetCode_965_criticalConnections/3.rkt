#lang racket

;; Helper function to construct the graph from the connections
(define (build-graph connections)
  (let ([graph (make-hash)])
    ;; For each connection, add the edge to both vertices' adjacency lists
    (for ([connection connections])
      (let ([u (first connection)]
            [v (second connection)])
        (hash-update! graph u (lambda (l) (cons v l)) '())
        (hash-update! graph v (lambda (l) (cons u l)) '())))
    graph))

;; Main function to find critical connections
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
  (let* ([graph (build-graph connections)]  ; Build the graph
         [time (box 0)]                     ; Boxed time for increment in dfs
         [low (make-vector n -1)]           ; Lowest discovery time reachable
         [disc (make-vector n -1)]          ; Discovery times
         [ans '()])                         ; List to store critical connections

    ;; Depth-first search helper function
    (define (dfs u parent)
      (let ([u-time (unbox time)])
        (set-box! time (+ 1 u-time))
        (vector-set! low u u-time)
        (vector-set! disc u u-time)
        (for ([v (hash-ref graph u '())])
          (cond
            [(= v parent) ; Skip the edge to the parent
             (void)]
            [(= (vector-ref disc v) -1) ; If v is not discovered
             (dfs v u)
             (vector-set! low u (min (vector-ref low u) (vector-ref low v)))
             (when (> (vector-ref low v) (vector-ref disc u))
               (set! ans (cons (list u v) ans)))] ; v is a critical connection
            [else ; Update low[u] based on disc[v]
             (vector-set! low u (min (vector-ref low u) (vector-ref disc v)))])))
      )

    ;; Start DFS from vertex 0 (assuming the graph is connected)
    (dfs 0 -1)

    ;; Return the list of critical connections
    ans))

;; Example usage:
(criticalConnections 4 '((0 1) (1 2) (2 0) (1 3)))
;; Expected output: '((1 3))
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