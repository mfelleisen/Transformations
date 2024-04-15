#lang racket

;; Convert list of connections into a graph representation
(define (build-graph connections)
  (let ([graph (make-hash)])
    (for ([conn connections])
      (let ([u (car conn)]
            [v (cadr conn)])
        ;; Add edge in both directions since the graph is undirected
        (hash-update! graph u (lambda (l) (cons v l)) '())
        (hash-update! graph v (lambda (l) (cons u l)) '())))
    graph))

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
  (let ([graph (build-graph connections)]
        [ans '()]  ; Accumulate critical connections here
        [low (make-vector n -1)]  ; Low-link values
        [disc (make-vector n -1)]  ; Discovery times
        [time (vector 0)])  ; Global variable to simulate static variable in Python

    ;; Depth-first search to find bridges (critical connections)
    (define (dfs u parent)
      (vector-set! low u (vector-ref time 0))
      (vector-set! disc u (vector-ref time 0))
      (vector-set! time 0 (+ (vector-ref time 0) 1))
      (for ([v (hash-ref graph u '())])
        (when (not (= v parent))
          (if (= (vector-ref disc v) -1)  ; If v is not visited
              (begin
                (dfs v u)
                (vector-set! low u (min (vector-ref low u) (vector-ref low v)))
                (when (> (vector-ref low v) (vector-ref disc u))
                  (set! ans (cons (list u v) ans))))
              (vector-set! low u (min (vector-ref low u) (vector-ref disc v))))))
      )

    ;; Starting DFS from the first vertex assuming the graph is connected
    (dfs 0 -1)
    ans))

;; Example usage:
(criticalConnections 4 '((0 1) (1 2) (2 0) (1 3)))  ; Expected: '((1 3))
(criticalConnections 2 '((0 1)))  ; Expected: '((0 1))
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