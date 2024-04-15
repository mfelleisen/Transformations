#lang racket

;; You are given an undirected graph. You are given an integer n which is the number of nodes in the graph and a list edges, where each edges[i] = [ui, vi] indicates that there is an undirected edge between ui and vi.
;; A connected trio is a set of three nodes where there is an edge between every pair of them.
;; The degree of a connected trio is the number of edges where one endpoint is in the trio, and the other is not.
;; Return the minimum degree of a connected trio in the graph, or -1 if the graph has no connected trios.
;; Example 1:
;; Input: n = 6, edges = [[1,2],[1,3],[3,2],[4,1],[5,2],[3,6]]
;; Output: 3
;; Explanation: There is exactly one trio, which is [1,2,3]. The edges that form its degree are bolded in the figure above.
;; Example 2:
;; Input: n = 7, edges = [[1,3],[4,1],[4,3],[2,5],[5,6],[6,7],[7,5],[2,6]]
;; Output: 0
;; Explanation: There are exactly three trios:
;; 1) [1,4,3] with degree 0.
;; 2) [2,5,6] with degree 2.
;; 3) [5,6,7] with degree 2.
;; Constraints:
;; 2 <= n <= 400
;; edges[i].length == 2
;; 1 <= edges.length <= n * (n-1) / 2
;; 1 <= ui, vi <= n
;; ui != vi
;; There are no repeated edges.
(define (min_trio_degree n edges)
  ;; Create a hash table to represent the graph and store degrees of nodes
  (define graph (make-hash))
  (define degrees (make-vector (add1 n) 0))
  
  ;; Populate the graph and degrees based on edges
  (for-each (lambda (edge)
              (let ((u (car edge))
                    (v (cadr edge)))
                ;; Update graph with edges
                (hash-update! graph u (lambda (v-list) (cons v v-list)) '())
                (hash-update! graph v (lambda (u-list) (cons u u-list)) '())
                ;; Update degrees
                (vector-set! degrees u (add1 (vector-ref degrees u)))
                (vector-set! degrees v (add1 (vector-ref degrees v)))))
            edges)
  
  ;; Initialize minimum degree to a large number
  (define min-degree +inf.0)
  
  ;; Iterate through each edge to find connected trios and calculate their degrees
  (for-each (lambda (edge)
              (let* ((u (car edge))
                     (v (cadr edge))
                     (u-neighbors (hash-ref graph u))
                     (v-neighbors (hash-ref graph v)))
                ;; Check if there is a common neighbor forming a trio
                (for-each (lambda (w)
                            (when (and (member w u-neighbors) (member w v-neighbors))
                              ;; Calculate and update minimum degree if necessary
                              (let ((degree-sum (- (+ (vector-ref degrees u)
                                                      (vector-ref degrees v)
                                                      (vector-ref degrees w))
                                                   6))) ;; Subtract 6 to exclude edges within the trio
                                (when (< degree-sum min-degree)
                                  (set! min-degree degree-sum)))))
                          (range 1 (add1 n)))))
            edges)
  
  ;; Return the minimum degree found or -1 if no trio exists
  (if (= min-degree +inf.0)
      -1
      min-degree))

;; Example usage:
(min_trio_degree 6 '((1 2) (1 3) (3 2) (4 1) (5 2) (3 6))) ;; => 3
(min_trio_degree 7 '((1 3) (4 1) (4 3) (2 5) (5 6) (6 7) (7 5) (2 6))) ;; => 0
(require rackunit)

(define (test-humaneval) 

  (let (( candidate min_trio_degree))
    (check-within (candidate 7 (list (list 1 3) (list 4 1) (list 4 3) (list 2 5) (list 5 6) (list 6 7) (list 7 5) (list 2 6))) 0 0.001)
    (check-within (candidate 7 (list (list 1 3) (list 4 1) (list 4 3) (list 2 5) (list 5 6) (list 6 7) (list 7 5) (list 2 6))) 0 0.001)
    (check-within (candidate 6 (list (list 1 2) (list 1 3) (list 3 2) (list 4 1) (list 5 2) (list 3 6))) 3 0.001)
    (check-within (candidate 6 (list (list 1 2) (list 1 3) (list 2 4) (list 2 5) (list 3 6))) -1 0.001)
    (check-within (candidate 7 (list (list 1 3) (list 4 1) (list 4 3) (list 2 5) (list 5 6) (list 6 7) (list 7 5) (list 2 6))) 0 0.001)
    (check-within (candidate 7 (list (list 1 2) (list 1 3) (list 3 2) (list 4 1) (list 5 2) (list 3 6) (list 4 6))) 3 0.001)
))

(test-humaneval)