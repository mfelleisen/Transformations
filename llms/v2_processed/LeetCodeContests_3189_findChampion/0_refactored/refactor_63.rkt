#lang racket

;; There are n teams numbered from 0 to n - 1 in a tournament; each team is also a node in a DAG.
;; You are given the integer n and a 0-indexed 2D integer array edges of length m representing the DAG, where edges[i] = [ui, vi] indicates that there is a directed edge from team ui to team vi in the graph.
;; A directed edge from a to b in the graph means that team a is stronger than team b and team b is weaker than team a.
;; Team a will be the champion of the tournament if there is no team b that is stronger than team a.
;; Return the team that will be the champion of the tournament if there is a unique champion, otherwise, return -1.
;; Notes
;;  * A cycle is a series of nodes a1, a2, ..., an, an+1 such that node a1 is the same node as node an+1, the nodes a1, a2, ..., an are distinct, and there is a directed edge from the node ai to node ai+1 for every i in the range [1, n].
;;  * A DAG is a directed graph that does not have any cycle.
;; Example 1:
;; [https://assets.leetcode.com/uploads/2023/10/19/graph-3.png]
;; Input: n = 3, edges = [[0,1],[1,2]]
;; Output: 0
;; Explanation: Team 1 is weaker than team 0. Team 2 is weaker than team 1. So the champion is team 0.
;; Example 2:
;; [https://assets.leetcode.com/uploads/2023/10/19/graph-4.png]
;; Input: n = 4, edges = [[0,2],[1,3],[1,2]]
;; Output: -1
;; Explanation: Team 2 is weaker than team 0 and team 1. Team 3 is weaker than team 1. But team 1 and team 0 are not weaker than any other teams. So the answer is -1.
;; Constraints:
;;  * 1 <= n <= 100
;;  * m == edges.length
;;  * 0 <= m <= n * (n - 1) / 2
;;  * edges[i].length == 2
;;  * 0 <= edge[i][j] <= n - 1
;;  * edges[i][0] != edges[i][1]
;;  * The input is generated such that if team a is stronger than team b, team b is not stronger than team a.
;;  * The input is generated such that if team a is stronger than team b and team b is stronger than team c, then team a is stronger than team c.
(define (findChampion n edges)
  ;; Create adjacency list and in-degree vector
  (define in-degrees (make-vector n 0))
  (define adj-list (make-hash))

  ;; Populate adjacency list and in-degrees vector
  (for-each (lambda (edge)
              (define u (first edge))
              (define v (second edge))
              (hash-update! adj-list u (lambda (x) (cons v x)) '())
              (vector-set! in-degrees v (add1 (vector-ref in-degrees v))))
            edges)

  ;; Find all nodes with zero in-degrees
  (define zero-in-degree-teams
    (for/list ([i (in-range n)] #:when (= (vector-ref in-degrees i) 0))
      i))

  ;; Check if there's exactly one node with zero in-degrees
  (cond
    [(= (length zero-in-degree-teams) 1) (first zero-in-degree-teams)]
    [else -1]))

;; Example usage:
(findChampion 3 '((0 1) (1 2)))  ;; Output: 0
(findChampion 4 '((0 2) (1 3) (1 2)))  ;; Output: -1

(require rackunit)


(define (test-humaneval) 

  (let (( candidate findChampion))
    (check-within (candidate 3 (list (list 0 1) (list 1 2))) 0 0.001)
    (check-within (candidate 4 (list (list 0 2) (list 1 3) (list 1 2))) -1 0.001)
    (check-within (candidate 1 (list )) 0 0.001)
    (check-within (candidate 2 (list )) -1 0.001)
    (check-within (candidate 2 (list (list 0 1))) 0 0.001)
    (check-within (candidate 2 (list (list 1 0))) 1 0.001)
    (check-within (candidate 3 (list )) -1 0.001)
    (check-within (candidate 3 (list (list 0 1))) -1 0.001)
    (check-within (candidate 3 (list (list 0 2))) -1 0.001)
    (check-within (candidate 3 (list (list 1 2))) -1 0.001)
    (check-within (candidate 3 (list (list 2 0))) -1 0.001)
    (check-within (candidate 3 (list (list 0 1) (list 2 1))) -1 0.001)
    (check-within (candidate 3 (list (list 0 2) (list 0 1))) 0 0.001)
    (check-within (candidate 3 (list (list 0 2) (list 1 0))) 1 0.001)
    (check-within (candidate 3 (list (list 2 0) (list 1 0))) -1 0.001)
    (check-within (candidate 3 (list (list 2 0) (list 2 1))) 2 0.001)
    (check-within (candidate 3 (list (list 2 1) (list 2 0))) 2 0.001)
    (check-within (candidate 3 (list (list 0 1) (list 1 2) (list 0 2))) 0 0.001)
    (check-within (candidate 3 (list (list 0 1) (list 2 1) (list 0 2))) 0 0.001)
    (check-within (candidate 3 (list (list 0 2) (list 0 1) (list 1 2))) 0 0.001)
    (check-within (candidate 3 (list (list 0 2) (list 0 1) (list 2 1))) 0 0.001)
    (check-within (candidate 3 (list (list 0 2) (list 1 2) (list 1 0))) 1 0.001)
    (check-within (candidate 3 (list (list 1 0) (list 0 2) (list 1 2))) 1 0.001)
    (check-within (candidate 3 (list (list 2 1) (list 1 0) (list 2 0))) 2 0.001)
    (check-within (candidate 4 (list )) -1 0.001)
    (check-within (candidate 4 (list (list 0 2))) -1 0.001)
    (check-within (candidate 4 (list (list 1 0))) -1 0.001)
    (check-within (candidate 4 (list (list 1 3))) -1 0.001)
    (check-within (candidate 4 (list (list 3 2))) -1 0.001)
    (check-within (candidate 4 (list (list 0 1) (list 2 3))) -1 0.001)
    (check-within (candidate 4 (list (list 0 3) (list 2 3))) -1 0.001)
    (check-within (candidate 4 (list (list 1 3) (list 2 1))) -1 0.001)
    (check-within (candidate 4 (list (list 2 1) (list 1 3))) -1 0.001)
    (check-within (candidate 4 (list (list 3 0) (list 3 1))) -1 0.001)
    (check-within (candidate 4 (list (list 0 1) (list 2 0) (list 2 1))) -1 0.001)
    (check-within (candidate 4 (list (list 0 2) (list 3 2) (list 1 2))) -1 0.001)
    (check-within (candidate 4 (list (list 1 0) (list 2 3) (list 1 2))) 1 0.001)
    (check-within (candidate 4 (list (list 1 2) (list 0 3) (list 1 3))) -1 0.001)
    (check-within (candidate 4 (list (list 1 2) (list 1 0) (list 1 3))) 1 0.001)
    (check-within (candidate 4 (list (list 1 3) (list 1 2) (list 0 3))) -1 0.001)
    (check-within (candidate 4 (list (list 1 3) (list 3 0) (list 2 0))) -1 0.001)
    (check-within (candidate 4 (list (list 3 1) (list 2 0) (list 1 2))) 3 0.001)
    (check-within (candidate 4 (list (list 3 1) (list 2 1) (list 0 2))) -1 0.001)
    (check-within (candidate 4 (list (list 0 2) (list 0 3) (list 1 2) (list 1 0))) 1 0.001)
    (check-within (candidate 4 (list (list 2 0) (list 2 3) (list 3 1) (list 2 1))) 2 0.001)
    (check-within (candidate 4 (list (list 2 1) (list 1 0) (list 3 0) (list 2 0))) -1 0.001)
    (check-within (candidate 4 (list (list 3 0) (list 3 1) (list 2 1) (list 0 1))) -1 0.001)
    (check-within (candidate 4 (list (list 3 0) (list 1 0) (list 1 2) (list 3 2) (list 2 0))) -1 0.001)
    (check-within (candidate 4 (list (list 3 0) (list 2 0) (list 1 0) (list 2 3) (list 1 2))) 1 0.001)
    (check-within (candidate 4 (list (list 3 2) (list 0 1) (list 3 0) (list 3 1) (list 2 0))) 3 0.001)
    (check-within (candidate 4 (list (list 0 3) (list 2 3) (list 2 1) (list 1 0) (list 2 0) (list 1 3))) 2 0.001)
    (check-within (candidate 4 (list (list 1 2) (list 2 3) (list 0 2) (list 0 1) (list 0 3) (list 1 3))) 0 0.001)
    (check-within (candidate 4 (list (list 2 1) (list 3 1) (list 3 0) (list 3 2) (list 2 0) (list 0 1))) 3 0.001)
    (check-within (candidate 4 (list (list 2 3) (list 2 1) (list 0 1) (list 0 3) (list 3 1) (list 0 2))) 0 0.001)
    (check-within (candidate 5 (list )) -1 0.001)
    (check-within (candidate 5 (list (list 0 2))) -1 0.001)
    (check-within (candidate 5 (list (list 2 0))) -1 0.001)
    (check-within (candidate 5 (list (list 4 0))) -1 0.001)
    (check-within (candidate 5 (list (list 1 4) (list 2 0))) -1 0.001)
    (check-within (candidate 5 (list (list 2 0) (list 4 3))) -1 0.001)
    (check-within (candidate 5 (list (list 3 2) (list 0 2))) -1 0.001)
    (check-within (candidate 5 (list (list 3 4) (list 2 1))) -1 0.001)
    (check-within (candidate 5 (list (list 0 4) (list 1 4) (list 0 1))) -1 0.001)
    (check-within (candidate 5 (list (list 1 3) (list 4 2) (list 1 0))) -1 0.001)
    (check-within (candidate 5 (list (list 2 3) (list 4 1) (list 3 0))) -1 0.001)
    (check-within (candidate 5 (list (list 0 4) (list 2 0) (list 1 3) (list 2 4))) -1 0.001)
    (check-within (candidate 5 (list (list 0 1) (list 2 1) (list 4 1) (list 4 2) (list 4 0))) -1 0.001)
    (check-within (candidate 5 (list (list 0 2) (list 2 1) (list 3 2) (list 4 1) (list 0 4))) -1 0.001)
    (check-within (candidate 5 (list (list 2 3) (list 0 4) (list 1 4) (list 1 0) (list 4 3))) -1 0.001)
    (check-within (candidate 5 (list (list 3 1) (list 0 2) (list 4 2) (list 0 1) (list 1 2))) -1 0.001)
    (check-within (candidate 5 (list (list 3 2) (list 3 4) (list 3 0) (list 3 1) (list 0 2))) 3 0.001)
    (check-within (candidate 5 (list (list 4 0) (list 3 0) (list 2 4) (list 3 4) (list 4 1))) -1 0.001)
    (check-within (candidate 5 (list (list 4 3) (list 1 0) (list 1 2) (list 3 2) (list 4 1))) 4 0.001)
    (check-within (candidate 5 (list (list 2 1) (list 0 3) (list 0 1) (list 0 4) (list 0 2) (list 4 1))) 0 0.001)
    (check-within (candidate 5 (list (list 2 1) (list 4 1) (list 3 0) (list 2 0) (list 3 4) (list 3 2))) 3 0.001)
    (check-within (candidate 5 (list (list 0 1) (list 0 4) (list 2 0) (list 3 4) (list 3 1) (list 2 1) (list 3 0))) -1 0.001)
    (check-within (candidate 5 (list (list 1 4) (list 3 1) (list 0 1) (list 3 0) (list 0 2) (list 2 4) (list 3 4))) 3 0.001)
    (check-within (candidate 5 (list (list 3 2) (list 1 2) (list 2 0) (list 2 4) (list 1 4) (list 3 1) (list 3 4))) 3 0.001)
    (check-within (candidate 5 (list (list 0 4) (list 0 3) (list 4 3) (list 4 2) (list 1 2) (list 4 1) (list 0 1) (list 3 2))) 0 0.001)
    (check-within (candidate 5 (list (list 4 3) (list 4 2) (list 4 1) (list 2 3) (list 4 0) (list 3 1) (list 2 0) (list 0 3))) 4 0.001)
    (check-within (candidate 5 (list (list 1 2) (list 1 4) (list 2 3) (list 0 2) (list 1 0) (list 1 3) (list 0 3) (list 4 3) (list 0 4))) 1 0.001)
    (check-within (candidate 5 (list (list 1 3) (list 3 0) (list 3 4) (list 2 0) (list 3 2) (list 0 4) (list 2 4) (list 1 0) (list 1 2))) 1 0.001)
    (check-within (candidate 5 (list (list 3 0) (list 4 0) (list 3 2) (list 0 1) (list 0 2) (list 4 3) (list 1 2) (list 4 2) (list 3 1))) 4 0.001)
    (check-within (candidate 5 (list (list 4 0) (list 2 3) (list 4 3) (list 4 2) (list 2 0) (list 4 1) (list 1 3) (list 1 0) (list 3 0))) 4 0.001)
    (check-within (candidate 5 (list (list 0 2) (list 1 3) (list 4 1) (list 4 2) (list 2 1) (list 0 3) (list 0 1) (list 2 3) (list 0 4) (list 4 3))) 0 0.001)
    (check-within (candidate 5 (list (list 2 0) (list 4 0) (list 3 4) (list 4 2) (list 1 2) (list 1 0) (list 3 1) (list 3 0) (list 1 4) (list 3 2))) 3 0.001)
    (check-within (candidate 6 (list )) -1 0.001)
    (check-within (candidate 6 (list (list 2 1))) -1 0.001)
    (check-within (candidate 6 (list (list 2 3))) -1 0.001)
    (check-within (candidate 6 (list (list 3 5))) -1 0.001)
    (check-within (candidate 6 (list (list 5 4))) -1 0.001)
    (check-within (candidate 6 (list (list 1 2) (list 4 3))) -1 0.001)
    (check-within (candidate 6 (list (list 0 4) (list 4 5) (list 3 1))) -1 0.001)
    (check-within (candidate 6 (list (list 0 4) (list 5 2) (list 5 4) (list 3 0) (list 1 0))) -1 0.001)
    (check-within (candidate 6 (list (list 1 0) (list 1 4) (list 0 2) (list 3 5) (list 3 4))) -1 0.001)
    (check-within (candidate 6 (list (list 5 0) (list 2 4) (list 0 4) (list 3 2) (list 2 0))) -1 0.001)
    (check-within (candidate 6 (list (list 1 2) (list 3 2) (list 3 5) (list 4 0) (list 1 5) (list 0 5))) -1 0.001)
    (check-within (candidate 6 (list (list 3 2) (list 1 2) (list 3 0) (list 5 0) (list 5 1) (list 5 3))) -1 0.001)
    (check-within (candidate 6 (list (list 4 2) (list 0 4) (list 1 3) (list 3 4) (list 1 2) (list 1 0))) -1 0.001)
    (check-within (candidate 6 (list (list 0 5) (list 2 0) (list 1 5) (list 3 2) (list 2 1) (list 2 4) (list 4 5))) 3 0.001)
))

(test-humaneval)