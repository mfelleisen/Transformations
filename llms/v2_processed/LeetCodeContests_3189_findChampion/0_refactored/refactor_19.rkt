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
  ;; Function to determine the champion of a tournament represented as a DAG.
  ;; n: number of teams
  ;; edges: list of directed edges where each edge [u, v] indicates team u is stronger than team v
  
  ;; Create a hash to keep track of the number of incoming edges for each team
  (define in-degrees (make-hash))
  (for ([i (in-range n)])
    (hash-set! in-degrees i 0))
  
  ;; Populate the in-degrees hash by iterating over each edge
  (for-each (lambda (edge)
              (define v (second edge))
              (hash-update! in-degrees v add1))
            edges)
  
  ;; Find the teams with zero in-degrees
  (define zero-in-degree-teams
    (for/list ([team (in-range n)]
               #:when (= (hash-ref in-degrees team) 0))
      team))
  
  ;; Determine the result based on the number of teams with zero in-degrees
  (match zero-in-degree-teams
    [(list only-one) only-one] ;; If exactly one, return it
    [else -1])) ;; Otherwise, return -1 indicating no unique champion or no champion at all

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