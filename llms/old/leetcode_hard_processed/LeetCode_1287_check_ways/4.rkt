#lang racket

;; Function to check the number of ways to form a rooted tree from given pairs.
;; You are given a list pairs, where pairs[i] = [xi, yi], and:
;; There are no duplicates.
;; xi < yi
;; Let ways be the number of rooted trees that satisfy the following conditions:
;; The tree consists of nodes whose values appeared in pairs.
;; A pair [xi, yi] exists in pairs if and only if xi is an ancestor of yi or yi is an ancestor of xi.
;; Note: the tree does not have to be a binary tree.
;; Two ways are considered to be different if there is at least one node that has different parents in both ways.
;; Return:
;; 0 if ways == 0
;; 1 if ways == 1
;; 2 if ways > 1
;; A rooted tree is a tree that has a single root node, and all edges are oriented to be outgoing from the root.
;; An ancestor of a node is any node on the path from the root to that node (excluding the node itself). The root has no ancestors.
;; Example 1:
;; Input: pairs = [[1,2],[2,3]]
;; Output: 1
;; Explanation: There is exactly one valid rooted tree, which is shown in the above figure.
;; Example 2:
;; Input: pairs = [[1,2],[2,3],[1,3]]
;; Output: 2
;; Explanation: There are multiple valid rooted trees. Three of them are shown in the above figures.
;; Example 3:
;; Input: pairs = [[1,2],[2,3],[2,4],[1,5]]
;; Output: 0
;; Explanation: There are no valid rooted trees.
;; Constraints:
;; 1 <= pairs.length <= 105
;; 1 <= xi < yi <= 500
;; The elements in pairs are unique.
(define (check_ways pairs)
  ;; Use a hash table to count occurrences of each node.
  (define candidates (make-hash))
  ;; Iterate through each pair and update the count of occurrences for both nodes in the pair.
  (for-each (lambda (pair)
              (for-each (lambda (x)
                          ;; Increment the count for node x, defaulting to 0 if not found.
                          (hash-update! candidates x add1 0))
                        pair))
            pairs)
  ;; Calculate the number of nodes with only one occurrence, which are potential roots.
  (define root-count (count (lambda (v) (= v 1)) (hash-values candidates)))
  ;; If there is more than one potential root, there are multiple ways to form a tree, return 2.
  ;; If there is exactly one, there is only one way, return 1.
  ;; Otherwise, there are no valid trees, return 0.
  (min root-count 2))

;; Helper function to add numbers, used in hash-update! to increment counts.
(define (add1 x) (+ x 1))
(require rackunit)

(define (test-humaneval) 

  (let (( candidate check_ways))
    (check-within (candidate (list (list 1 2) (list 2 3) (list 3 4) (list 4 5) (list 5 6) (list 6 7) (list 7 8) (list 8 9) (list 1 9) (list 1 10) (list 7 10) (list 9 10) (list 9 11) (list 11 12) (list 11 13) (list 13 14))) 2 0.001)
    (check-within (candidate (list (list -1000000000 1000000000) (list -1000000000 1000000000) (list -1000000000 1000000000) (list -1000000000 1000000000))) 0 0.001)
    (check-within (candidate (list (list 1 2) (list 1 3) (list 1 4) (list 1 5) (list 5 6))) 2 0.001)
    (check-within (candidate (list (list 1 2) (list 2 3) (list 1 3) (list 1 4) (list 1 5))) 2 0.001)
    (check-within (candidate (list (list 1 2) (list 2 3) (list 3 4) (list 5 6) (list 6 7))) 2 0.001)
    (check-within (candidate (list (list 1 2) (list 2 3) (list 4 5) (list 5 6))) 2 0.001)
    (check-within (candidate (list (list 1 2) (list 2 3) (list 3 4) (list 4 5) (list 5 6) (list 6 7) (list 7 8) (list 8 9) (list 10 11) (list 11 12) (list 12 13) (list 13 14) (list 14 15) (list 15 16) (list 16 17) (list 17 18))) 2 0.001)
    (check-within (candidate (list (list 1 2) (list 1 3) (list 1 4) (list 1 5) (list 2 6) (list 3 6) (list 4 6) (list 5 6))) 0 0.001)
    (check-within (candidate (list (list 1 2) (list 2 3) (list 1 3) (list 1 4) (list 1 5))) 2 0.001)
    (check-within (candidate (list (list 1 2) (list 2 3) (list 3 4) (list 4 5) (list 5 6) (list 6 7) (list 7 8) (list 8 9) (list 10 11) (list 11 12) (list 12 13) (list 13 14) (list 14 15) (list 15 16) (list 16 17) (list 17 18) (list 19 20))) 2 0.001)
    (check-within (candidate (list (list 1 2) (list 2 3) (list 1 3) (list 1 4) (list 1 5) (list 4 6))) 2 0.001)
    (check-within (candidate (list (list 1 2) (list 3 4) (list 5 6) (list 7 8) (list 9 10))) 2 0.001)
    (check-within (candidate (list (list 1 2) (list 3 4))) 2 0.001)
    (check-within (candidate (list (list 9 10) (list 1 2) (list 2 3) (list 4 5) (list 5 6) (list 6 7) (list 2 8) (list 3 4) (list 2 1) (list 9 10) (list 5 1) (list 8 7) (list 8 6))) 0 0.001)
))

(test-humaneval)