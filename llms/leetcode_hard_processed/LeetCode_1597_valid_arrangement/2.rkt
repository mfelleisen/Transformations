#lang racket

;; You are given a 0-indexed 2D integer list pairs where pairs[i] = [starti, endi]. An arrangement of pairs is valid if for every index i where 1 <= i < pairs.length, we have endi-1 == starti.
;; Return any valid arrangement of pairs.
;; Note: The inputs will be generated such that there exists a valid arrangement of pairs.
;; Example 1:
;; Input: pairs = [[5,1],[4,5],[11,9],[9,4]]
;; Output: [[11,9],[9,4],[4,5],[5,1]]
;; Explanation:
;; This is a valid arrangement since endi-1 always equals starti.
;; end0 = 9 == 9 = start1 
;; end1 = 4 == 4 = start2
;; end2 = 5 == 5 = start3
;; Example 2:
;; Input: pairs = [[1,3],[3,2],[2,1]]
;; Output: [[1,3],[3,2],[2,1]]
;; Explanation:
;; This is a valid arrangement since endi-1 always equals starti.
;; end0 = 3 == 3 = start1
;; end1 = 2 == 2 = start2
;; The arrangements [[2,1],[1,3],[3,2]] and [[3,2],[2,1],[1,3]] are also valid.
;; Example 3:
;; Input: pairs = [[1,2],[1,3],[2,1]]
;; Output: [[1,2],[2,1],[1,3]]
;; Explanation:
;; This is a valid arrangement since endi-1 always equals starti.
;; end0 = 2 == 2 = start1
;; end1 = 1 == 1 = start2
;; Constraints:
;; 1 <= pairs.length <= 105
;; pairs[i].length == 2
;; 0 <= starti, endi <= 109
;; starti != endi
;; No two pairs are exactly the same.
;; There exists a valid arrangement of pairs.
(define (valid_arrangement pairs)
  ;; Create a mutable hash table to represent the graph
  (define graph (make-hash))
  
  ;; Populate the graph with pairs, each `start` points to a list of `ends`
  (for-each (lambda (pair)
              (define start (car pair))
              (define end (cadr pair))
              ;; If `start` is not already a key in graph, initialize an empty list
              (hash-update! graph start (lambda (ends) (cons end ends)) '()))
            pairs)
  
  ;; Initialize the result list
  (define result '())
  
  ;; Start with the first element of the first pair
  (define current (caar pairs))
  
  ;; Keep looping until the graph is empty
  (let loop ()
    (when (not (hash-empty? graph))
      ;; Retrieve the list of ends for the current start and remove one end
      (define-values (end ends) (car+cdr (hash-ref graph current)))
      ;; Update the graph: if there are more ends, update the list; otherwise, remove the key
      (if (null? ends)
          (hash-remove! graph current)
          (hash-set! graph current ends))
      ;; Add the current pair to the result and set the new current to the selected end
      (set! result (cons (list current end) result))
      (set! current end)
      (loop)))
  
  ;; Return the reversed result to maintain the original order
  (reverse result))

;; Helper function to get the first element and the rest of a list
(define (car+cdr lst)
  (values (car lst) (cdr lst)))
(require rackunit)

(define (test-humaneval) 

  (let (( candidate valid_arrangement))
    (check-within (candidate (list (list 0 1) (list 1 2) (list 2 3))) (list (list 0 1) (list 1 2) (list 2 3)) 0.001)
    (check-within (candidate (list (list 9 8) (list 10 9) (list 8 10))) (list (list 9 8) (list 8 10) (list 10 9)) 0.001)
    (check-within (candidate (list (list 1 3) (list 3 2) (list 2 1))) (list (list 1 3) (list 3 2) (list 2 1)) 0.001)
    (check-within (candidate (list (list 16 17) (list 18 16) (list 17 18))) (list (list 16 17) (list 17 18) (list 18 16)) 0.001)
    (check-within (candidate (list (list 3 5) (list 7 3) (list 5 7))) (list (list 3 5) (list 5 7) (list 7 3)) 0.001)
    (check-within (candidate (list (list 0 1) (list 1 2) (list 2 0))) (list (list 0 1) (list 1 2) (list 2 0)) 0.001)
    (check-within (candidate (list (list 0 1) (list 2 0) (list 1 2))) (list (list 0 1) (list 1 2) (list 2 0)) 0.001)
    (check-within (candidate (list (list 11 10) (list 12 11) (list 10 12))) (list (list 11 10) (list 10 12) (list 12 11)) 0.001)
    (check-within (candidate (list (list 13 14) (list 15 13) (list 14 15))) (list (list 13 14) (list 14 15) (list 15 13)) 0.001)
    (check-within (candidate (list (list 12 0) (list 0 10) (list 10 7) (list 7 12))) (list (list 12 0) (list 0 10) (list 10 7) (list 7 12)) 0.001)
    (check-within (candidate (list (list 5 2) (list 3 5) (list 2 3))) (list (list 5 2) (list 2 3) (list 3 5)) 0.001)
    (check-within (candidate (list (list 1 5) (list 3 1) (list 5 3))) (list (list 1 5) (list 5 3) (list 3 1)) 0.001)
))

(test-humaneval)