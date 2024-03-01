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
  ;; Create a graph represented as a hash table from the pairs
  (define graph (make-hash))
  (for-each (lambda (pair)
              (let ([start (first pair)])
                ;; If the start is not already a key in the graph, add it with an empty list
                (unless (hash-has-key? graph start)
                  (hash-set! graph start '()))
                ;; Append the end to the list of ends associated with the start
                (hash-update! graph start (curry cons (second pair)))))
            pairs)
  
  ;; Initialize the result list
  (define result '())
  ;; Start with the first pair's start value
  (define current (caar pairs))
  
  (let loop ()
    (when (hash-has-key? graph current)
      (let* ((ends (hash-ref graph current))
             ;; Remove the last element from the list of ends (simulating .pop())
             (end (car (reverse ends)))
             (new-ends (reverse (cdr (reverse ends)))))
        ;; Update the graph with the new list of ends (or remove the key if it's empty)
        (if (null? new-ends)
            (hash-remove! graph current)
            (hash-set! graph current new-ends))
        ;; Add the current pair to the result
        (set! result (cons (list current end) result))
        ;; Update the current value to be the last end value
        (set! current end)
        ;; Continue until there are no more keys in the graph
        (loop))))
  
  ;; Since we've built the result list in reverse order, return it reversed back
  (reverse result))

;; Examples from the problem statement
(valid_arrangement '((5 1) (4 5) (11 9) (9 4)))
(valid_arrangement '((1 3) (3 2) (2 1)))
(valid_arrangement '((1 2) (1 3) (2 1)))
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