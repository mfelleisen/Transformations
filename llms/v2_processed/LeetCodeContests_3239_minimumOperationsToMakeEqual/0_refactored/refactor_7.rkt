#lang racket

;; You are given two positive integers x and y.
;; In one operation, you can do one of the four following operations:
;; Divide x by 11 if x is a multiple of 11.
;; Divide x by 5 if x is a multiple of 5.
;; Decrement x by 1.
;; Increment x by 1.
;; Return the minimum number of operations required to make  x and y equal.
;; Example 1:
;; Input: x = 26, y = 1
;; Output: 3
;; Explanation: We can make 26 equal to 1 by applying the following operations: 
;; 1. Decrement x by 1
;; 2. Divide x by 5
;; 3. Divide x by 5
;; It can be shown that 3 is the minimum number of operations required to make 26 equal to 1.
;; Example 2:
;; Input: x = 54, y = 2
;; Output: 4
;; Explanation: We can make 54 equal to 2 by applying the following operations: 
;; 1. Increment x by 1
;; 2. Divide x by 11 
;; 3. Divide x by 5
;; 4. Increment x by 1
;; It can be shown that 4 is the minimum number of operations required to make 54 equal to 2.
;; Example 3:
;; Input: x = 25, y = 30
;; Output: 5
;; Explanation: We can make 25 equal to 30 by applying the following operations: 
;; 1. Increment x by 1
;; 2. Increment x by 1
;; 3. Increment x by 1
;; 4. Increment x by 1
;; 5. Increment x by 1
;; It can be shown that 5 is the minimum number of operations required to make 25 equal to 30.
;; Constraints:
;; 1 <= x, y <= 104
(define (minimumOperationsToMakeEqual x y)
  ;; Define a helper function that returns the possible next states given a current value
  (define (next-states current)
    (define possible-states
      (list
       (if (zero? (remainder current 11)) (/ current 11) current)
       (if (zero? (remainder current 5)) (/ current 5) current)
       (- current 1)
       (+ current 1)))
    (filter (lambda (v) (> v 0)) possible-states))

  ;; Breadth-first search to find the minimum number of operations
  (define (bfs queue visited)
    (match queue
      ['() -1]  ;; This should never happen as per problem statement
      [(list (list current steps) rest ...)
       (cond
         [(= current y) steps]
         [else
          (define possibilities (filter (lambda (v) (not (set-member? visited v))) (next-states current)))
          (bfs (append rest (map (λ (v) (list v (add1 steps))) possibilities))
               (foldl set-add visited possibilities))])]))

  ;; Initialize the BFS with the starting point
  (if (= x y)
      0
      (bfs (list (list x 0)) (set x))))

;; Example usage
(minimumOperationsToMakeEqual 26 1)  ;; Output: 3
(minimumOperationsToMakeEqual 54 2)  ;; Output: 4
(minimumOperationsToMakeEqual 25 30) ;; Output: 5

(require rackunit)


(define (test-humaneval) 

  (let (( candidate minimumOperationsToMakeEqual))
    (check-within (candidate 26 1) 3 0.001)
    (check-within (candidate 54 2) 4 0.001)
    (check-within (candidate 25 30) 5 0.001)
    (check-within (candidate 1 1) 0 0.001)
    (check-within (candidate 1 2) 1 0.001)
    (check-within (candidate 1 3) 2 0.001)
    (check-within (candidate 1 4) 3 0.001)
    (check-within (candidate 1 5) 4 0.001)
    (check-within (candidate 1 6) 5 0.001)
    (check-within (candidate 1 7) 6 0.001)
    (check-within (candidate 1 8) 7 0.001)
    (check-within (candidate 1 9) 8 0.001)
    (check-within (candidate 1 10) 9 0.001)
    (check-within (candidate 1 11) 10 0.001)
    (check-within (candidate 1 12) 11 0.001)
    (check-within (candidate 1 13) 12 0.001)
    (check-within (candidate 1 14) 13 0.001)
    (check-within (candidate 1 15) 14 0.001)
    (check-within (candidate 1 16) 15 0.001)
    (check-within (candidate 1 17) 16 0.001)
    (check-within (candidate 1 18) 17 0.001)
    (check-within (candidate 1 19) 18 0.001)
    (check-within (candidate 1 20) 19 0.001)
    (check-within (candidate 1 21) 20 0.001)
    (check-within (candidate 1 22) 21 0.001)
    (check-within (candidate 1 23) 22 0.001)
    (check-within (candidate 1 24) 23 0.001)
    (check-within (candidate 1 25) 24 0.001)
    (check-within (candidate 2 1) 1 0.001)
    (check-within (candidate 2 2) 0 0.001)
    (check-within (candidate 2 3) 1 0.001)
    (check-within (candidate 2 4) 2 0.001)
    (check-within (candidate 2 5) 3 0.001)
    (check-within (candidate 2 6) 4 0.001)
    (check-within (candidate 2 7) 5 0.001)
    (check-within (candidate 2 8) 6 0.001)
    (check-within (candidate 2 9) 7 0.001)
    (check-within (candidate 2 10) 8 0.001)
    (check-within (candidate 2 11) 9 0.001)
    (check-within (candidate 2 12) 10 0.001)
    (check-within (candidate 2 13) 11 0.001)
    (check-within (candidate 2 14) 12 0.001)
    (check-within (candidate 2 15) 13 0.001)
    (check-within (candidate 2 16) 14 0.001)
    (check-within (candidate 2 17) 15 0.001)
    (check-within (candidate 2 18) 16 0.001)
    (check-within (candidate 2 19) 17 0.001)
    (check-within (candidate 2 20) 18 0.001)
    (check-within (candidate 2 21) 19 0.001)
    (check-within (candidate 2 22) 20 0.001)
    (check-within (candidate 2 23) 21 0.001)
    (check-within (candidate 2 24) 22 0.001)
    (check-within (candidate 2 25) 23 0.001)
    (check-within (candidate 3 1) 2 0.001)
    (check-within (candidate 3 2) 1 0.001)
    (check-within (candidate 3 3) 0 0.001)
    (check-within (candidate 3 4) 1 0.001)
    (check-within (candidate 3 5) 2 0.001)
    (check-within (candidate 3 6) 3 0.001)
    (check-within (candidate 3 7) 4 0.001)
    (check-within (candidate 3 8) 5 0.001)
    (check-within (candidate 3 9) 6 0.001)
    (check-within (candidate 3 10) 7 0.001)
    (check-within (candidate 3 11) 8 0.001)
    (check-within (candidate 3 12) 9 0.001)
    (check-within (candidate 3 13) 10 0.001)
    (check-within (candidate 3 14) 11 0.001)
    (check-within (candidate 3 15) 12 0.001)
    (check-within (candidate 3 16) 13 0.001)
    (check-within (candidate 3 17) 14 0.001)
    (check-within (candidate 3 18) 15 0.001)
    (check-within (candidate 3 19) 16 0.001)
    (check-within (candidate 3 20) 17 0.001)
    (check-within (candidate 3 21) 18 0.001)
    (check-within (candidate 3 22) 19 0.001)
    (check-within (candidate 3 23) 20 0.001)
    (check-within (candidate 3 24) 21 0.001)
    (check-within (candidate 3 25) 22 0.001)
    (check-within (candidate 4 1) 2 0.001)
    (check-within (candidate 4 2) 2 0.001)
    (check-within (candidate 4 3) 1 0.001)
    (check-within (candidate 4 4) 0 0.001)
    (check-within (candidate 4 5) 1 0.001)
    (check-within (candidate 4 6) 2 0.001)
    (check-within (candidate 4 7) 3 0.001)
    (check-within (candidate 4 8) 4 0.001)
    (check-within (candidate 4 9) 5 0.001)
    (check-within (candidate 4 10) 6 0.001)
    (check-within (candidate 4 11) 7 0.001)
    (check-within (candidate 4 12) 8 0.001)
    (check-within (candidate 4 13) 9 0.001)
    (check-within (candidate 4 14) 10 0.001)
    (check-within (candidate 4 15) 11 0.001)
    (check-within (candidate 4 16) 12 0.001)
    (check-within (candidate 4 17) 13 0.001)
    (check-within (candidate 4 18) 14 0.001)
    (check-within (candidate 4 19) 15 0.001)
    (check-within (candidate 4 20) 16 0.001)
    (check-within (candidate 4 21) 17 0.001)
    (check-within (candidate 4 22) 18 0.001)
))

(test-humaneval)