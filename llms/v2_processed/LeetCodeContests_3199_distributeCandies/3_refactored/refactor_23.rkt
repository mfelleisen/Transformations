#lang racket

;; Function to calculate the number of ways to distribute candies among 3 children with a limit per child
;; You are given two positive integers n and limit.
;; Return the total number of ways to distribute n candies among 3 children such that no child gets more than limit candies.
;; Example 1:
;; Input: n = 5, limit = 2
;; Output: 3
;; Explanation: There are 3 ways to distribute 5 candies such that no child gets more than 2 candies: (1, 2, 2), (2, 1, 2) and (2, 2, 1).
;; Example 2:
;; Input: n = 3, limit = 3
;; Output: 10
;; Explanation: There are 10 ways to distribute 3 candies such that no child gets more than 3 candies: (0, 0, 3), (0, 1, 2), (0, 2, 1), (0, 3, 0), (1, 0, 2), (1, 1, 1), (1, 2, 0), (2, 0, 1), (2, 1, 0) and (3, 0, 0).
;; Constraints:
;;  * 1 <= n <= 50
;;  * 1 <= limit <= 50
(define (distribute-candies n limit)
  ;; A function to generate all possible distributions of candies
  (define (valid-distributions a b)
    (let ((c (- n a b)))
      (and (<= c limit) (>= c 0))))

  ;; Using for*/fold to count the valid distributions
  (for*/fold ([count 0])
             ([a (in-range (add1 (min n limit)))]
              [b (in-range (add1 (min (- n a) limit)))]
              #:when (valid-distributions a b))
    (add1 count)))

;; Example usage:
(distribute-candies 5 2)  ;; Output: 3
(distribute-candies 3 3)  ;; Output: 10

(require rackunit)


(define (test-humaneval) 

  (let (( candidate distributeCandies))
    (check-within (candidate 5 2) 3 0.001)
    (check-within (candidate 3 3) 10 0.001)
    (check-within (candidate 1 1) 3 0.001)
    (check-within (candidate 1 2) 3 0.001)
    (check-within (candidate 1 3) 3 0.001)
    (check-within (candidate 1 4) 3 0.001)
    (check-within (candidate 1 5) 3 0.001)
    (check-within (candidate 1 6) 3 0.001)
    (check-within (candidate 1 7) 3 0.001)
    (check-within (candidate 1 8) 3 0.001)
    (check-within (candidate 1 9) 3 0.001)
    (check-within (candidate 1 10) 3 0.001)
    (check-within (candidate 1 11) 3 0.001)
    (check-within (candidate 1 12) 3 0.001)
    (check-within (candidate 1 13) 3 0.001)
    (check-within (candidate 1 14) 3 0.001)
    (check-within (candidate 1 15) 3 0.001)
    (check-within (candidate 1 16) 3 0.001)
    (check-within (candidate 1 17) 3 0.001)
    (check-within (candidate 1 18) 3 0.001)
    (check-within (candidate 1 19) 3 0.001)
    (check-within (candidate 1 20) 3 0.001)
    (check-within (candidate 1 21) 3 0.001)
    (check-within (candidate 1 22) 3 0.001)
    (check-within (candidate 1 23) 3 0.001)
    (check-within (candidate 1 24) 3 0.001)
    (check-within (candidate 1 25) 3 0.001)
    (check-within (candidate 2 1) 3 0.001)
    (check-within (candidate 2 2) 6 0.001)
    (check-within (candidate 2 3) 6 0.001)
    (check-within (candidate 2 4) 6 0.001)
    (check-within (candidate 2 5) 6 0.001)
    (check-within (candidate 2 6) 6 0.001)
    (check-within (candidate 2 7) 6 0.001)
    (check-within (candidate 2 8) 6 0.001)
    (check-within (candidate 2 9) 6 0.001)
    (check-within (candidate 2 10) 6 0.001)
    (check-within (candidate 2 11) 6 0.001)
    (check-within (candidate 2 12) 6 0.001)
    (check-within (candidate 2 13) 6 0.001)
    (check-within (candidate 2 14) 6 0.001)
    (check-within (candidate 2 15) 6 0.001)
    (check-within (candidate 2 16) 6 0.001)
    (check-within (candidate 2 17) 6 0.001)
    (check-within (candidate 2 18) 6 0.001)
    (check-within (candidate 2 19) 6 0.001)
    (check-within (candidate 2 20) 6 0.001)
    (check-within (candidate 2 21) 6 0.001)
    (check-within (candidate 2 22) 6 0.001)
    (check-within (candidate 2 23) 6 0.001)
    (check-within (candidate 2 24) 6 0.001)
    (check-within (candidate 2 25) 6 0.001)
    (check-within (candidate 3 1) 1 0.001)
    (check-within (candidate 3 2) 7 0.001)
    (check-within (candidate 3 4) 10 0.001)
    (check-within (candidate 3 5) 10 0.001)
    (check-within (candidate 3 6) 10 0.001)
    (check-within (candidate 3 7) 10 0.001)
    (check-within (candidate 3 8) 10 0.001)
    (check-within (candidate 3 9) 10 0.001)
    (check-within (candidate 3 10) 10 0.001)
    (check-within (candidate 3 11) 10 0.001)
    (check-within (candidate 3 12) 10 0.001)
    (check-within (candidate 3 13) 10 0.001)
    (check-within (candidate 3 14) 10 0.001)
    (check-within (candidate 3 15) 10 0.001)
    (check-within (candidate 3 16) 10 0.001)
    (check-within (candidate 3 17) 10 0.001)
    (check-within (candidate 3 18) 10 0.001)
    (check-within (candidate 3 19) 10 0.001)
    (check-within (candidate 3 20) 10 0.001)
    (check-within (candidate 3 21) 10 0.001)
    (check-within (candidate 3 22) 10 0.001)
    (check-within (candidate 3 23) 10 0.001)
    (check-within (candidate 3 24) 10 0.001)
    (check-within (candidate 3 25) 10 0.001)
    (check-within (candidate 4 1) 0 0.001)
    (check-within (candidate 4 2) 6 0.001)
    (check-within (candidate 4 3) 12 0.001)
    (check-within (candidate 4 4) 15 0.001)
    (check-within (candidate 4 5) 15 0.001)
    (check-within (candidate 4 6) 15 0.001)
    (check-within (candidate 4 7) 15 0.001)
    (check-within (candidate 4 8) 15 0.001)
    (check-within (candidate 4 9) 15 0.001)
    (check-within (candidate 4 10) 15 0.001)
    (check-within (candidate 4 11) 15 0.001)
    (check-within (candidate 4 12) 15 0.001)
    (check-within (candidate 4 13) 15 0.001)
    (check-within (candidate 4 14) 15 0.001)
    (check-within (candidate 4 15) 15 0.001)
    (check-within (candidate 4 16) 15 0.001)
    (check-within (candidate 4 17) 15 0.001)
    (check-within (candidate 4 18) 15 0.001)
    (check-within (candidate 4 19) 15 0.001)
    (check-within (candidate 4 20) 15 0.001)
    (check-within (candidate 4 21) 15 0.001)
    (check-within (candidate 4 22) 15 0.001)
    (check-within (candidate 4 23) 15 0.001)
    (check-within (candidate 4 24) 15 0.001)
))

(test-humaneval)