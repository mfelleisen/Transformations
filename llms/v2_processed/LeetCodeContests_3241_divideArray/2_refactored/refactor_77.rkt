#lang racket

;; You are given an integer array nums of size n and a positive integer k.
;; Divide the array into one or more arrays of size 3 satisfying the following conditions:
;; Each element of nums should be in exactly one array.
;; The difference between any two elements in one array is less than or equal to k.
;; Return a 2D array containing all the arrays. If it is impossible to satisfy the conditions, return an empty array. And if there are multiple answers, return any of them.
;; Example 1:
;; Input: nums = [1,3,4,8,7,9,3,5,1], k = 2
;; Output: [[1,1,3],[3,4,5],[7,8,9]]
;; Explanation: We can divide the array into the following arrays: [1,1,3], [3,4,5] and [7,8,9].
;; The difference between any two elements in each array is less than or equal to 2.
;; Note that the order of elements is not important.
;; Example 2:
;; Input: nums = [1,3,3,2,7,3], k = 3
;; Output: []
;; Explanation: It is not possible to divide the array satisfying all the conditions.
;; Constraints:
;; n == nums.length
;; 1 <= n <= 105
;; n is a multiple of 3.
;; 1 <= nums[i] <= 105
;; 1 <= k <= 105
(define (divideArray nums k)
  ;; Check if the length of nums is a multiple of 3
  (if (not (= (remainder (length nums) 3) 0))
      '()  ; Return an empty list if nums length is not a multiple of 3
      ;; Sort the list in ascending order
      (let loop ([sorted-nums (sort nums <)]
                 [sublists '()])
        (match sorted-nums
          ;; If the remaining list is empty, return the collected sublists
          ['() (reverse sublists)]
          ;; Extract the first three elements
          [(list-rest triplet more (== 3))
           (if (<= (- (third triplet) (first triplet)) k)
               ;; If the condition is met, add the triplet to sublists and continue
               (loop more (cons triplet sublists))
               ;; Otherwise, return an empty list
               '())]))))


(require rackunit)


(define (test-humaneval) 

  (let (( candidate divideArray))
    (check-within (candidate (list 1 3 4 8 7 9 3 5 1) 2) (list (list 1 1 3) (list 3 4 5) (list 7 8 9)) 0.001)
    (check-within (candidate (list 1 3 3 2 7 3) 3) (list ) 0.001)
    (check-within (candidate (list 4 2 9 8 2 12 7 12 10 5 8 5 5 7 9 2 5 11) 14) (list (list 2 2 2) (list 4 5 5) (list 5 5 7) (list 7 8 8) (list 9 9 10) (list 11 12 12)) 0.001)
    (check-within (candidate (list 33 26 4 18 16 24 24 15 8 18 34 20 24 16 3) 16) (list (list 3 4 8) (list 15 16 16) (list 18 18 20) (list 24 24 24) (list 26 33 34)) 0.001)
    (check-within (candidate (list 6 1 8 8 5 8 5 9 8 9 5 8 3 4 6) 7) (list (list 1 3 4) (list 5 5 5) (list 6 6 8) (list 8 8 8) (list 8 9 9)) 0.001)
    (check-within (candidate (list 20 21 34 3 19 2 23 32 20 17 14 13 19 20 6) 15) (list (list 2 3 6) (list 13 14 17) (list 19 19 20) (list 20 20 21) (list 23 32 34)) 0.001)
    (check-within (candidate (list 6 10 5 12 7 11 6 6 12 12 11 7) 2) (list (list 5 6 6) (list 6 7 7) (list 10 11 11) (list 12 12 12)) 0.001)
    (check-within (candidate (list 12 15 26 7 10 13 15 5 27 16 14 15) 18) (list (list 5 7 10) (list 12 13 14) (list 15 15 15) (list 16 26 27)) 0.001)
    (check-within (candidate (list 12 7 13 10 7 19 11 23 3 3 7 9) 16) (list (list 3 3 7) (list 7 7 9) (list 10 11 12) (list 13 19 23)) 0.001)
    (check-within (candidate (list 19 3 23 4 8 1 1 3 26) 7) (list (list 1 1 3) (list 3 4 8) (list 19 23 26)) 0.001)
    (check-within (candidate (list 11 13 24 11 9 23 16 19 13) 8) (list (list 9 11 11) (list 13 13 16) (list 19 23 24)) 0.001)
    (check-within (candidate (list 6 12 21 12 6 12 25 20 15 22 11 19 8 4 18 26 17 18 12 5 8) 11) (list (list 4 5 6) (list 6 8 8) (list 11 12 12) (list 12 12 15) (list 17 18 18) (list 19 20 21) (list 22 25 26)) 0.001)
    (check-within (candidate (list 15 17 14 3 25 15 11 25 15 16 12 18) 10) (list (list 3 11 12) (list 14 15 15) (list 15 16 17) (list 18 25 25)) 0.001)
    (check-within (candidate (list 16 20 16 19 20 13 14 20 14) 10) (list (list 13 14 14) (list 16 16 19) (list 20 20 20)) 0.001)
    (check-within (candidate (list 2 13 15 14 18 15 3 13 2) 1) (list ) 0.001)
    (check-within (candidate (list 1 14 20 7 17 2 14 1 8) 11) (list (list 1 1 2) (list 7 8 14) (list 14 17 20)) 0.001)
    (check-within (candidate (list 8 12 19 8 9 19 9 19 9 8 6 9 6 6 12) 3) (list (list 6 6 6) (list 8 8 8) (list 9 9 9) (list 9 12 12) (list 19 19 19)) 0.001)
    (check-within (candidate (list 18 16 17 19 12 25 11 27 11 32 32 17) 20) (list (list 11 11 12) (list 16 17 17) (list 18 19 25) (list 27 32 32)) 0.001)
    (check-within (candidate (list 21 11 24 20 17 13 7 20 20 16 24 20 12 17 16 15 7 7 18 15 20) 6) (list (list 7 7 7) (list 11 12 13) (list 15 15 16) (list 16 17 17) (list 18 20 20) (list 20 20 20) (list 21 24 24)) 0.001)
    (check-within (candidate (list 6 7 7 6 7 6) 13) (list (list 6 6 6) (list 7 7 7)) 0.001)
    (check-within (candidate (list 11 12 12 5 6 5) 9) (list (list 5 5 6) (list 11 12 12)) 0.001)
    (check-within (candidate (list 5 5 12 5 5 22 2 2 5 2 5 5 16 2 22 2 12 16 15 13 19) 3) (list (list 2 2 2) (list 2 2 5) (list 5 5 5) (list 5 5 5) (list 12 12 13) (list 15 16 16) (list 19 22 22)) 0.001)
    (check-within (candidate (list 11 28 12 5 19 15 16 9 21 13 12 9 19 19 18) 9) (list (list 5 9 9) (list 11 12 12) (list 13 15 16) (list 18 19 19) (list 19 21 28)) 0.001)
    (check-within (candidate (list 10 14 17) 15) (list (list 10 14 17)) 0.001)
    (check-within (candidate (list 16 15 9 20 17 19 11 18 16) 9) (list (list 9 11 15) (list 16 16 17) (list 18 19 20)) 0.001)
    (check-within (candidate (list 16 28 16 7 18 13 5 27 27 16 20 22 13 6 17) 11) (list (list 5 6 7) (list 13 13 16) (list 16 16 17) (list 18 20 22) (list 27 27 28)) 0.001)
    (check-within (candidate (list 14 7 13 2 3 7 17 13 13 2 14 7) 3) (list (list 2 2 3) (list 7 7 7) (list 13 13 13) (list 14 14 17)) 0.001)
    (check-within (candidate (list 20 8 6 5 10 5 10 2 20 6 12 13 13 20 4) 6) (list (list 2 4 5) (list 5 6 6) (list 8 10 10) (list 12 13 13) (list 20 20 20)) 0.001)
    (check-within (candidate (list 12 14 16 9 20 18 16 4 24 14 16 30 1 17 30 16 30 6) 13) (list (list 1 4 6) (list 9 12 14) (list 14 16 16) (list 16 16 17) (list 18 20 24) (list 30 30 30)) 0.001)
    (check-within (candidate (list 13 6 19 21 16 11 1 14 7) 20) (list (list 1 6 7) (list 11 13 14) (list 16 19 21)) 0.001)
    (check-within (candidate (list 13 2 12 22 18 15 3 20 2 18 3 14 2 10 14 9 14 3 14 17 5) 9) (list (list 2 2 2) (list 3 3 3) (list 5 9 10) (list 12 13 14) (list 14 14 14) (list 15 17 18) (list 18 20 22)) 0.001)
    (check-within (candidate (list 12 13 12 14 14 6 5 7 23 21 21 16 15 20 22 14 20 7) 10) (list (list 5 6 7) (list 7 12 12) (list 13 14 14) (list 14 15 16) (list 20 20 21) (list 21 22 23)) 0.001)
    (check-within (candidate (list 15 14 3 19 17 18 19 23 2 16 19 3) 5) (list (list 2 3 3) (list 14 15 16) (list 17 18 19) (list 19 19 23)) 0.001)
    (check-within (candidate (list 12 8 18 6 12 6 8 33 20 6 17 17 27 8 12) 16) (list (list 6 6 6) (list 8 8 8) (list 12 12 12) (list 17 17 18) (list 20 27 33)) 0.001)
    (check-within (candidate (list 1 1 23 17 18 1) 12) (list (list 1 1 1) (list 17 18 23)) 0.001)
    (check-within (candidate (list 13 13 3 7 6 13 6 4 3) 1) (list (list 3 3 4) (list 6 6 7) (list 13 13 13)) 0.001)
    (check-within (candidate (list 19 10 9 20 29 28 29 9 18 27 23 4 16 8 11 19 10 12 10 10 21) 20) (list (list 4 8 9) (list 9 10 10) (list 10 10 11) (list 12 16 18) (list 19 19 20) (list 21 23 27) (list 28 29 29)) 0.001)
    (check-within (candidate (list 13 12 12 11 22 10) 15) (list (list 10 11 12) (list 12 13 22)) 0.001)
    (check-within (candidate (list 15 16 12 34 16 16 24 21 3 24 29 10) 20) (list (list 3 10 12) (list 15 16 16) (list 16 21 24) (list 24 29 34)) 0.001)
    (check-within (candidate (list 17 16 17 11 13 6) 19) (list (list 6 11 13) (list 16 17 17)) 0.001)
    (check-within (candidate (list 11 16 16 6 8 20 21 3 20 11 16 6 6 11 6) 3) (list (list 3 6 6) (list 6 6 8) (list 11 11 11) (list 16 16 16) (list 20 20 21)) 0.001)
    (check-within (candidate (list 2 16 8 7 15 16) 9) (list (list 2 7 8) (list 15 16 16)) 0.001)
    (check-within (candidate (list 15 17 22) 14) (list (list 15 17 22)) 0.001)
    (check-within (candidate (list 8 4 9 18 18 5 10 11 19 18 19 23 4 15 25 20 20 6) 7) (list (list 4 4 5) (list 6 8 9) (list 10 11 15) (list 18 18 18) (list 19 19 20) (list 20 23 25)) 0.001)
    (check-within (candidate (list 12 20 16 12 15 16 15 20 14 16 19 13) 1) (list (list 12 12 13) (list 14 15 15) (list 16 16 16) (list 19 20 20)) 0.001)
    (check-within (candidate (list 20 19 8 21 13 18 21 12 12 18 9 9) 1) (list (list 8 9 9) (list 12 12 13) (list 18 18 19) (list 20 21 21)) 0.001)
    (check-within (candidate (list 6 14 19 17 13 4 17 10 17) 19) (list (list 4 6 10) (list 13 14 17) (list 17 17 19)) 0.001)
    (check-within (candidate (list 8 8 12) 4) (list (list 8 8 12)) 0.001)
    (check-within (candidate (list 3 16 17 18 10 8 20 16 20 10 10 21) 16) (list (list 3 8 10) (list 10 10 16) (list 16 17 18) (list 20 20 21)) 0.001)
    (check-within (candidate (list 19 14 17 20 16 16 7 10 18 8 16 15 15 13 12 14 17 11) 8) (list (list 7 8 10) (list 11 12 13) (list 14 14 15) (list 15 16 16) (list 16 17 17) (list 18 19 20)) 0.001)
    (check-within (candidate (list 18 7 11 13 13 9 22 20 21 13 7 18 8 8 16) 4) (list (list 7 7 8) (list 8 9 11) (list 13 13 13) (list 16 18 18) (list 20 21 22)) 0.001)
    (check-within (candidate (list 10 15 9 15 15 10) 1) (list (list 9 10 10) (list 15 15 15)) 0.001)
    (check-within (candidate (list 16 17 16) 16) (list (list 16 16 17)) 0.001)
    (check-within (candidate (list 15 1 15 14 18 17 1 18 12 16 6 6 7 1 12) 4) (list (list 1 1 1) (list 6 6 7) (list 12 12 14) (list 15 15 16) (list 17 18 18)) 0.001)
    (check-within (candidate (list 6 11 6 18 11 13 13 8 11 4 4 11 12 17 11) 12) (list (list 4 4 6) (list 6 8 11) (list 11 11 11) (list 11 12 13) (list 13 17 18)) 0.001)
    (check-within (candidate (list 5 13 4 14 11 18 9 10 20 5 17 11 5 8 20 5 14 4 18 17 17) 8) (list (list 4 4 5) (list 5 5 5) (list 8 9 10) (list 11 11 13) (list 14 14 17) (list 17 17 18) (list 18 20 20)) 0.001)
    (check-within (candidate (list 13 6 20 13 12 8 7 12 22 16 13 7 12 17 5) 6) (list (list 5 6 7) (list 7 8 12) (list 12 12 13) (list 13 13 16) (list 17 20 22)) 0.001)
    (check-within (candidate (list 23 2 15 20 18 14 20 7 2 22 4 14 7 9 15 14 2 7) 8) (list (list 2 2 2) (list 4 7 7) (list 7 9 14) (list 14 14 15) (list 15 18 20) (list 20 22 23)) 0.001)
    (check-within (candidate (list 19 9 2 4 17 2 27 18 17) 18) (list (list 2 2 4) (list 9 17 17) (list 18 19 27)) 0.001)
    (check-within (candidate (list 5 20 29 4 12 14 31 6 11 2 15 17 15 19 4) 20) (list (list 2 4 4) (list 5 6 11) (list 12 14 15) (list 15 17 19) (list 20 29 31)) 0.001)
    (check-within (candidate (list 15 20 5 24 18 16 25 21 28 12 19 28 25 20 14 18 24 28) 17) (list (list 5 12 14) (list 15 16 18) (list 18 19 20) (list 20 21 24) (list 24 25 25) (list 28 28 28)) 0.001)
    (check-within (candidate (list 9 6 23 17 7 17) 20) (list (list 6 7 9) (list 17 17 23)) 0.001)
    (check-within (candidate (list 24 23 19) 6) (list (list 19 23 24)) 0.001)
    (check-within (candidate (list 6 19 22 7 17 7 15 17 7 18 4 14 9 10 16) 9) (list (list 4 6 7) (list 7 7 9) (list 10 14 15) (list 16 17 17) (list 18 19 22)) 0.001)
    (check-within (candidate (list 4 3 15 1 15 15) 4) (list (list 1 3 4) (list 15 15 15)) 0.001)
    (check-within (candidate (list 10 22 18 15 7 21 6 7 11 9 7 6 7 10 18) 8) (list (list 6 6 7) (list 7 7 7) (list 9 10 10) (list 11 15 18) (list 18 21 22)) 0.001)
    (check-within (candidate (list 16 17 2 17 9 7 22 17 12 4 14 17 4 19 12 18 19 8 17 5 6) 7) (list (list 2 4 4) (list 5 6 7) (list 8 9 12) (list 12 14 16) (list 17 17 17) (list 17 17 18) (list 19 19 22)) 0.001)
    (check-within (candidate (list 20 18 18 22 7 9 9 10 16 4 18 18 11 9 18 11 11 21) 7) (list (list 4 7 9) (list 9 9 10) (list 11 11 11) (list 16 18 18) (list 18 18 18) (list 20 21 22)) 0.001)
    (check-within (candidate (list 5 11 15 9 17 6 16 14 4 9 5 13 10 12 13 15 13 12 16 12 13) 5) (list (list 4 5 5) (list 6 9 9) (list 10 11 12) (list 12 12 13) (list 13 13 13) (list 14 15 15) (list 16 16 17)) 0.001)
    (check-within (candidate (list 4 16 17) 20) (list (list 4 16 17)) 0.001)
    (check-within (candidate (list 10 9 22 13 17 11 6 9 11) 10) (list (list 6 9 9) (list 10 11 11) (list 13 17 22)) 0.001)
    (check-within (candidate (list 3 11 19 8 22 23 15 18 37 7 25 20 12 19 7) 18) (list (list 3 7 7) (list 8 11 12) (list 15 18 19) (list 19 20 22) (list 23 25 37)) 0.001)
    (check-within (candidate (list 4 6 6 3 11 11) 16) (list (list 3 4 6) (list 6 11 11)) 0.001)
    (check-within (candidate (list 10 17 10 15 16 8) 7) (list (list 8 10 10) (list 15 16 17)) 0.001)
    (check-within (candidate (list 4 20 4 19 8 7 4 20 7) 3) (list (list 4 4 4) (list 7 7 8) (list 19 20 20)) 0.001)
    (check-within (candidate (list 4 4 4) 17) (list (list 4 4 4)) 0.001)
    (check-within (candidate (list 18 6 15 20 5 27 23 15 26 11 11 4 17 23 11) 15) (list (list 4 5 6) (list 11 11 11) (list 15 15 17) (list 18 20 23) (list 23 26 27)) 0.001)
    (check-within (candidate (list 8 9 5) 15) (list (list 5 8 9)) 0.001)
    (check-within (candidate (list 20 15 8 11 11 10 19 7 20) 7) (list (list 7 8 10) (list 11 11 15) (list 19 20 20)) 0.001)
    (check-within (candidate (list 12 11 18 13 13 21) 11) (list (list 11 12 13) (list 13 18 21)) 0.001)
    (check-within (candidate (list 19 29 11 18 19 17 29 19 7) 14) (list (list 7 11 17) (list 18 19 19) (list 19 29 29)) 0.001)
    (check-within (candidate (list 14 1 25 1 14 19 2 2 4 16 17 11 26 29 12) 17) (list (list 1 1 2) (list 2 4 11) (list 12 14 14) (list 16 17 19) (list 25 26 29)) 0.001)
    (check-within (candidate (list 14 25 16 11 7 13 12 16 24 19 5 17) 13) (list (list 5 7 11) (list 12 13 14) (list 16 16 17) (list 19 24 25)) 0.001)
    (check-within (candidate (list 11 26 19 10 16 10 11 18 9) 11) (list (list 9 10 10) (list 11 11 16) (list 18 19 26)) 0.001)
    (check-within (candidate (list 16 8 15) 16) (list (list 8 15 16)) 0.001)
    (check-within (candidate (list 12 8 18 8 18 13 12 18 18 13 12 23 21 8 13) 5) (list (list 8 8 8) (list 12 12 12) (list 13 13 13) (list 18 18 18) (list 18 21 23)) 0.001)
    (check-within (candidate (list 12 16 9 8 22 16) 16) (list (list 8 9 12) (list 16 16 22)) 0.001)
    (check-within (candidate (list 15 16 18 8 12 7 5 17 23 17 18 13 5 4 13 18 7 20) 6) (list (list 4 5 5) (list 7 7 8) (list 12 13 13) (list 15 16 17) (list 17 18 18) (list 18 20 23)) 0.001)
    (check-within (candidate (list 12 11 14 13 9 16 31 19 21 22 7 1 22 23 9 2 21 21) 15) (list (list 1 2 7) (list 9 9 11) (list 12 13 14) (list 16 19 21) (list 21 21 22) (list 22 23 31)) 0.001)
    (check-within (candidate (list 7 15 18 20 6 21 18 17 11 1 14 15 18 8 17 13 11 8 5 12 11) 10) (list (list 1 5 6) (list 7 8 8) (list 11 11 11) (list 12 13 14) (list 15 15 17) (list 17 18 18) (list 18 20 21)) 0.001)
    (check-within (candidate (list 13 16 17 16 6 12) 11) (list (list 6 12 13) (list 16 16 17)) 0.001)
    (check-within (candidate (list 17 17 17 16 17 17) 1) (list (list 16 17 17) (list 17 17 17)) 0.001)
    (check-within (candidate (list 6 14 6 15 14 6) 17) (list (list 6 6 6) (list 14 14 15)) 0.001)
    (check-within (candidate (list 23 19 21 10 10 13 15 19 19 3 15 3) 12) (list (list 3 3 10) (list 10 13 15) (list 15 19 19) (list 19 21 23)) 0.001)
    (check-within (candidate (list 11 4 3 11 3 27 19 10 6 12 11 24 27 1 31) 17) (list (list 1 3 3) (list 4 6 10) (list 11 11 11) (list 12 19 24) (list 27 27 31)) 0.001)
    (check-within (candidate (list 8 18 18 20 20 19 20 31 7) 17) (list (list 7 8 18) (list 18 19 20) (list 20 20 31)) 0.001)
    (check-within (candidate (list 4 22 8 12 1 4 4 17 22 4 10 1) 12) (list (list 1 1 4) (list 4 4 4) (list 8 10 12) (list 17 22 22)) 0.001)
    (check-within (candidate (list 16 15 16 6 9 22 14 16 10 26 18 16 11 18 7) 10) (list (list 6 7 9) (list 10 11 14) (list 15 16 16) (list 16 16 18) (list 18 22 26)) 0.001)
    (check-within (candidate (list 5 16 12 26 16 18 1 6 23 2 1 21 8 11 9) 14) (list (list 1 1 2) (list 5 6 8) (list 9 11 12) (list 16 16 18) (list 21 23 26)) 0.001)
    (check-within (candidate (list 6 3 24 13 19 24 13 12 15 3 6 3) 17) (list (list 3 3 3) (list 6 6 12) (list 13 13 15) (list 19 24 24)) 0.001)
))

(test-humaneval)