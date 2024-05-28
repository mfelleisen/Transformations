#lang racket

;; You are given an array of integers nums of length n.
;; The cost of an array is the value of its first element. For example, the cost of [1,2,3] is 1 while the cost of [3,4,1] is 3.
;; You need to divide nums into 3 disjoint contiguous subarrays.
;; Return the minimum possible sum of the cost of these subarrays.
;; Example 1:
;; Input: nums = [1,2,3,12]
;; Output: 6
;; Explanation: The best possible way to form 3 subarrays is: [1], [2], and [3,12] at a total cost of 1 + 2 + 3 = 6.
;; The other possible ways to form 3 subarrays are:
;; - [1], [2,3], and [12] at a total cost of 1 + 2 + 12 = 15.
;; - [1,2], [3], and [12] at a total cost of 1 + 3 + 12 = 16.
;; Example 2:
;; Input: nums = [5,4,3]
;; Output: 12
;; Explanation: The best possible way to form 3 subarrays is: [5], [4], and [3] at a total cost of 5 + 4 + 3 = 12.
;; It can be shown that 12 is the minimum cost achievable.
;; Example 3:
;; Input: nums = [10,3,1,1]
;; Output: 12
;; Explanation: The best possible way to form 3 subarrays is: [10,3], [1], and [1] at a total cost of 10 + 1 + 1 = 12.
;; It can be shown that 12 is the minimum cost achievable.
;; Constraints:
;; 3 <= n <= 50
;; 1 <= nums[i] <= 50
(define (minimumCost nums)
  ;; Function to determine the minimum cost of dividing an array into 3 contiguous subarrays.
  (let* ((n (length nums))
         (initial-cost (if (= n 3)
                           (apply + nums)  ; If there are exactly 3 elements, the cost is the sum of all elements.
                           +inf.0)))       ; Initialize initial-cost with infinity to find minimum.
    ;; Use list comprehensions to iterate over all possible pairs (i, j) such that 0 < i < j < n to split the array into 3 parts.
    (define costs
      (for*/list ([i (in-range 1 (- n 1))]
                  [j (in-range (+ i 1) n)])
        (let ((cost1 (list-ref nums 0))  ; Cost of the first subarray starting from the first element.
              (cost2 (list-ref nums i))  ; Cost of the second subarray starting at index i.
              (cost3 (list-ref nums j))) ; Cost of the third subarray starting at index j.
          (+ cost1 cost2 cost3))))       ; Calculate total cost for this partition.
    (apply min (cons initial-cost costs))))  ; Return the minimum cost found.


(require rackunit)


(define (test-humaneval) 

  (let (( candidate minimumCost))
    (check-within (candidate (list 1 2 3 12)) 6 0.001)
    (check-within (candidate (list 5 4 3)) 12 0.001)
    (check-within (candidate (list 10 3 1 1)) 12 0.001)
    (check-within (candidate (list 1 1 1)) 3 0.001)
    (check-within (candidate (list 1 1 2)) 4 0.001)
    (check-within (candidate (list 1 1 3)) 5 0.001)
    (check-within (candidate (list 1 1 4)) 6 0.001)
    (check-within (candidate (list 1 1 5)) 7 0.001)
    (check-within (candidate (list 1 2 1)) 4 0.001)
    (check-within (candidate (list 1 2 2)) 5 0.001)
    (check-within (candidate (list 1 2 3)) 6 0.001)
    (check-within (candidate (list 1 2 4)) 7 0.001)
    (check-within (candidate (list 1 2 5)) 8 0.001)
    (check-within (candidate (list 1 3 1)) 5 0.001)
    (check-within (candidate (list 1 3 2)) 6 0.001)
    (check-within (candidate (list 1 3 3)) 7 0.001)
    (check-within (candidate (list 1 3 4)) 8 0.001)
    (check-within (candidate (list 1 3 5)) 9 0.001)
    (check-within (candidate (list 1 4 1)) 6 0.001)
    (check-within (candidate (list 1 4 2)) 7 0.001)
    (check-within (candidate (list 1 4 3)) 8 0.001)
    (check-within (candidate (list 1 4 4)) 9 0.001)
    (check-within (candidate (list 1 4 5)) 10 0.001)
    (check-within (candidate (list 1 5 1)) 7 0.001)
    (check-within (candidate (list 1 5 2)) 8 0.001)
    (check-within (candidate (list 1 5 3)) 9 0.001)
    (check-within (candidate (list 1 5 4)) 10 0.001)
    (check-within (candidate (list 1 5 5)) 11 0.001)
    (check-within (candidate (list 2 1 1)) 4 0.001)
    (check-within (candidate (list 2 1 2)) 5 0.001)
    (check-within (candidate (list 2 1 3)) 6 0.001)
    (check-within (candidate (list 2 1 4)) 7 0.001)
    (check-within (candidate (list 2 1 5)) 8 0.001)
    (check-within (candidate (list 2 2 1)) 5 0.001)
    (check-within (candidate (list 2 2 2)) 6 0.001)
    (check-within (candidate (list 2 2 3)) 7 0.001)
    (check-within (candidate (list 2 2 4)) 8 0.001)
    (check-within (candidate (list 2 2 5)) 9 0.001)
    (check-within (candidate (list 2 3 1)) 6 0.001)
    (check-within (candidate (list 2 3 2)) 7 0.001)
    (check-within (candidate (list 2 3 3)) 8 0.001)
    (check-within (candidate (list 2 3 4)) 9 0.001)
    (check-within (candidate (list 2 3 5)) 10 0.001)
    (check-within (candidate (list 2 4 1)) 7 0.001)
    (check-within (candidate (list 2 4 2)) 8 0.001)
    (check-within (candidate (list 2 4 3)) 9 0.001)
    (check-within (candidate (list 2 4 4)) 10 0.001)
    (check-within (candidate (list 2 4 5)) 11 0.001)
    (check-within (candidate (list 2 5 1)) 8 0.001)
    (check-within (candidate (list 2 5 2)) 9 0.001)
    (check-within (candidate (list 2 5 3)) 10 0.001)
    (check-within (candidate (list 2 5 4)) 11 0.001)
    (check-within (candidate (list 2 5 5)) 12 0.001)
    (check-within (candidate (list 3 1 1)) 5 0.001)
    (check-within (candidate (list 3 1 2)) 6 0.001)
    (check-within (candidate (list 3 1 3)) 7 0.001)
    (check-within (candidate (list 3 1 4)) 8 0.001)
    (check-within (candidate (list 3 1 5)) 9 0.001)
    (check-within (candidate (list 3 2 1)) 6 0.001)
    (check-within (candidate (list 3 2 2)) 7 0.001)
    (check-within (candidate (list 3 2 3)) 8 0.001)
    (check-within (candidate (list 3 2 4)) 9 0.001)
    (check-within (candidate (list 3 2 5)) 10 0.001)
    (check-within (candidate (list 3 3 1)) 7 0.001)
    (check-within (candidate (list 3 3 2)) 8 0.001)
    (check-within (candidate (list 3 3 3)) 9 0.001)
    (check-within (candidate (list 3 3 4)) 10 0.001)
    (check-within (candidate (list 3 3 5)) 11 0.001)
    (check-within (candidate (list 3 4 1)) 8 0.001)
    (check-within (candidate (list 3 4 2)) 9 0.001)
    (check-within (candidate (list 3 4 3)) 10 0.001)
    (check-within (candidate (list 3 4 4)) 11 0.001)
    (check-within (candidate (list 3 4 5)) 12 0.001)
    (check-within (candidate (list 3 5 1)) 9 0.001)
    (check-within (candidate (list 3 5 2)) 10 0.001)
    (check-within (candidate (list 3 5 3)) 11 0.001)
    (check-within (candidate (list 3 5 4)) 12 0.001)
    (check-within (candidate (list 3 5 5)) 13 0.001)
    (check-within (candidate (list 4 1 1)) 6 0.001)
    (check-within (candidate (list 4 1 2)) 7 0.001)
    (check-within (candidate (list 4 1 3)) 8 0.001)
    (check-within (candidate (list 4 1 4)) 9 0.001)
    (check-within (candidate (list 4 1 5)) 10 0.001)
    (check-within (candidate (list 4 2 1)) 7 0.001)
    (check-within (candidate (list 4 2 2)) 8 0.001)
    (check-within (candidate (list 4 2 3)) 9 0.001)
    (check-within (candidate (list 4 2 4)) 10 0.001)
    (check-within (candidate (list 4 2 5)) 11 0.001)
    (check-within (candidate (list 4 3 1)) 8 0.001)
    (check-within (candidate (list 4 3 2)) 9 0.001)
    (check-within (candidate (list 4 3 3)) 10 0.001)
    (check-within (candidate (list 4 3 4)) 11 0.001)
    (check-within (candidate (list 4 3 5)) 12 0.001)
    (check-within (candidate (list 4 4 1)) 9 0.001)
    (check-within (candidate (list 4 4 2)) 10 0.001)
    (check-within (candidate (list 4 4 3)) 11 0.001)
    (check-within (candidate (list 4 4 4)) 12 0.001)
    (check-within (candidate (list 4 4 5)) 13 0.001)
    (check-within (candidate (list 4 5 1)) 10 0.001)
    (check-within (candidate (list 4 5 2)) 11 0.001)
))

(test-humaneval)