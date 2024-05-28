#lang racket

;; You are given a 0-indexed array of integers nums of length n, and two positive integers k and dist.
;; The cost of an array is the value of its first element. For example, the cost of [1,2,3] is 1 while the cost of [3,4,1] is 3.
;; You need to divide nums into k disjoint contiguous subarrays, such that the difference between the starting index of the second subarray and the starting index of the kth subarray should be less than or equal to dist. In other words, if you divide nums into the subarrays nums[0..(i1 - 1)], nums[i1..(i2 - 1)], ..., nums[ik-1..(n - 1)], then ik-1 - i1 <= dist.
;; Return the minimum possible sum of the cost of these subarrays.
;; Example 1:
;; Input: nums = [1,3,2,6,4,2], k = 3, dist = 3
;; Output: 5
;; Explanation: The best possible way to divide nums into 3 subarrays is: [1,3], [2,6,4], and [2]. This choice is valid because ik-1 - i1 is 5 - 2 = 3 which is equal to dist. The total cost is nums[0] + nums[2] + nums[5] which is 1 + 2 + 2 = 5.
;; It can be shown that there is no possible way to divide nums into 3 subarrays at a cost lower than 5.
;; Example 2:
;; Input: nums = [10,1,2,2,2,1], k = 4, dist = 3
;; Output: 15
;; Explanation: The best possible way to divide nums into 4 subarrays is: [10], [1], [2], and [2,2,1]. This choice is valid because ik-1 - i1 is 3 - 1 = 2 which is less than dist. The total cost is nums[0] + nums[1] + nums[2] + nums[3] which is 10 + 1 + 2 + 2 = 15.
;; The division [10], [1], [2,2,2], and [1] is not valid, because the difference between ik-1 and i1 is 5 - 1 = 4, which is greater than dist.
;; It can be shown that there is no possible way to divide nums into 4 subarrays at a cost lower than 15.
;; Example 3:
;; Input: nums = [10,8,18,9], k = 3, dist = 1
;; Output: 36
;; Explanation: The best possible way to divide nums into 4 subarrays is: [10], [8], and [18,9]. This choice is valid because ik-1 - i1 is 2 - 1 = 1 which is equal to dist.The total cost is nums[0] + nums[1] + nums[2] which is 10 + 8 + 18 = 36.
;; The division [10], [8,18], and [9] is not valid, because the difference between ik-1 and i1 is 3 - 1 = 2, which is greater than dist.
;; It can be shown that there is no possible way to divide nums into 3 subarrays at a cost lower than 36.
;; Constraints:
;; 3 <= n <= 105
;; 1 <= nums[i] <= 109
;; 3 <= k <= n
;; k - 2 <= dist <= n - 2
(define (minimumCost nums k dist)
  (define n (length nums))

  (define (calculate-cost start-indices)
    (apply + (map (lambda (idx) (list-ref nums idx)) start-indices)))

  (define (dfs depth current-indices min-cost)
    (if (= depth k)
        (if (<= (- (list-ref current-indices (- k 1))
                   (list-ref current-indices 1))
                dist)
            (min min-cost (calculate-cost current-indices))
            min-cost)
        (let ([start-index (last current-indices)]
              [next-start-limit (- n (- k depth))])
          (for/fold ([min-cost min-cost])
                    ([next-start (in-range (+ start-index 1) (+ next-start-limit 1))])
            (dfs (+ depth 1) (append current-indices (list next-start)) min-cost)))))

  (if (= k 1)
      (first nums)
      (for/fold ([min-cost +inf.0])
                ([first-end (in-range 1 (+ (- n k) 2))])
        (dfs 2 (list 0 first-end) min-cost))))

;; Example usage
(minimumCost '(1 3 2 6 4 2) 3 3)  ; Output: 5
(minimumCost '(10 1 2 2 2 1) 4 3) ; Output: 15
(minimumCost '(10 8 18 9) 3 1)    ; Output: 36

(require rackunit)


(define (test-humaneval) 

  (let (( candidate minimumCost))
    (check-within (candidate (list 1 3 2 6 4 2) 3 3) 5 0.001)
    (check-within (candidate (list 10 1 2 2 2 1) 4 3) 15 0.001)
    (check-within (candidate (list 10 8 18 9) 3 1) 36 0.001)
    (check-within (candidate (list 1 1 1) 3 1) 3 0.001)
    (check-within (candidate (list 1 1 3) 3 1) 5 0.001)
    (check-within (candidate (list 1 2 2) 3 1) 5 0.001)
    (check-within (candidate (list 1 2 5) 3 1) 8 0.001)
    (check-within (candidate (list 1 4 4) 3 1) 9 0.001)
    (check-within (candidate (list 2 2 1) 3 1) 5 0.001)
    (check-within (candidate (list 2 3 2) 3 1) 7 0.001)
    (check-within (candidate (list 2 5 4) 3 1) 11 0.001)
    (check-within (candidate (list 3 1 2) 3 1) 6 0.001)
    (check-within (candidate (list 3 1 3) 3 1) 7 0.001)
    (check-within (candidate (list 3 2 2) 3 1) 7 0.001)
    (check-within (candidate (list 3 3 2) 3 1) 8 0.001)
    (check-within (candidate (list 3 4 1) 3 1) 8 0.001)
    (check-within (candidate (list 3 5 3) 3 1) 11 0.001)
    (check-within (candidate (list 4 1 4) 3 1) 9 0.001)
    (check-within (candidate (list 4 1 5) 3 1) 10 0.001)
    (check-within (candidate (list 4 2 1) 3 1) 7 0.001)
    (check-within (candidate (list 4 2 2) 3 1) 8 0.001)
    (check-within (candidate (list 4 2 4) 3 1) 10 0.001)
    (check-within (candidate (list 4 2 5) 3 1) 11 0.001)
    (check-within (candidate (list 4 3 1) 3 1) 8 0.001)
    (check-within (candidate (list 4 3 2) 3 1) 9 0.001)
    (check-within (candidate (list 4 5 3) 3 1) 12 0.001)
    (check-within (candidate (list 5 2 1) 3 1) 8 0.001)
    (check-within (candidate (list 5 3 5) 3 1) 13 0.001)
    (check-within (candidate (list 50 50 50) 3 1) 150 0.001)
    (check-within (candidate (list 1 5 3 6) 3 2) 9 0.001)
    (check-within (candidate (list 1 5 3 7) 3 1) 9 0.001)
    (check-within (candidate (list 1 5 3 7) 3 2) 9 0.001)
    (check-within (candidate (list 1 5 3 8) 3 1) 9 0.001)
    (check-within (candidate (list 1 5 3 8) 3 2) 9 0.001)
    (check-within (candidate (list 1 5 4 6) 4 2) 16 0.001)
    (check-within (candidate (list 1 6 3 5) 3 2) 9 0.001)
    (check-within (candidate (list 1 6 3 8) 3 2) 10 0.001)
    (check-within (candidate (list 1 6 4 5) 4 2) 16 0.001)
    (check-within (candidate (list 1 7 4 6) 4 2) 18 0.001)
    (check-within (candidate (list 1 7 4 8) 4 2) 20 0.001)
    (check-within (candidate (list 1 8 3 8) 3 1) 12 0.001)
    (check-within (candidate (list 1 8 4 7) 4 2) 20 0.001)
    (check-within (candidate (list 2 5 3 8) 3 2) 10 0.001)
    (check-within (candidate (list 2 5 4 7) 4 2) 18 0.001)
    (check-within (candidate (list 2 5 4 8) 4 2) 19 0.001)
    (check-within (candidate (list 2 6 3 5) 3 1) 10 0.001)
    (check-within (candidate (list 2 6 3 6) 3 1) 11 0.001)
    (check-within (candidate (list 2 6 4 5) 4 2) 17 0.001)
    (check-within (candidate (list 2 6 4 7) 4 2) 19 0.001)
    (check-within (candidate (list 2 6 4 8) 4 2) 20 0.001)
    (check-within (candidate (list 2 7 3 5) 3 2) 10 0.001)
    (check-within (candidate (list 2 7 3 8) 3 2) 12 0.001)
    (check-within (candidate (list 2 7 4 6) 4 2) 19 0.001)
    (check-within (candidate (list 2 8 3 5) 3 1) 10 0.001)
    (check-within (candidate (list 2 8 4 5) 4 2) 19 0.001)
    (check-within (candidate (list 3 5 3 5) 3 1) 11 0.001)
    (check-within (candidate (list 3 5 3 5) 3 2) 11 0.001)
    (check-within (candidate (list 3 5 3 8) 3 2) 11 0.001)
    (check-within (candidate (list 3 5 4 7) 4 2) 19 0.001)
    (check-within (candidate (list 3 6 3 5) 3 1) 11 0.001)
    (check-within (candidate (list 3 6 3 7) 3 2) 12 0.001)
    (check-within (candidate (list 3 6 3 8) 3 1) 12 0.001)
    (check-within (candidate (list 3 6 4 5) 4 2) 18 0.001)
    (check-within (candidate (list 3 7 3 5) 3 1) 11 0.001)
    (check-within (candidate (list 3 7 3 5) 3 2) 11 0.001)
    (check-within (candidate (list 3 7 3 6) 3 1) 12 0.001)
    (check-within (candidate (list 3 7 3 8) 3 2) 13 0.001)
    (check-within (candidate (list 3 7 4 7) 4 2) 21 0.001)
    (check-within (candidate (list 3 8 3 5) 3 1) 11 0.001)
    (check-within (candidate (list 3 8 4 6) 4 2) 21 0.001)
    (check-within (candidate (list 4 5 3 5) 3 2) 12 0.001)
    (check-within (candidate (list 4 5 3 6) 3 2) 12 0.001)
    (check-within (candidate (list 4 5 3 8) 3 2) 12 0.001)
    (check-within (candidate (list 4 5 4 5) 4 2) 18 0.001)
    (check-within (candidate (list 4 6 3 6) 3 1) 13 0.001)
    (check-within (candidate (list 4 6 3 7) 3 1) 13 0.001)
    (check-within (candidate (list 4 6 4 5) 4 2) 19 0.001)
    (check-within (candidate (list 4 6 4 8) 4 2) 22 0.001)
    (check-within (candidate (list 4 7 3 6) 3 2) 13 0.001)
    (check-within (candidate (list 4 7 4 5) 4 2) 20 0.001)
    (check-within (candidate (list 4 7 4 7) 4 2) 22 0.001)
    (check-within (candidate (list 4 8 3 5) 3 2) 12 0.001)
    (check-within (candidate (list 4 8 3 6) 3 1) 13 0.001)
    (check-within (candidate (list 4 8 3 7) 3 2) 14 0.001)
    (check-within (candidate (list 4 8 4 6) 4 2) 22 0.001)
    (check-within (candidate (list 4 8 4 8) 4 2) 24 0.001)
    (check-within (candidate (list 1 5 6 6 3 7 2) 6 5) 23 0.001)
    (check-within (candidate (list 1 6 4 6 2 9 11) 4 3) 13 0.001)
    (check-within (candidate (list 1 6 4 7 9 6 1) 4 4) 12 0.001)
    (check-within (candidate (list 1 6 5 6 4 9 11) 5 5) 22 0.001)
    (check-within (candidate (list 1 6 5 7 8 7 5) 5 4) 25 0.001)
    (check-within (candidate (list 1 6 5 8 11 10 6) 5 3) 31 0.001)
    (check-within (candidate (list 1 6 6 8 4 8 7) 6 4) 33 0.001)
    (check-within (candidate (list 1 7 6 8 5 10 10) 6 5) 37 0.001)
    (check-within (candidate (list 1 8 3 8 11 11 10) 3 5) 12 0.001)
    (check-within (candidate (list 1 8 4 7 11 1 8) 4 4) 13 0.001)
    (check-within (candidate (list 1 8 6 5 6 12 12) 6 5) 38 0.001)
    (check-within (candidate (list 1 8 6 6 12 5 2) 6 5) 28 0.001)
    (check-within (candidate (list 2 5 3 5 7 4 3) 3 3) 9 0.001)
    (check-within (candidate (list 2 5 4 6 6 1 3) 4 5) 10 0.001)
))

(test-humaneval)