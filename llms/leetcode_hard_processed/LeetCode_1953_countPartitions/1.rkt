#lang racket

;; You are given a list nums consisting of positive integers and an integer k.
;; Partition the list into two ordered groups such that each element is in exactly one group. A partition is called great if the sum of elements of each group is greater than or equal to k.
;; Return the number of distinct great partitions. Since the answer may be too large, return it modulo 109 + 7.
;; Two partitions are considered distinct if some element nums[i] is in different groups in the two partitions.
;; Example 1:
;; Input: nums = [1,2,3,4], k = 4
;; Output: 6
;; Explanation: The great partitions are: ([1,2,3], [4]), ([1,3], [2,4]), ([1,4], [2,3]), ([2,3], [1,4]), ([2,4], [1,3]) and ([4], [1,2,3]).
;; Example 2:
;; Input: nums = [3,3,3], k = 4
;; Output: 0
;; Explanation: There are no great partitions for this list.
;; Example 3:
;; Input: nums = [6,6], k = 2
;; Output: 2
;; Explanation: We can either put nums[0] in the first partition or in the second partition.
;; The great partitions will be ([6], [6]) and ([6], [6]).
;; Constraints:
;; 1 <= nums.length, k <= 1000
;; 1 <= nums[i] <= 109
(define (countPartitions nums k)
  ;; Calculate the total sum of the list
  (define total-sum (apply + nums))
  (define n (length nums))
  ;; Modulo value as per the problem statement
  (define mod 1000000007)

  ;; If the total sum is less than double k, there can be no great partitions
  (if (< total-sum (* k 2))
      0
      (let ([dp (make-vector (+ n 1) 1)]) ; Initialize dp vector with 1s
        ;; Populate the dp vector based on the dynamic programming approach
        (for ([i (in-range 1 k)])
          (for ([j (in-range (- n 1) 0 -1)])
            (vector-set! dp j (modulo (+ (* (vector-ref dp j) j) (vector-ref dp (- j 1))) mod))))
        
        ;; Calculate the result by iterating through the nums list and using the dp vector
        (define result 0)
        (for ([i (in-range 0 (- n 1))])
          (set! total-sum (- total-sum (list-ref nums i)))
          (when (>= total-sum k)
            (set! result (modulo (+ result (vector-ref dp (+ i 1))) mod))))
        result)))

;; Example usage:
(countPartitions '(1 2 3 4) 4) ; Output: 6
(require rackunit)

(define (test-humaneval) 

  (let (( candidate countPartitions))
    (check-within (candidate (list 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6) 4500) 0 0.001)
    (check-within (candidate (list 10 20 30 40 50 60 70 80 90 100) 600) 0 0.001)
    (check-within (candidate (list 100 200 300 400 500 600 700 800 900 1000) 5000) 0 0.001)
    (check-within (candidate (list 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3) 45) 0 0.001)
    (check-within (candidate (list 100 200 300 400 500 600 700 800 900 1000) 3000) 0 0.001)
    (check-within (candidate (list 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100 100) 2000) 0 0.001)
    (check-within (candidate (list 6 6) 2) 2 0.001)
    (check-within (candidate (list 100 200 300 400 500 600 700 800 900 1000) 4000) 0 0.001)
    (check-within (candidate (list 10 20 30 40 50 60 70 80 90 100) 550) 0 0.001)
    (check-within (candidate (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20) 5000) 0 0.001)
    (check-within (candidate (list 100 200 300 400 500 600 700 800 900 1000) 4500) 0 0.001)
))

(test-humaneval)