#lang racket

;; You are given a 0-indexed integer array nums.
;; The distinct count of a subarray of nums is defined as:
;;  * Let nums[i..j] be a subarray of nums consisting of all the indices from i to j such that
;; 0 <= i <= j < nums.length. Then the number of distinct values in nums[i..j] is called the
;; distinct count of nums[i..j].
;; Return the sum of the squares of distinct counts of all subarrays of nums.
;; A subarray is a contiguous non-empty sequence of elements within an array.

;; Example 1:
;; Input: nums = [1,2,1]
;; Output: 15
;; Explanation: Six possible subarrays are:
;; [1]: 1 distinct value
;; [2]: 1 distinct value
;; [1]: 1 distinct value
;; [1,2]: 2 distinct values
;; [2,1]: 2 distinct values
;; [1,2,1]: 2 distinct values
;; The sum of the squares of the distinct counts in all subarrays is equal to
;;  12 + 12 + 12 + 22 + 22 + 22 = 15.

;; Example 2:
;; Input: nums = [1,1]
;; Output: 3
;; Explanation: Three possible subarrays are:
;; [1]: 1 distinct value
;; [1]: 1 distinct value
;; [1,1]: 1 distinct value
;; The sum of the squares of the distinct counts in all subarrays is equal to
;;  12 + 12 + 12 = 3.

;; Constraints:
;;  * 1 <= nums.length <= 100
;;  * 1 <= nums[i] <= 100

;; MF: The AI learned about `in-suffixes` and implemented a totally simplistic version of it.
;;   This won't be fast but it sure simplifies the coding. 

(define (sumCounts nums)
  (for/fold ([total 0]) ([suffix (in-suffixes nums)])
    (for/fold ([seen (set)] [acc total] #:result acc) ([x suffix])
      (define new-seen (set-add seen x))
      (define new-acc (+ acc (sqr (set-count new-seen))))
      (values new-seen new-acc))))

;; ---------------------------------------------------------------------------------------------------

(define (sumCounts-AI nums)
  ;; Nested function to scan subarrays and calculate the sum of squares of distinct counts
  (define (scan-subarrays lst)
    (define (scan lst seen acc)
      (match lst
        [(cons x xs)
         (define new-seen (set-add seen x))
         (define new-acc (+ acc (sqr (set-count new-seen))))
         (scan xs new-seen new-acc)]
        [else acc]))
    (scan lst (set) 0))

  ;; Iterate over all suffixes of the list and sum up the results of scan-subarrays
  (for/fold ([total 0]) ([suffix (in-suffixes nums)])
    (+ total (scan-subarrays suffix))))

;; Helper function to generate all suffixes of a list
(define (in-suffixes lst)
  (for/list ([i (in-range (length lst))])
    (drop lst i)))

;; Example usage
(sumCounts '(1 2 1)) ;; Should return 15
(sumCounts '(1 1))   ;; Should return 3

(require rackunit)


(define (test-humaneval) 

  (let (( candidate sumCounts))
    (check-within (candidate (list 1 2 1)) 15 0.001)
    (check-within (candidate (list 1 1)) 3 0.001)
    (check-within (candidate (list 2 2 5 5)) 22 0.001)
    (check-within (candidate (list 5 2 4 2 1 3 2 4 3 1)) 578 0.001)
    (check-within (candidate (list 2 3 2 1 2 5 3 4 5 2)) 629 0.001)
    (check-within (candidate (list 5 1 5 2 3 5 1 5 1)) 385 0.001)
    (check-within (candidate (list 4 5 4 3 4 2)) 120 0.001)
    (check-within (candidate (list 2)) 1 0.001)
    (check-within (candidate (list 3 4 2 5 2 4 1 2 2 5)) 535 0.001)
    (check-within (candidate (list 4 4 2 4 1)) 57 0.001)
    (check-within (candidate (list 2 2 5)) 12 0.001)
    (check-within (candidate (list 4 5 1 2 2 1 3 3)) 266 0.001)
    (check-within (candidate (list 3 1 5 5 2 3 2 2 1)) 334 0.001)
    (check-within (candidate (list 2 5 2 5 3 2 5 2)) 205 0.001)
    (check-within (candidate (list 5 4 1 4 5 2 4)) 203 0.001)
    (check-within (candidate (list 1 3 3 4 3 1 2 1)) 253 0.001)
    (check-within (candidate (list 4)) 1 0.001)
    (check-within (candidate (list 1 4 2 1 5 4 3 1 4)) 507 0.001)
    (check-within (candidate (list 2 4 5 3 2 5 1 5 4 4)) 626 0.001)
    (check-within (candidate (list 3 4 1 4 5 2 2)) 220 0.001)
    (check-within (candidate (list 3 5 1 1 3)) 62 0.001)
    (check-within (candidate (list 4 3 2 5 3)) 89 0.001)
    (check-within (candidate (list 2 5)) 6 0.001)
    (check-within (candidate (list 1 5 1 4 5)) 70 0.001)
    (check-within (candidate (list 5 1)) 6 0.001)
    (check-within (candidate (list 4 5 4 3 3 5 3)) 138 0.001)
    (check-within (candidate (list 5 4 3)) 20 0.001)
    (check-within (candidate (list 5 5 3 3 4 5 4 5 5)) 234 0.001)
    (check-within (candidate (list 3 1 5 5 3 4 5 5 1 4)) 456 0.001)
    (check-within (candidate (list 4 2 3 1 1)) 81 0.001)
    (check-within (candidate (list 4 5 3 1 2 5 5 3 5)) 434 0.001)
    (check-within (candidate (list 3 2 1 2 5 2 4 5 1 5)) 531 0.001)
    (check-within (candidate (list 1 3 1 4 4)) 62 0.001)
    (check-within (candidate (list 5 1 2 1 2 1 2 3 1)) 257 0.001)
    (check-within (candidate (list 2 4)) 6 0.001)
    (check-within (candidate (list 4 5 4 5)) 28 0.001)
    (check-within (candidate (list 3 1 5 5 5 4 3 3 2)) 334 0.001)
    (check-within (candidate (list 3 2 5 2 1 5 3)) 203 0.001)
    (check-within (candidate (list 4 4 2 5 5 4 2 2 1)) 294 0.001)
    (check-within (candidate (list 1)) 1 0.001)
    (check-within (candidate (list 1 1 3 3 3 4 4)) 96 0.001)
    (check-within (candidate (list 3 2 2 3 4)) 57 0.001)
    (check-within (candidate (list 1 5 3 2 4 4)) 161 0.001)
    (check-within (candidate (list 5 4 1 1 3)) 69 0.001)
    (check-within (candidate (list 4 3 3 5 3 4 5 3 3 1)) 376 0.001)
    (check-within (candidate (list 2 3 4 1 5 1 3 3 4)) 432 0.001)
    (check-within (candidate (list 5 1 4 2 1 1)) 129 0.001)
    (check-within (candidate (list 5 4 4 1)) 30 0.001)
    (check-within (candidate (list 1 5 1 3 2 1)) 139 0.001)
    (check-within (candidate (list 5 3)) 6 0.001)
    (check-within (candidate (list 4 1 4 3)) 38 0.001)
    (check-within (candidate (list 1 5 4 3 4 2 4 5 5 4)) 513 0.001)
    (check-within (candidate (list 4 2 3 4 3 2 5 4 4)) 378 0.001)
    (check-within (candidate (list 2 3 3 2 1 5 2 2)) 262 0.001)
    (check-within (candidate (list 2 1 4 2 4 1 4 3)) 243 0.001)
    (check-within (candidate (list 1 4 4 1 3)) 57 0.001)
    (check-within (candidate (list 2 3 2 1)) 38 0.001)
    (check-within (candidate (list 1 4 2 1)) 43 0.001)
    (check-within (candidate (list 2 4 3 2 5 1)) 169 0.001)
    (check-within (candidate (list 2 5 3 2 1 3 1 3 2)) 348 0.001)
    (check-within (candidate (list 4 1)) 6 0.001)
    (check-within (candidate (list 4 3 1 4 3 4 3 4 1)) 263 0.001)
    (check-within (candidate (list 5 1 1 1 4 3)) 89 0.001)
    (check-within (candidate (list 4 5)) 6 0.001)
    (check-within (candidate (list 5 2 2 3 1 2 5 3)) 289 0.001)
    (check-within (candidate (list 3 2 4)) 20 0.001)
    (check-within (candidate (list 5 3 5 2 3 2)) 106 0.001)
    (check-within (candidate (list 3 2 1)) 20 0.001)
    (check-within (candidate (list 4 4 2 4 3)) 57 0.001)
    (check-within (candidate (list 1 4 4)) 12 0.001)
    (check-within (candidate (list 1 4 4 3 1 2 1 4 3)) 387 0.001)
    (check-within (candidate (list 1 5 4 2 5 5 5 3)) 249 0.001)
    (check-within (candidate (list 2 1 5 3)) 50 0.001)
    (check-within (candidate (list 2 3 5 1 5 2 3 2 3 4)) 533 0.001)
    (check-within (candidate (list 5 3 4 4 3 5 4 5)) 202 0.001)
    (check-within (candidate (list 4 4 2 2 4 1)) 80 0.001)
    (check-within (candidate (list 2 3)) 6 0.001)
    (check-within (candidate (list 4 2 3 2)) 38 0.001)
    (check-within (candidate (list 1 2 2)) 12 0.001)
    (check-within (candidate (list 4 1 5 1 5 4 5 1)) 205 0.001)
    (check-within (candidate (list 4 5 3 1)) 50 0.001)
    (check-within (candidate (list 4 2 3 4 2 4 3 3 2)) 275 0.001)
    (check-within (candidate (list 4 3)) 6 0.001)
    (check-within (candidate (list 1 3 5 4 4 4)) 113 0.001)
    (check-within (candidate (list 1 2 4 2 1 2 2 4 1 3)) 391 0.001)
    (check-within (candidate (list 4 2 5 3 2)) 89 0.001)
    (check-within (candidate (list 3 4 5 3 2 5)) 144 0.001)
    (check-within (candidate (list 5 4 5)) 15 0.001)
    (check-within (candidate (list 2 4 5 1)) 50 0.001)
    (check-within (candidate (list 5 4 1 4 2 1 5)) 203 0.001)
    (check-within (candidate (list 2 3 3 2 2 3 1)) 110 0.001)
    (check-within (candidate (list 1 4 2 5)) 50 0.001)
    (check-within (candidate (list 3)) 1 0.001)
    (check-within (candidate (list 5)) 1 0.001)
    (check-within (candidate (list 1 3 5 3 2 1 1 4 3)) 441 0.001)
    (check-within (candidate (list 1 5 2 2 3 3 3)) 140 0.001)
    (check-within (candidate (list 1 2 1 4 5 5 4 1 1 1)) 407 0.001)
    (check-within (candidate (list 2 2 1 1 1 2 5 4 5)) 296 0.001)
    (check-within (candidate (list 3 2 3)) 15 0.001)
    (check-within (candidate (list 2 1 5 4 3 3 2 1 5 5)) 652 0.001)
    ))

(test-humaneval)