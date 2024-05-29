#lang racket

;; You are given a 0-indexed array nums of integers.
;; A triplet of indices (i, j, k) is a mountain if:
;;  * i < j < k
;;  * nums[i] < nums[j] and nums[k] < nums[j]
;; Return the minimum possible sum of a mountain triplet of nums. If no such triplet exists, return -1.
;; Example 1:
;; Input: nums = [8,6,1,5,3]
;; Output: 9
;; Explanation: Triplet (2, 3, 4) is a mountain triplet of sum 9 since:
;; - 2 < 3 < 4
;; - nums[2] < nums[3] and nums[4] < nums[3]
;; And the sum of this triplet is nums[2] + nums[3] + nums[4] = 9. It can be shown that there are no mountain triplets with a sum of less than 9.
;; Example 2:
;; Input: nums = [5,4,8,7,10,2]
;; Output: 13
;; Explanation: Triplet (1, 3, 5) is a mountain triplet of sum 13 since:
;; - 1 < 3 < 5
;; - nums[1] < nums[3] and nums[5] < nums[3]
;; And the sum of this triplet is nums[1] + nums[3] + nums[5] = 13. It can be shown that there are no mountain triplets with a sum of less than 13.
;; Example 3:
;; Input: nums = [6,5,4,3,4,5]
;; Output: -1
;; Explanation: It can be shown that there are no mountain triplets in nums.
;; Constraints:
;;  * 3 <= nums.length <= 50
;;  * 1 <= nums[i] <= 50
(define (minimumSum nums)
  (define n (length nums))

  (define (find-left-mountains nums n)
    (for/list ([j (in-range 1 (- n 1))])
      (for/fold ([left-min +inf.0]) ([i (in-range j)])
        (if (< (list-ref nums i) (list-ref nums j))
            (min left-min (list-ref nums i))
            left-min))))
  
  (define (find-right-mountains nums n)
    (for/list ([j (in-range 1 (- n 1))])
      (for/fold ([right-min +inf.0]) ([k (in-range (+ j 1) n)])
        (if (< (list-ref nums k) (list-ref nums j))
            (min right-min (list-ref nums k))
            right-min))))

  (define left-mountains (find-left-mountains nums n))
  (define right-mountains (find-right-mountains nums n))

  (define min-sum
    (for/fold ([min-sum +inf.0]) ([j (in-range 1 (- n 1))])
      (let ([left-min (list-ref left-mountains (- j 1))]
            [right-min (list-ref right-mountains (- j 1))])
        (if (and (not (= left-min +inf.0))
                 (not (= right-min +inf.0)))
            (min min-sum (+ left-min (list-ref nums j) right-min))
            min-sum))))

  (if (= min-sum +inf.0)
      -1
      min-sum))

;; Example usages
(minimumSum '(8 6 1 5 3))  ;; Output: 9
(minimumSum '(5 4 8 7 10 2))  ;; Output: 13
(minimumSum '(6 5 4 3 4 5))  ;; Output: -1

(require rackunit)


(define (test-humaneval) 

  (let (( candidate minimumSum))
    (check-within (candidate (list 8 6 1 5 3)) 9 0.001)
    (check-within (candidate (list 5 4 8 7 10 2)) 13 0.001)
    (check-within (candidate (list 6 5 4 3 4 5)) -1 0.001)
    (check-within (candidate (list 50 50 50)) -1 0.001)
    (check-within (candidate (list 49 50 48)) 147 0.001)
    (check-within (candidate (list 48 50 49)) 147 0.001)
    (check-within (candidate (list 1 1 1)) -1 0.001)
    (check-within (candidate (list 1 1 2)) -1 0.001)
    (check-within (candidate (list 1 1 3)) -1 0.001)
    (check-within (candidate (list 1 2 1)) 4 0.001)
    (check-within (candidate (list 1 2 2)) -1 0.001)
    (check-within (candidate (list 1 2 3)) -1 0.001)
    (check-within (candidate (list 1 3 1)) 5 0.001)
    (check-within (candidate (list 1 3 2)) 6 0.001)
    (check-within (candidate (list 1 3 3)) -1 0.001)
    (check-within (candidate (list 2 1 1)) -1 0.001)
    (check-within (candidate (list 2 1 2)) -1 0.001)
    (check-within (candidate (list 2 1 3)) -1 0.001)
    (check-within (candidate (list 2 2 1)) -1 0.001)
    (check-within (candidate (list 2 2 2)) -1 0.001)
    (check-within (candidate (list 2 2 3)) -1 0.001)
    (check-within (candidate (list 2 3 1)) 6 0.001)
    (check-within (candidate (list 2 3 2)) 7 0.001)
    (check-within (candidate (list 2 3 3)) -1 0.001)
    (check-within (candidate (list 3 1 1)) -1 0.001)
    (check-within (candidate (list 3 1 2)) -1 0.001)
    (check-within (candidate (list 3 1 3)) -1 0.001)
    (check-within (candidate (list 3 2 1)) -1 0.001)
    (check-within (candidate (list 3 2 2)) -1 0.001)
    (check-within (candidate (list 3 2 3)) -1 0.001)
    (check-within (candidate (list 3 3 1)) -1 0.001)
    (check-within (candidate (list 3 3 2)) -1 0.001)
    (check-within (candidate (list 3 3 3)) -1 0.001)
    (check-within (candidate (list 1 1 1 1)) -1 0.001)
    (check-within (candidate (list 1 1 1 2)) -1 0.001)
    (check-within (candidate (list 1 1 1 3)) -1 0.001)
    (check-within (candidate (list 1 1 1 4)) -1 0.001)
    (check-within (candidate (list 1 1 2 1)) 4 0.001)
    (check-within (candidate (list 1 1 2 2)) -1 0.001)
    (check-within (candidate (list 1 1 2 3)) -1 0.001)
    (check-within (candidate (list 1 1 2 4)) -1 0.001)
    (check-within (candidate (list 1 1 3 1)) 5 0.001)
    (check-within (candidate (list 1 1 3 2)) 6 0.001)
    (check-within (candidate (list 1 1 3 3)) -1 0.001)
    (check-within (candidate (list 1 1 3 4)) -1 0.001)
    (check-within (candidate (list 1 1 4 1)) 6 0.001)
    (check-within (candidate (list 1 1 4 2)) 7 0.001)
    (check-within (candidate (list 1 1 4 3)) 8 0.001)
    (check-within (candidate (list 1 1 4 4)) -1 0.001)
    (check-within (candidate (list 1 2 1 1)) 4 0.001)
    (check-within (candidate (list 1 2 1 2)) 4 0.001)
    (check-within (candidate (list 1 2 1 3)) 4 0.001)
    (check-within (candidate (list 1 2 1 4)) 4 0.001)
    (check-within (candidate (list 1 2 2 1)) 4 0.001)
    (check-within (candidate (list 1 2 2 2)) -1 0.001)
    (check-within (candidate (list 1 2 2 3)) -1 0.001)
    (check-within (candidate (list 1 2 2 4)) -1 0.001)
    (check-within (candidate (list 1 2 3 1)) 4 0.001)
    (check-within (candidate (list 1 2 3 2)) 6 0.001)
    (check-within (candidate (list 1 2 3 3)) -1 0.001)
    (check-within (candidate (list 1 2 3 4)) -1 0.001)
    (check-within (candidate (list 1 2 4 1)) 4 0.001)
    (check-within (candidate (list 1 2 4 2)) 7 0.001)
    (check-within (candidate (list 1 2 4 3)) 8 0.001)
    (check-within (candidate (list 1 2 4 4)) -1 0.001)
    (check-within (candidate (list 1 3 1 1)) 5 0.001)
    (check-within (candidate (list 1 3 1 2)) 5 0.001)
    (check-within (candidate (list 1 3 1 3)) 5 0.001)
    (check-within (candidate (list 1 3 1 4)) 5 0.001)
    (check-within (candidate (list 1 3 2 1)) 4 0.001)
    (check-within (candidate (list 1 3 2 2)) 6 0.001)
    (check-within (candidate (list 1 3 2 3)) 6 0.001)
    (check-within (candidate (list 1 3 2 4)) 6 0.001)
    (check-within (candidate (list 1 3 3 1)) 5 0.001)
    (check-within (candidate (list 1 3 3 2)) 6 0.001)
    (check-within (candidate (list 1 3 3 3)) -1 0.001)
    (check-within (candidate (list 1 3 3 4)) -1 0.001)
    (check-within (candidate (list 1 3 4 1)) 5 0.001)
    (check-within (candidate (list 1 3 4 2)) 6 0.001)
    (check-within (candidate (list 1 3 4 3)) 8 0.001)
    (check-within (candidate (list 1 3 4 4)) -1 0.001)
    (check-within (candidate (list 1 4 1 1)) 6 0.001)
    (check-within (candidate (list 1 4 1 2)) 6 0.001)
    (check-within (candidate (list 1 4 1 3)) 6 0.001)
    (check-within (candidate (list 1 4 1 4)) 6 0.001)
    (check-within (candidate (list 1 4 2 1)) 4 0.001)
    (check-within (candidate (list 1 4 2 2)) 7 0.001)
    (check-within (candidate (list 1 4 2 3)) 7 0.001)
    (check-within (candidate (list 1 4 2 4)) 7 0.001)
    (check-within (candidate (list 1 4 3 1)) 5 0.001)
    (check-within (candidate (list 1 4 3 2)) 6 0.001)
    (check-within (candidate (list 1 4 3 3)) 8 0.001)
    (check-within (candidate (list 1 4 3 4)) 8 0.001)
    (check-within (candidate (list 1 4 4 1)) 6 0.001)
    (check-within (candidate (list 1 4 4 2)) 7 0.001)
    (check-within (candidate (list 1 4 4 3)) 8 0.001)
    (check-within (candidate (list 1 4 4 4)) -1 0.001)
    (check-within (candidate (list 2 1 1 1)) -1 0.001)
    (check-within (candidate (list 2 1 1 2)) -1 0.001)
    (check-within (candidate (list 2 1 1 3)) -1 0.001)
))

(test-humaneval)