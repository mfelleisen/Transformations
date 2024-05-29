#lang racket

;; You are given a 0-indexed array maxHeights of n integers.
;; You are tasked with building n towers in the coordinate line. The ith tower is built at coordinate i and has a height of heights[i].
;; A configuration of towers is beautiful if the following conditions hold:
;;  1. 1 <= heights[i] <= maxHeights[i]
;;  2. heights is a mountain array.
;; Array heights is a mountain if there exists an index i such that:
;;  * For all 0 < j <= i, heights[j - 1] <= heights[j]
;;  * For all i <= k < n - 1, heights[k + 1] <= heights[k]
;; Return the maximum possible sum of heights of a beautiful configuration of towers.
;; Example 1:
;; Input: maxHeights = [5,3,4,1,1]
;; Output: 13
;; Explanation: One beautiful configuration with a maximum sum is heights = [5,3,3,1,1]. This configuration is beautiful since:
;; - 1 <= heights[i] <= maxHeights[i]
;; - heights is a mountain of peak i = 0.
;; It can be shown that there exists no other beautiful configuration with a sum of heights greater than 13.
;; Example 2:
;; Input: maxHeights = [6,5,3,9,2,7]
;; Output: 22
;; Explanation: One beautiful configuration with a maximum sum is heights = [3,3,3,9,2,2]. This configuration is beautiful since:
;; - 1 <= heights[i] <= maxHeights[i]
;; - heights is a mountain of peak i = 3.
;; It can be shown that there exists no other beautiful configuration with a sum of heights greater than 22.
;; Example 3:
;; Input: maxHeights = [3,2,5,5,2,3]
;; Output: 18
;; Explanation: One beautiful configuration with a maximum sum is heights = [2,2,5,5,2,2]. This configuration is beautiful since:
;; - 1 <= heights[i] <= maxHeights[i]
;; - heights is a mountain of peak i = 2.
;; Note that, for this configuration, i = 3 can also be considered a peak.
;; It can be shown that there exists no other beautiful configuration with a sum of heights greater than 18.
;; Constraints:
;;  * 1 <= n == maxHeights <= 103
;;  * 1 <= maxHeights[i] <= 109
(define (maximumSumOfHeights maxHeights)
  ;; Function to calculate the maximum sum with a fixed peak position
  (define (calculate-max-sum-with-peak peak)
    (define n (length maxHeights))
    ;; Initialize the heights array with 0's
    (define heights (make-vector n 0))
    ;; Set the peak height
    (vector-set! heights peak (list-ref maxHeights peak))
    ;; Expand to the left of the peak
    (for ([i (in-range (sub1 peak) -1 -1)])
      (vector-set! heights i (min (list-ref maxHeights i) (vector-ref heights (add1 i)))))
    ;; Expand to the right of the peak
    (for ([i (in-range (add1 peak) n)])
      (vector-set! heights i (min (list-ref maxHeights i) (vector-ref heights (sub1 i)))))
    ;; Calculate the sum of the heights array
    (apply + (vector->list heights)))

  ;; Try every position as a peak and find the maximum sum
  (define (find-max-sum n)
    (for/fold ([max-sum 0]) ([i (in-range n)])
      (max max-sum (calculate-max-sum-with-peak i))))

  ;; Start the process
  (find-max-sum (length maxHeights)))

;; Example usage
(maximumSumOfHeights '(5 3 4 1 1))  ;; Output: 13
(maximumSumOfHeights '(6 5 3 9 2 7))  ;; Output: 22
(maximumSumOfHeights '(3 2 5 5 2 3))  ;; Output: 18

(require rackunit)


(define (test-humaneval) 

  (let (( candidate maximumSumOfHeights))
    (check-within (candidate (list 5 3 4 1 1)) 13 0.001)
    (check-within (candidate (list 6 5 3 9 2 7)) 22 0.001)
    (check-within (candidate (list 3 2 5 5 2 3)) 18 0.001)
    (check-within (candidate (list 1000000000)) 1000000000 0.001)
    (check-within (candidate (list 1)) 1 0.001)
    (check-within (candidate (list 933754743)) 933754743 0.001)
    (check-within (candidate (list 1 1000000000)) 1000000001 0.001)
    (check-within (candidate (list 1000000000 1000000000)) 2000000000 0.001)
    (check-within (candidate (list 999999999 1000000000)) 1999999999 0.001)
    (check-within (candidate (list 1000000000 999999999)) 1999999999 0.001)
    (check-within (candidate (list 30 1)) 31 0.001)
    (check-within (candidate (list 1 12 19)) 32 0.001)
    (check-within (candidate (list 1000000000 1000000000 1000000000)) 3000000000 0.001)
    (check-within (candidate (list 999999999 1000000000 999999999)) 2999999998 0.001)
    (check-within (candidate (list 1000000000 999999999 999999998)) 2999999997 0.001)
    (check-within (candidate (list 999999998 999999999 1000000000)) 2999999997 0.001)
    (check-within (candidate (list 1 1 1)) 3 0.001)
    (check-within (candidate (list 1 1 4 3 3 3 6)) 20 0.001)
    (check-within (candidate (list 2 4 1 3 5)) 11 0.001)
    (check-within (candidate (list 1 5 2 5 6 4 6 3 4 5)) 33 0.001)
    (check-within (candidate (list 3 6 3 5 5 1 2 5 5 6)) 24 0.001)
    (check-within (candidate (list 1 6 5 6 2 4 1 5)) 23 0.001)
    (check-within (candidate (list 5 1 6 5 4 4 2)) 23 0.001)
    (check-within (candidate (list 3 4 3 1 1 3)) 13 0.001)
    (check-within (candidate (list 4 1 6 5 3 6)) 19 0.001)
    (check-within (candidate (list 3 5 5 6 4 6 5 6)) 35 0.001)
    (check-within (candidate (list 6 4 3 3)) 16 0.001)
    (check-within (candidate (list 6 4 3 6 1 2 2 3)) 20 0.001)
    (check-within (candidate (list 6 5 1 4 6 1 5)) 16 0.001)
    (check-within (candidate (list 2 3 4 4 3 2 3 5 5 5)) 30 0.001)
    (check-within (candidate (list 5 4 6 1 2)) 16 0.001)
    (check-within (candidate (list 1 4 2)) 7 0.001)
    (check-within (candidate (list 5 2 4 4)) 12 0.001)
    (check-within (candidate (list 1 5 5 3 3)) 17 0.001)
    (check-within (candidate (list 3 1 1 4 5 5 4 6)) 25 0.001)
    (check-within (candidate (list 1 4 3 4 5 1)) 17 0.001)
    (check-within (candidate (list 5 5 3 1 1 2 5 5)) 18 0.001)
    (check-within (candidate (list 3 1 3 2 6 1 4 4 6)) 20 0.001)
    (check-within (candidate (list 5 3 3)) 11 0.001)
    (check-within (candidate (list 5 2 1 4 3 5)) 14 0.001)
    (check-within (candidate (list 1 3 2 1)) 7 0.001)
    (check-within (candidate (list 1 3 6)) 10 0.001)
    (check-within (candidate (list 5 5 5 3 3 3 3)) 27 0.001)
    (check-within (candidate (list 1 3 3 2 1 2)) 11 0.001)
    (check-within (candidate (list 5 5 4 1 4 4 5 6 4)) 27 0.001)
    (check-within (candidate (list 3 5 5 6 2)) 21 0.001)
    (check-within (candidate (list 4 6 6 1)) 17 0.001)
    (check-within (candidate (list 4 2 6 1 4 1 5 3 6)) 18 0.001)
    (check-within (candidate (list 4 1 6 3 6 6)) 20 0.001)
    (check-within (candidate (list 5 2 1 4 1 6 1 5 3 4)) 18 0.001)
    (check-within (candidate (list 1 4 6 3 5 1)) 18 0.001)
    (check-within (candidate (list 6 1 2 5)) 9 0.001)
    (check-within (candidate (list 6 1 5 1 6 2 2)) 14 0.001)
    (check-within (candidate (list 6 1 2 3 4 4)) 15 0.001)
    (check-within (candidate (list 6 1 5)) 8 0.001)
    (check-within (candidate (list 1 6 6 3 5 6 1 1)) 24 0.001)
    (check-within (candidate (list 2 6 1 5 1 2 3)) 13 0.001)
    (check-within (candidate (list 3 5 1 6 3 6)) 15 0.001)
    (check-within (candidate (list 4 4 5 3)) 16 0.001)
    (check-within (candidate (list 3 5 4 4 3 1 1 6)) 22 0.001)
    (check-within (candidate (list 5 6 4 4 5 1 2 3 5 6)) 28 0.001)
    (check-within (candidate (list 2 5 1 5 5)) 13 0.001)
    (check-within (candidate (list 1 2 6 2 6 5 5)) 23 0.001)
    (check-within (candidate (list 1 1 6 4 5)) 16 0.001)
    (check-within (candidate (list 3 4 1 6 2)) 11 0.001)
    (check-within (candidate (list 1 3 3 5 6 4 6 5 2 2)) 34 0.001)
    (check-within (candidate (list 2 4 6 4 6 3 1 5 6)) 26 0.001)
    (check-within (candidate (list 1 6 1 6 4)) 13 0.001)
    (check-within (candidate (list 4 2 1)) 7 0.001)
    (check-within (candidate (list 1 2 4 1)) 8 0.001)
    (check-within (candidate (list 1 3 3 2 5 1 4 3 1 5)) 17 0.001)
    (check-within (candidate (list 4 3 1 4)) 9 0.001)
    (check-within (candidate (list 5 1 3 4 5)) 14 0.001)
    (check-within (candidate (list 1 5 4 2)) 12 0.001)
    (check-within (candidate (list 2 6 4 3 2 2 2 5 6 5)) 30 0.001)
    (check-within (candidate (list 2 6 1 2 1 1 2 4)) 14 0.001)
    (check-within (candidate (list 1 2 5 3 3 3 5)) 20 0.001)
    (check-within (candidate (list 2 5 2 2 1)) 12 0.001)
    (check-within (candidate (list 3 5 3 4)) 14 0.001)
    (check-within (candidate (list 4 6 6 3 4 1 6)) 24 0.001)
    (check-within (candidate (list 2 3 5 3 4 1 1 1 3 2)) 21 0.001)
    (check-within (candidate (list 6 6 5 3 5 4 5)) 29 0.001)
    (check-within (candidate (list 5 5 2 2 4 2 3 2 4 4)) 26 0.001)
    (check-within (candidate (list 1 2 5)) 8 0.001)
    (check-within (candidate (list 3 6 2 4 5 2 2 5 1)) 22 0.001)
    (check-within (candidate (list 6 2 6 3 4 6 2)) 22 0.001)
    (check-within (candidate (list 6 2 5)) 10 0.001)
    (check-within (candidate (list 5 4 2 6)) 13 0.001)
    (check-within (candidate (list 2 6 5 3)) 16 0.001)
    (check-within (candidate (list 4 5 4 1 6 5 1 3)) 18 0.001)
    (check-within (candidate (list 4 1 2 4 6 2 6 3)) 20 0.001)
    (check-within (candidate (list 6 4 4 4 2)) 20 0.001)
    (check-within (candidate (list 5 5 4 2)) 16 0.001)
    (check-within (candidate (list 5 3 3 4 2 2 1 4)) 20 0.001)
    (check-within (candidate (list 6 5 6 6 6)) 28 0.001)
    (check-within (candidate (list 6 3 5 6 2 2 3)) 23 0.001)
    (check-within (candidate (list 4 6 4 5)) 18 0.001)
    (check-within (candidate (list 3 5 4 2 1 1 5 6 1)) 19 0.001)
    (check-within (candidate (list 2 3 5 6 2 4)) 20 0.001)
    (check-within (candidate (list 4 5 2 1 4 6 6 1 6 2)) 23 0.001)
))

(test-humaneval)