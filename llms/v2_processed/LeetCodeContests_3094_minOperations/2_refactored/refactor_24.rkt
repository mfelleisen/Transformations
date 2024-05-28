#lang racket

;; You are given a 0-indexed array nums consisting of positive integers.
;; There are two types of operations that you can apply on the array any number of times:
;;  * Choose two elements with equal values and delete them from the array.
;;  * Choose three elements with equal values and delete them from the array.
;; Return the minimum number of operations required to make the array empty, or -1 if it is not possible.
;; Example 1:
;; Input: nums = [2,3,3,2,2,4,2,3,4]
;; Output: 4
;; Explanation: We can apply the following operations to make the array empty:
;; - Apply the first operation on the elements at indices 0 and 3. The resulting array is nums = [3,3,2,4,2,3,4].
;; - Apply the first operation on the elements at indices 2 and 4. The resulting array is nums = [3,3,4,3,4].
;; - Apply the second operation on the elements at indices 0, 1, and 3. The resulting array is nums = [4,4].
;; - Apply the first operation on the elements at indices 0 and 1. The resulting array is nums = [].
;; It can be shown that we cannot make the array empty in less than 4 operations.
;; Example 2:
;; Input: nums = [2,1,2,2,3,3]
;; Output: -1
;; Explanation: It is impossible to empty the array.
;; Constraints:
;;  * 2 <= nums.length <= 105
;;  * 1 <= nums[i] <= 106
(define (minOperations nums)
  (define freqs
    (for/fold ([table (hash)])
              ([num (in-list nums)])
      (hash-update table num add1 0)))
  
  (define (calc-ops freq)
    (match (remainder freq 3)
      [0 (quotient freq 3)]
      [1 (if (>= freq 4)
             (+ 2 (quotient (- freq 4) 3))
             -1)]
      [2 (if (>= freq 2)
             (+ 1 (quotient (- freq 2) 3))
             -1)]))
  
  (define ops
    (map calc-ops (hash-values freqs)))
  
  (if (member -1 ops)
      -1
      (apply + ops)))

;; Example usage:
(minOperations '(2 3 3 2 2 4 2 3 4))  ; Output: 4
(minOperations '(2 1 2 2 3 3))       ; Output: -1

(require rackunit)


(define (test-humaneval) 

  (let (( candidate minOperations))
    (check-within (candidate (list 2 3 3 2 2 4 2 3 4)) 4 0.001)
    (check-within (candidate (list 2 1 2 2 3 3)) -1 0.001)
    (check-within (candidate (list 3 3)) 1 0.001)
    (check-within (candidate (list 14 12 14 14 12 14 14 12 12 12 12 14 14 12 14 14 14 12 12)) 7 0.001)
    (check-within (candidate (list 2 2 2 2 2 2 2 2 2)) 3 0.001)
    (check-within (candidate (list 15 3 3 15 15 13 8 15 6 15 3 1 8 8 15)) -1 0.001)
    (check-within (candidate (list 19 19 19 19 19 19 19 19 19 19 19 19 19)) 5 0.001)
    (check-within (candidate (list 13 7 13 7 13 7 13 13 7)) 4 0.001)
    (check-within (candidate (list 5 5)) 1 0.001)
    (check-within (candidate (list 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3)) 5 0.001)
    (check-within (candidate (list 3 14 3 14 3 14 14 3 3 14 14 14 3 14 14 3 14 14 14 3)) 7 0.001)
    (check-within (candidate (list 16 16 16 19 16 3 16 8 16 16 16 19 3 16 16)) -1 0.001)
    (check-within (candidate (list 11 11 11 11 19 11 11 11 11 11 19 11 11 11 11 11 19)) 6 0.001)
    (check-within (candidate (list 1 1 1 5 1 5 1 1 1 1 1 1 1)) 5 0.001)
    (check-within (candidate (list 16 16 16 3 16 16 3)) 3 0.001)
    (check-within (candidate (list 14 4 4 19 19)) -1 0.001)
    (check-within (candidate (list 1 14 1 1 1)) -1 0.001)
    (check-within (candidate (list 3 10 11 3 3 11 3 3 3 3 3 3 3 3 10 3 3 3)) 7 0.001)
    (check-within (candidate (list 3 8 8 8 8 3 8 8 8 8 8 8 8 8)) 5 0.001)
    (check-within (candidate (list 9 9 9)) 1 0.001)
    (check-within (candidate (list 6 6 6 6 6 8 8 8 8 6 8 6 15 15 6 15 6 6)) 7 0.001)
    (check-within (candidate (list 9 19 19 19 9 9 19 19 19 9 9 19 9 19 19 19)) 6 0.001)
    (check-within (candidate (list 9 4 9 20 20 4 20)) 3 0.001)
    (check-within (candidate (list 6 9 18 16 18 1 9 1 1 1 1 16 1 6 1 1 9 6)) 7 0.001)
    (check-within (candidate (list 11 18 11 18 11 18 11)) 3 0.001)
    (check-within (candidate (list 20 20 20 20 20)) 2 0.001)
    (check-within (candidate (list 12 7 7)) -1 0.001)
    (check-within (candidate (list 10 7 9 9 10 9 9 10 10 9 10 9 10 10)) -1 0.001)
    (check-within (candidate (list 9 9 9 8 9 9 9 9 2 9 9 9 9 9)) -1 0.001)
    (check-within (candidate (list 5 5 18 1 5 5)) -1 0.001)
    (check-within (candidate (list 13 13 16 4 16 13 2 16 16 16 2 16 6 16 13 18 9)) -1 0.001)
    (check-within (candidate (list 8 8 8 8 8 8 8 8 8 7 8 8 8 8 8 7 8)) 6 0.001)
    (check-within (candidate (list 20 20 19 19 20 19 20 20 20 19 20 20 20 20 20 20 20 20 20)) 7 0.001)
    (check-within (candidate (list 4 4 20 20 4 20 1 4 4 4 4 4 4 4 20 4 4)) -1 0.001)
    (check-within (candidate (list 16 17 17 8 17 17 16 8 17 16 17)) 4 0.001)
    (check-within (candidate (list 10 10 10 9 10 10 10 9 10 18 10 4 20 2 10 10)) -1 0.001)
    (check-within (candidate (list 11 20 20 11 11 20 14 20 11 11 20 1)) -1 0.001)
    (check-within (candidate (list 14 14 14 14 15 20 15)) -1 0.001)
    (check-within (candidate (list 17 7 17 5 17 17 17 7 17 17 17 17 5 17 17 7 5)) 6 0.001)
    (check-within (candidate (list 4 4 4 4 4 4 4 4 4)) 3 0.001)
    (check-within (candidate (list 17 17)) 1 0.001)
    (check-within (candidate (list 15 2 15 2 8 15 15 15 15 15 15 8 2)) 5 0.001)
    (check-within (candidate (list 1 12 12 1 1 1 1 12 1)) 3 0.001)
    (check-within (candidate (list 12 4 9 10 17 12 5 17 4 12 12 12 4 10 4)) -1 0.001)
    (check-within (candidate (list 7 7 7 7 7 7 7 16 7 7 7 16 7 16 7 16 16 16 16 7)) 8 0.001)
    (check-within (candidate (list 20 20 20 20 19)) -1 0.001)
    (check-within (candidate (list 13 13 13 13 13 13 13 13)) 3 0.001)
    (check-within (candidate (list 15 12 18 18 15 15 15 12 12 12 12 12 12 15 18)) 6 0.001)
    (check-within (candidate (list 14 14 14 1)) -1 0.001)
    (check-within (candidate (list 2 2 2 2 2 2 8 2 8 2 2 2 2)) 5 0.001)
    (check-within (candidate (list 10 16 6 6 10 6)) -1 0.001)
    (check-within (candidate (list 18 17 3 18 6 13 3 6 14 6 15 3)) -1 0.001)
    (check-within (candidate (list 15 15)) 1 0.001)
    (check-within (candidate (list 9 9 9 9 9 9 9 9 9 9 9)) 4 0.001)
    (check-within (candidate (list 11 4 4 18 11 12 18 18 12 4 4 12 4)) 5 0.001)
    (check-within (candidate (list 2 5 20 20 5 20 20 16 20 20 20 20 20 20 3 20 20 20)) -1 0.001)
    (check-within (candidate (list 12 13 13 13 12 13 13 13 13 13 11 13 13 13)) -1 0.001)
    (check-within (candidate (list 1 1 1 1)) 2 0.001)
    (check-within (candidate (list 10 10 10 10 3 10 10 3 10)) 4 0.001)
    (check-within (candidate (list 7 14 7 7 2 2 7)) -1 0.001)
    (check-within (candidate (list 1 10 1 10 1)) 2 0.001)
    (check-within (candidate (list 11 11 11 11 11 11 11 11 11 11)) 4 0.001)
    (check-within (candidate (list 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18 18)) 7 0.001)
    (check-within (candidate (list 13 13 13 13 13 13 13)) 3 0.001)
    (check-within (candidate (list 19 19 19 19 18 19 15 7 19 19 15 5)) -1 0.001)
    (check-within (candidate (list 2 12 12 12 17 12 12 12 12 12 12 12 12 12 12)) -1 0.001)
    (check-within (candidate (list 19 16 19 19 16 16 16 16)) 3 0.001)
    (check-within (candidate (list 15 15 15 15 15 15 11 13 15 15 11 15 13 15 11 13)) 6 0.001)
    (check-within (candidate (list 15 16 16 15 16)) 2 0.001)
    (check-within (candidate (list 9 7 14 9 14 7 7 9 9 9 9 9 9 14 14)) 6 0.001)
    (check-within (candidate (list 12 16 5 5 7 10 2 16 12 7 2 12 5 16 2 11)) -1 0.001)
    (check-within (candidate (list 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11)) 6 0.001)
    (check-within (candidate (list 18 13 13 18 18 13 13 18 13 13)) 4 0.001)
    (check-within (candidate (list 4 4 8 10 8 10 19 19 19 19 8 8 19 4)) 6 0.001)
    (check-within (candidate (list 1 18 14 16 14)) -1 0.001)
    (check-within (candidate (list 7 7 7 7 3 7 7 3 7 7)) 4 0.001)
    (check-within (candidate (list 13 13)) 1 0.001)
    (check-within (candidate (list 6 11 6 8 6 13 17 14)) -1 0.001)
    (check-within (candidate (list 10 2 2 10)) 2 0.001)
    (check-within (candidate (list 19 17 17 17 17 17 17 17 19 19 19 17 19 17)) 5 0.001)
    (check-within (candidate (list 4 16 12 7 16 16 16)) -1 0.001)
    (check-within (candidate (list 11 11 11 11 11 11 11 11 11 11 11 11 11 11 11)) 5 0.001)
    (check-within (candidate (list 6 6 6 13 6 18 13 18 5 18 12 3 12 12 18 6 18 3 18 6)) -1 0.001)
    (check-within (candidate (list 4 4 3 3 4 4)) 3 0.001)
    (check-within (candidate (list 11 11 11 11 9 11 9 9 11 11 9 9 11 11 9)) 5 0.001)
    (check-within (candidate (list 16 16)) 1 0.001)
    (check-within (candidate (list 4 4 4 4 4 4 4 4 4 4 4 4 4 4)) 5 0.001)
    (check-within (candidate (list 17 16 16 17 16 16 16)) 3 0.001)
    (check-within (candidate (list 10 18 10 10)) -1 0.001)
    (check-within (candidate (list 8 8)) 1 0.001)
    (check-within (candidate (list 8 6 6 6 8 8 6 8 8 6 8 6 8 8 6 6 6 8)) 6 0.001)
    (check-within (candidate (list 15 14 20 15 20 14 14 14 20 14 20 20)) 5 0.001)
    (check-within (candidate (list 19 3 3 3 3 3 3 15 17 3 3 18 10 17 17 15 17 3 3)) -1 0.001)
    (check-within (candidate (list 1 1 16 2 16 1 2 2 2 2 2 1 2 2 2 16)) 6 0.001)
    (check-within (candidate (list 1 1 4 4 4)) 2 0.001)
    (check-within (candidate (list 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)) 7 0.001)
    (check-within (candidate (list 16 18 18 20)) -1 0.001)
    (check-within (candidate (list 2 2 2 20 15 2 20 15 2 15)) 4 0.001)
    (check-within (candidate (list 16 16 16 16 16)) 2 0.001)
    (check-within (candidate (list 1 14 14 14 14 1 14 14 1 14 14 14 14 1 14 14 1 14)) 7 0.001)
))

(test-humaneval)