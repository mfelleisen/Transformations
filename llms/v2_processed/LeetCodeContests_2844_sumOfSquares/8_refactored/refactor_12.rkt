#lang racket

;; You are given a 1-indexed integer array nums of length n.
;; An element nums[i] of nums is called special if i divides n, i.e. n % i == 0.
;; Return the sum of the squares of all special elements of nums.
;; Example 1:
;; Input: nums = [1,2,3,4]
;; Output: 21
;; Explanation: There are exactly 3 special elements in nums: nums[1] since 1 divides 4, nums[2] since 2 divides 4, and nums[4] since 4 divides 4.
;; Hence, the sum of the squares of all special elements of nums is nums[1] * nums[1] + nums[2] * nums[2] + nums[4] * nums[4] = 1 * 1 + 2 * 2 + 4 * 4 = 21.
;; Example 2:
;; Input: nums = [2,7,1,19,18,3]
;; Output: 63
;; Explanation: There are exactly 4 special elements in nums: nums[1] since 1 divides 6, nums[2] since 2 divides 6, nums[3] since 3 divides 6, and nums[6] since 6 divides 6.
;; Hence, the sum of the squares of all special elements of nums is nums[1] * nums[1] + nums[2] * nums[2] + nums[3] * nums[3] + nums[6] * nums[6] = 2 * 2 + 7 * 7 + 1 * 1 + 3 * 3 = 63.
;; Constraints:
;;  * 1 <= nums.length == n <= 50
;;  * 1 <= nums[i] <= 50
(define (sumOfSquares nums)
  (define n (length nums))
  (for/fold ([sum 0]) ([i (in-range 1 (add1 n))])
    (if (zero? (remainder n i))
        (+ sum (sqr (list-ref nums (sub1 i))))
        sum)))

;; Test cases
(sumOfSquares '(1 2 3 4))  ;; Output: 21
(sumOfSquares '(2 7 1 19 18 3))  ;; Output: 63

(require rackunit)


(define (test-humaneval) 

  (let (( candidate sumOfSquares))
    (check-within (candidate (list 1 2 3 4)) 21 0.001)
    (check-within (candidate (list 2 7 1 19 18 3)) 63 0.001)
    (check-within (candidate (list 1)) 1 0.001)
    (check-within (candidate (list 2)) 4 0.001)
    (check-within (candidate (list 3)) 9 0.001)
    (check-within (candidate (list 4)) 16 0.001)
    (check-within (candidate (list 5)) 25 0.001)
    (check-within (candidate (list 6)) 36 0.001)
    (check-within (candidate (list 7)) 49 0.001)
    (check-within (candidate (list 8)) 64 0.001)
    (check-within (candidate (list 9)) 81 0.001)
    (check-within (candidate (list 10)) 100 0.001)
    (check-within (candidate (list 11)) 121 0.001)
    (check-within (candidate (list 12)) 144 0.001)
    (check-within (candidate (list 13)) 169 0.001)
    (check-within (candidate (list 14)) 196 0.001)
    (check-within (candidate (list 15)) 225 0.001)
    (check-within (candidate (list 16)) 256 0.001)
    (check-within (candidate (list 17)) 289 0.001)
    (check-within (candidate (list 18)) 324 0.001)
    (check-within (candidate (list 19)) 361 0.001)
    (check-within (candidate (list 20)) 400 0.001)
    (check-within (candidate (list 21)) 441 0.001)
    (check-within (candidate (list 22)) 484 0.001)
    (check-within (candidate (list 23)) 529 0.001)
    (check-within (candidate (list 24)) 576 0.001)
    (check-within (candidate (list 25)) 625 0.001)
    (check-within (candidate (list 26)) 676 0.001)
    (check-within (candidate (list 27)) 729 0.001)
    (check-within (candidate (list 28)) 784 0.001)
    (check-within (candidate (list 29)) 841 0.001)
    (check-within (candidate (list 30)) 900 0.001)
    (check-within (candidate (list 31)) 961 0.001)
    (check-within (candidate (list 32)) 1024 0.001)
    (check-within (candidate (list 33)) 1089 0.001)
    (check-within (candidate (list 34)) 1156 0.001)
    (check-within (candidate (list 35)) 1225 0.001)
    (check-within (candidate (list 36)) 1296 0.001)
    (check-within (candidate (list 37)) 1369 0.001)
    (check-within (candidate (list 38)) 1444 0.001)
    (check-within (candidate (list 39)) 1521 0.001)
    (check-within (candidate (list 40)) 1600 0.001)
    (check-within (candidate (list 41)) 1681 0.001)
    (check-within (candidate (list 42)) 1764 0.001)
    (check-within (candidate (list 43)) 1849 0.001)
    (check-within (candidate (list 44)) 1936 0.001)
    (check-within (candidate (list 45)) 2025 0.001)
    (check-within (candidate (list 46)) 2116 0.001)
    (check-within (candidate (list 47)) 2209 0.001)
    (check-within (candidate (list 48)) 2304 0.001)
    (check-within (candidate (list 49)) 2401 0.001)
    (check-within (candidate (list 50)) 2500 0.001)
    (check-within (candidate (list 16 16)) 512 0.001)
    (check-within (candidate (list 13 36)) 1465 0.001)
    (check-within (candidate (list 40 37)) 2969 0.001)
    (check-within (candidate (list 33 42)) 2853 0.001)
    (check-within (candidate (list 46 9)) 2197 0.001)
    (check-within (candidate (list 30 14)) 1096 0.001)
    (check-within (candidate (list 5 41)) 1706 0.001)
    (check-within (candidate (list 17 9)) 370 0.001)
    (check-within (candidate (list 29 21)) 1282 0.001)
    (check-within (candidate (list 4 38)) 1460 0.001)
    (check-within (candidate (list 14 18)) 520 0.001)
    (check-within (candidate (list 11 7)) 170 0.001)
    (check-within (candidate (list 11 36)) 1417 0.001)
    (check-within (candidate (list 18 26)) 1000 0.001)
    (check-within (candidate (list 37 46)) 3485 0.001)
    (check-within (candidate (list 13 33)) 1258 0.001)
    (check-within (candidate (list 39 1)) 1522 0.001)
    (check-within (candidate (list 37 16)) 1625 0.001)
    (check-within (candidate (list 22 34)) 1640 0.001)
    (check-within (candidate (list 4 50)) 2516 0.001)
    (check-within (candidate (list 42 40)) 3364 0.001)
    (check-within (candidate (list 7 44)) 1985 0.001)
    (check-within (candidate (list 21 27)) 1170 0.001)
    (check-within (candidate (list 49 35)) 3626 0.001)
    (check-within (candidate (list 32 20)) 1424 0.001)
    (check-within (candidate (list 30 12)) 1044 0.001)
    (check-within (candidate (list 50 42)) 4264 0.001)
    (check-within (candidate (list 3 11)) 130 0.001)
    (check-within (candidate (list 38 17)) 1733 0.001)
    (check-within (candidate (list 50 32)) 3524 0.001)
    (check-within (candidate (list 12 35)) 1369 0.001)
    (check-within (candidate (list 9 32)) 1105 0.001)
    (check-within (candidate (list 6 11)) 157 0.001)
    (check-within (candidate (list 11 39)) 1642 0.001)
    (check-within (candidate (list 18 29)) 1165 0.001)
    (check-within (candidate (list 44 29)) 2777 0.001)
    (check-within (candidate (list 50 13)) 2669 0.001)
    (check-within (candidate (list 46 46)) 4232 0.001)
    (check-within (candidate (list 27 5)) 754 0.001)
    (check-within (candidate (list 12 13)) 313 0.001)
    (check-within (candidate (list 10 46)) 2216 0.001)
    (check-within (candidate (list 37 32)) 2393 0.001)
    (check-within (candidate (list 26 33)) 1765 0.001)
    (check-within (candidate (list 44 3)) 1945 0.001)
    (check-within (candidate (list 9 16)) 337 0.001)
    (check-within (candidate (list 7 21)) 490 0.001)
    (check-within (candidate (list 23 33)) 1618 0.001)
    (check-within (candidate (list 22 5)) 509 0.001)
))

(test-humaneval)