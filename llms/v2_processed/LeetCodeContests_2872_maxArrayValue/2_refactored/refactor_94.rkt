#lang racket

;; You are given a 0-indexed array nums consisting of positive integers.
;; You can do the following operation on the array any number of times:
;;  * Choose an integer i such that 0 <= i < nums.length - 1 and nums[i] <= nums[i + 1]. Replace the element nums[i + 1] with nums[i] + nums[i + 1] and delete the element nums[i] from the array.
;; Return the value of the largest element that you can possibly obtain in the final array.
;; Example 1:
;; Input: nums = [2,3,7,9,3]
;; Output: 21
;; Explanation: We can apply the following operations on the array:
;; - Choose i = 0. The resulting array will be nums = [5,7,9,3].
;; - Choose i = 1. The resulting array will be nums = [5,16,3].
;; - Choose i = 0. The resulting array will be nums = [21,3].
;; The largest element in the final array is 21. It can be shown that we cannot obtain a larger element.
;; Example 2:
;; Input: nums = [5,3,3]
;; Output: 11
;; Explanation: We can do the following operations on the array:
;; - Choose i = 1. The resulting array will be nums = [5,6].
;; - Choose i = 0. The resulting array will be nums = [11].
;; There is only one element in the final array, which is 11.
;; Constraints:
;;  * 1 <= nums.length <= 105
;;  * 1 <= nums[i] <= 106
(define (maxArrayValue nums)
  ;; This function simulates the described operations in a right-to-left folding manner,
  ;; combining elements from right to left if they meet the condition.
  (define (fold-nums lst)
    (for/fold ([combined '()])
              ([current (in-list (reverse lst))])
      ;; If the combined list is not empty and the current element is less than or equal to the first element of the combined list,
      ;; combine them by adding the current element to the first element of the combined list.
      ;; Otherwise, prepend the current element to the combined list.
      (if (and (not (null? combined)) (<= current (car combined)))
          (cons (+ current (car combined)) (cdr combined))
          (cons current combined))))
  
  ;; The largest value in the folded list is the result.
  (apply max (fold-nums nums)))

;; Example usage:
(maxArrayValue '(2 3 7 9 3))  ; Output: 21
(maxArrayValue '(5 3 3))      ; Output: 11

(require rackunit)


(define (test-humaneval) 

  (let (( candidate maxArrayValue))
    (check-within (candidate (list 2 3 7 9 3)) 21 0.001)
    (check-within (candidate (list 5 3 3)) 11 0.001)
    (check-within (candidate (list 77)) 77 0.001)
    (check-within (candidate (list 34 95 50 12 25 100 21 3 25 16 76 73 93 46 18)) 623 0.001)
    (check-within (candidate (list 40 15 35 98 77 79 24 62 53 84 97 16 30 22 49)) 781 0.001)
    (check-within (candidate (list 64 35 42 19 95 8 83 89 33 21 97 11 51 93 36 34 67 53)) 878 0.001)
    (check-within (candidate (list 65 68 55 6 79 30 81 25 61 2 28 59 63 15 35 8 10 83)) 773 0.001)
    (check-within (candidate (list 56)) 56 0.001)
    (check-within (candidate (list 100)) 100 0.001)
    (check-within (candidate (list 35 23 71 38)) 129 0.001)
    (check-within (candidate (list 56 67 18 81 95 41 39 56 63 70 56 31 84 46 28 38 27 56 13 10 58 16 85 21 63 8)) 1134 0.001)
    (check-within (candidate (list 72 72)) 144 0.001)
    (check-within (candidate (list 16 31 55)) 102 0.001)
    (check-within (candidate (list 6 65 68 7 35 19 28)) 228 0.001)
    (check-within (candidate (list 38 37 88 60 93 4 5 65 74 25 59 28 86 33 28 33 93)) 849 0.001)
    (check-within (candidate (list 29 9 3 55 25 38 88 39 38 73 47 57 40 56 4 52 1 44 88 20 18 8)) 786 0.001)
    (check-within (candidate (list 34 92 42 24 98 87 40 82 51 67 70 75 45 57 67)) 931 0.001)
    (check-within (candidate (list 31 75 44 92 13 10 3 41 47 89 5 92 17 62 65 40 43 68 30 45 85 24 40 77 80 65)) 1218 0.001)
    (check-within (candidate (list 63 58 61 58 82 48 83 24 24 61 31 16 26 50)) 685 0.001)
    (check-within (candidate (list 10 82 74 54 20 43 74 95 17 28 44 74 25 19 75 2 84 99)) 919 0.001)
    (check-within (candidate (list 91 32 21 55 44 29 82 75 66 29 77 62 55 94 49 80 12 46 80 64 88 51 2 24 11 10 86 39 16)) 1415 0.001)
    (check-within (candidate (list 18 16 56 64 82 25 16 2 19)) 236 0.001)
    (check-within (candidate (list 29 79 47 55 13 47 48 91 29 28 34 85 98 44 93 56 24 77 61)) 977 0.001)
    (check-within (candidate (list 74 8 1 57 25 62)) 227 0.001)
    (check-within (candidate (list 25 36 55 32 15 16 73 67 82 23 17 29 78 34 4 91 1 1 55 65)) 799 0.001)
    (check-within (candidate (list 6 36 69 97 86 44 27 46)) 411 0.001)
    (check-within (candidate (list 18 36 100 34 31 89 96 6 73 10 82 20 26 13 24 51 87 70 63 36)) 796 0.001)
    (check-within (candidate (list 86 57 57 56 38 97 82 48 33 55 19 21 57 85 11 11 71 94 61 41 1 78 39 45)) 1243 0.001)
    (check-within (candidate (list 33 9 11 6 68 43 76 40 91)) 377 0.001)
    (check-within (candidate (list 89 45 50 98 23 79 10 98 69 65 47 46 95)) 814 0.001)
    (check-within (candidate (list 58 95)) 153 0.001)
    (check-within (candidate (list 91 50)) 91 0.001)
    (check-within (candidate (list 99 82 49 52 5 69 65 94 94 57 46 26 28 84 42 61 19 87 71 66 1 72)) 1269 0.001)
    (check-within (candidate (list 75 75 93 44 16 27 43 71 65 90 100 97 39 100 55 15 10 7 25 23 47)) 1117 0.001)
    (check-within (candidate (list 65)) 65 0.001)
    (check-within (candidate (list 50)) 50 0.001)
    (check-within (candidate (list 56 8 10 87 83 79 33 72 32 59 75 2 46 9)) 594 0.001)
    (check-within (candidate (list 26 77 78 94 90 90 57 100 60 1 98 85 78 77 63 30 88 60 41 55)) 1348 0.001)
    (check-within (candidate (list 65 53 93 76 75 18 32 88 4)) 500 0.001)
    (check-within (candidate (list 24 89 92 48 81 49 83 4 53 39 48 10 53 51 41 23 83 8 53 91 43 58 82)) 1206 0.001)
    (check-within (candidate (list 59 17 33)) 59 0.001)
    (check-within (candidate (list 15 35 97 93 34 34 90 2 21)) 398 0.001)
    (check-within (candidate (list 87 64 21 27 41 63 28 75 64 22 30 76 77 91 84 81 99 86 1 74 46 4 7)) 1030 0.001)
    (check-within (candidate (list 89 49 59 59 2 77 55 44 51 47 100 77 30 71 47 100 13 17 12 38 26 55 89 41)) 1207 0.001)
    (check-within (candidate (list 71 4 53 51 9 92 91 86 84 58 31 39 38 49 56 27 91 17 10 56 52 78 35 76 39)) 1254 0.001)
    (check-within (candidate (list 9 46 6 42 81 7 61 88 37 15 20 67)) 479 0.001)
    (check-within (candidate (list 50 64 31 70 46 30 41 69 80 45 73 4 100 88 7 3 59)) 703 0.001)
    (check-within (candidate (list 20 41 58 61 79 7 58 42 89 39)) 455 0.001)
    (check-within (candidate (list 68 86 34 99 4 6 24 88 26 83 2 33 37 79 30 60 56 44 53 4 86 60 13 81 95 28 83 24)) 1362 0.001)
    (check-within (candidate (list 5 61 59 13 21 90 32 93 84 16 71 78 53 90 5 50 47 85 83 72 88 20 97 28 73 75 59 34 21)) 1489 0.001)
    (check-within (candidate (list 99 57 14 77 78 88 47 12 45 72 70 73 75 35 50 88 26 38 77 23 86 27 9 16)) 1230 0.001)
    (check-within (candidate (list 68 65 95 53 51 26 2 3 17 26 15 37 50 79 20 71 99 72 82 37 29 34 74 93)) 1198 0.001)
    (check-within (candidate (list 68 21 61 74 38 91 99 32 98 12 52)) 582 0.001)
    (check-within (candidate (list 98 95 15 53 31 15 9 24 59)) 399 0.001)
    (check-within (candidate (list 51 18 21 99 6 55 41 20 74 43 98 41 58 29 75 16 8 83 23 79 73 68 95 10 67)) 1174 0.001)
    (check-within (candidate (list 78 91 52 92 42 53 77 88 40 33 86 70 85 50 65 43 75 60 28 97 95 95)) 1495 0.001)
    (check-within (candidate (list 99 6)) 99 0.001)
    (check-within (candidate (list 59 50 38 100 42 42 99 7)) 430 0.001)
    (check-within (candidate (list 35 23 73 45 29 94 1 18 46 7 52 6 47 47 19 93 48 70 85 98 50 89 23)) 1075 0.001)
    (check-within (candidate (list 2 55 19 10 28 45 86 31 45 32 38 95 65 23 50 39 51 24 40 15 16)) 778 0.001)
    (check-within (candidate (list 31 59 12 90 39)) 192 0.001)
    (check-within (candidate (list 53 87 11 58 79 42 44 24 68 61)) 466 0.001)
    (check-within (candidate (list 20 45)) 65 0.001)
    (check-within (candidate (list 85 36 99 11 91 88 55 25 68 88 27 98 7 14 40 27 18 51 90 21 77 12 87 11 37 80 70)) 1343 0.001)
    (check-within (candidate (list 43 50 40 92 31 2 92)) 350 0.001)
    (check-within (candidate (list 79 90 32 30 33 18 55 96)) 433 0.001)
    (check-within (candidate (list 65 15 18 94 96 22 37 19 23 11 27 94 5 99)) 625 0.001)
    (check-within (candidate (list 74 26 11 96 49 19 25 77 47 31 87 96 19 40 95 91 48 79 33 96)) 1139 0.001)
    (check-within (candidate (list 17 90 66)) 107 0.001)
    (check-within (candidate (list 60 11 95 75 10 64 62 20)) 166 0.001)
    (check-within (candidate (list 99 6 67 44 84 29 87 13 44 12 92 53 26 47 88 44 75 33 19)) 910 0.001)
    (check-within (candidate (list 69 21 11 10 42 3 38 36 50 28 25 93 37 45 73 37 97)) 715 0.001)
    (check-within (candidate (list 32 76 65 61 84 11 94 96 17 14 79 15 62)) 629 0.001)
    (check-within (candidate (list 59 91 27 74 57 30 51 67 88 26 89 10 70 31 32 26 42)) 870 0.001)
    (check-within (candidate (list 8 73 22 37 39 1 66 59 5 20 16 68 55 50 48 6 8 46 93 76 48 14 92)) 950 0.001)
    (check-within (candidate (list 58 34 72 5 33 34 68 96 63 85 84 74 87 33 75 43 36 28 62 44 95 39 2)) 1209 0.001)
    (check-within (candidate (list 49 88 44 17 36 65 94 92 75 23 67 55 68 80 95 11 68 66 77 66 3 32 16 81 34 20 56 87 87 29)) 1652 0.001)
    (check-within (candidate (list 94 27 5 47)) 94 0.001)
    (check-within (candidate (list 83 85 59 55)) 168 0.001)
    (check-within (candidate (list 72 56 30 65 94 91 12 99 9)) 519 0.001)
    (check-within (candidate (list 9 99 20 61 57 88 50 36 21 100 62 98 94 81 96 3 98 37 88)) 1198 0.001)
    (check-within (candidate (list 45 18 13 66 54 45 64 70 94 67 26 48 84 57 81 85 35 17 20 84 78 24 63 9)) 1238 0.001)
    (check-within (candidate (list 5 38 82 83 92 97)) 397 0.001)
    (check-within (candidate (list 95 15 19 26 59 58)) 214 0.001)
    (check-within (candidate (list 94 75 16 33 2 70 56 4 64)) 414 0.001)
    (check-within (candidate (list 64 50 26 66)) 206 0.001)
    (check-within (candidate (list 3 48 11 71 57 72 83 61 59 25 36 29 11 69 75 48 44 44)) 846 0.001)
    (check-within (candidate (list 88 7 38 15 43 8 87 7 25 2 51 29 74 34 84 87 83 34 74 22 45 96 71 4 23 28 27 68 61)) 1254 0.001)
    (check-within (candidate (list 30 58 2 20 54)) 164 0.001)
    (check-within (candidate (list 17 34 71 23 88 84 35 49 89 39 33 13 87 49 48 97)) 856 0.001)
    (check-within (candidate (list 50 67 98 47 18 91 80 3 19 74 40 89 85 99 95 81 72 96 56 15 48 93 64)) 1416 0.001)
    (check-within (candidate (list 58 10 99 6 16 94 45 47 4 30 58)) 467 0.001)
    (check-within (candidate (list 92 58 90 38 37 95 47 82 6 86 99 9 91 80 73 54 45)) 830 0.001)
    (check-within (candidate (list 31 100 59 88 81 74 49 21 31 53 9 89 67 4 84 46 41)) 840 0.001)
    (check-within (candidate (list 25 3 94 55 70 23 43 8 65 34 83 60 53 62 97 55 3 10)) 775 0.001)
    (check-within (candidate (list 27 53 99 55 15 59 85 40 46 45 45 71 42 67)) 749 0.001)
    (check-within (candidate (list 8 62 12 10 79 36 59 73 76 24 45 98 72 83 61 6 19 49)) 872 0.001)
    (check-within (candidate (list 2 24 30 18 94 26 22 60 50 3 27 31)) 387 0.001)
    (check-within (candidate (list 64 17 57 72 24 88 29 2 23 82 15 69 80 93 38 47 9 10 68 89 65 16)) 976 0.001)
    (check-within (candidate (list 99 58 59 5 67 15 6 91 71 75 79 59 40 1 18 49 48 75 92 72 81 43 31 31 29 94 39)) 1388 0.001)
))

(test-humaneval)