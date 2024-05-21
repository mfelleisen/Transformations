#lang racket

;; You are given an array of positive integers nums.
;; You have to check if it is possible to select two or more elements in the array such that the bitwise OR of the selected elements has at least one trailing zero in its binary representation.
;; For example, the binary representation of 5, which is "101", does not have any trailing zeros, whereas the binary representation of 4, which is "100", has two trailing zeros.
;; Return true if it is possible to select two or more elements whose bitwise OR has trailing zeros, return false otherwise.
;; Example 1:
;; Input: nums = [1,2,3,4,5]
;; Output: true
;; Explanation: If we select the elements 2 and 4, their bitwise OR is 6, which has the binary representation "110" with one trailing zero.
;; Example 2:
;; Input: nums = [2,4,8,16]
;; Output: true
;; Explanation: If we select the elements 2 and 4, their bitwise OR is 6, which has the binary representation "110" with one trailing zero.
;; Other possible ways to select elements to have trailing zeroes in the binary representation of their bitwise OR are: (2, 8), (2, 16), (4, 8), (4, 16), (8, 16), (2, 4, 8), (2, 4, 16), (2, 8, 16), (4, 8, 16), and (2, 4, 8, 16).
;; Example 3:
;; Input: nums = [1,3,5,7,9]
;; Output: false
;; Explanation: There is no possible way to select two or more elements to have trailing zeros in the binary representation of their bitwise OR.
;; Constraints:
;; 2 <= nums.length <= 100
;; 1 <= nums[i] <= 100
(define (hasTrailingZeros nums)
  ;; This function checks if it is possible to select two or more elements from the list `nums`
  ;; such that their bitwise OR has at least one trailing zero.
  (define (bitwise-or-has-trailing-zero? x y)
    ;; Helper function to determine if the bitwise OR of two numbers has a trailing zero.
    (= (bitwise-and (bitwise-ior x y) 1) 0))

  ;; Use nested loops to check each combination of two different elements from the list.
  (for/or ([i (in-range (length nums))]
           [j (in-range (add1 i) (length nums))]
           #:when (bitwise-or-has-trailing-zero? (list-ref nums i) (list-ref nums j)))
    #t))

;; Example usage:
(hasTrailingZeros '(1 2 3 4 5))  ;; Output: #t
(hasTrailingZeros '(2 4 8 16))   ;; Output: #t
(hasTrailingZeros '(1 3 5 7 9))  ;; Output: #f

(require rackunit)


(define (test-humaneval) 

  (let (( candidate hasTrailingZeros))
    (check-within (candidate (list 1 2 3 4 5)) #t 0.001)
    (check-within (candidate (list 1 3 5 7 9)) #f 0.001)
    (check-within (candidate (list 1 2)) #f 0.001)
    (check-within (candidate (list 2 4 8 16)) #t 0.001)
    (check-within (candidate (list 1 3)) #f 0.001)
    (check-within (candidate (list 1 7)) #f 0.001)
    (check-within (candidate (list 3 3)) #f 0.001)
    (check-within (candidate (list 3 4)) #f 0.001)
    (check-within (candidate (list 2 2)) #t 0.001)
    (check-within (candidate (list 4 8)) #t 0.001)
    (check-within (candidate (list 4 32)) #t 0.001)
    (check-within (candidate (list 6 2)) #t 0.001)
    (check-within (candidate (list 6 8)) #t 0.001)
    (check-within (candidate (list 3 9)) #f 0.001)
    (check-within (candidate (list 4 3)) #f 0.001)
    (check-within (candidate (list 5 6)) #f 0.001)
    (check-within (candidate (list 7 9)) #f 0.001)
    (check-within (candidate (list 8 2)) #t 0.001)
    (check-within (candidate (list 8 4)) #t 0.001)
    (check-within (candidate (list 7 10)) #f 0.001)
    (check-within (candidate (list 9 73)) #f 0.001)
    (check-within (candidate (list 8 8)) #t 0.001)
    (check-within (candidate (list 10 5)) #f 0.001)
    (check-within (candidate (list 11 17)) #f 0.001)
    (check-within (candidate (list 19 11)) #f 0.001)
    (check-within (candidate (list 19 35)) #f 0.001)
    (check-within (candidate (list 19 51)) #f 0.001)
    (check-within (candidate (list 21 61)) #f 0.001)
    (check-within (candidate (list 23 21)) #f 0.001)
    (check-within (candidate (list 25 25)) #f 0.001)
    (check-within (candidate (list 10 2)) #t 0.001)
    (check-within (candidate (list 12 8)) #t 0.001)
    (check-within (candidate (list 27 77)) #f 0.001)
    (check-within (candidate (list 16 4)) #t 0.001)
    (check-within (candidate (list 16 8)) #t 0.001)
    (check-within (candidate (list 16 16)) #t 0.001)
    (check-within (candidate (list 16 32)) #t 0.001)
    (check-within (candidate (list 29 13)) #f 0.001)
    (check-within (candidate (list 37 69)) #f 0.001)
    (check-within (candidate (list 39 53)) #f 0.001)
    (check-within (candidate (list 24 32)) #t 0.001)
    (check-within (candidate (list 32 32)) #t 0.001)
    (check-within (candidate (list 42 9)) #f 0.001)
    (check-within (candidate (list 45 24)) #f 0.001)
    (check-within (candidate (list 64 16)) #t 0.001)
    (check-within (candidate (list 49 23)) #f 0.001)
    (check-within (candidate (list 4 6 4)) #t 0.001)
    (check-within (candidate (list 8 16 4)) #t 0.001)
    (check-within (candidate (list 57 27)) #f 0.001)
    (check-within (candidate (list 8 16 16)) #t 0.001)
    (check-within (candidate (list 10 4 6)) #t 0.001)
    (check-within (candidate (list 12 8 8)) #t 0.001)
    (check-within (candidate (list 63 47)) #f 0.001)
    (check-within (candidate (list 67 69)) #f 0.001)
    (check-within (candidate (list 69 87)) #f 0.001)
    (check-within (candidate (list 16 8 4)) #t 0.001)
    (check-within (candidate (list 77 49)) #f 0.001)
    (check-within (candidate (list 89 31)) #f 0.001)
    (check-within (candidate (list 1 3 4)) #f 0.001)
    (check-within (candidate (list 1 5 3)) #f 0.001)
    (check-within (candidate (list 1 7 9)) #f 0.001)
    (check-within (candidate (list 3 69 59)) #f 0.001)
    (check-within (candidate (list 16 16 16)) #t 0.001)
    (check-within (candidate (list 6 5 5)) #f 0.001)
    (check-within (candidate (list 7 77 9)) #f 0.001)
    (check-within (candidate (list 9 77 51)) #f 0.001)
    (check-within (candidate (list 16 32 8)) #t 0.001)
    (check-within (candidate (list 16 32 16)) #t 0.001)
    (check-within (candidate (list 10 1 9)) #f 0.001)
    (check-within (candidate (list 10 7 5)) #f 0.001)
    (check-within (candidate (list 11 23 27)) #f 0.001)
    (check-within (candidate (list 15 13 63)) #f 0.001)
    (check-within (candidate (list 21 27 79)) #f 0.001)
    (check-within (candidate (list 23 23 47)) #f 0.001)
    (check-within (candidate (list 35 91 15)) #f 0.001)
    (check-within (candidate (list 32 4 16)) #t 0.001)
    (check-within (candidate (list 32 8 48)) #t 0.001)
    (check-within (candidate (list 33 40 84)) #t 0.001)
    (check-within (candidate (list 41 83 53)) #f 0.001)
    (check-within (candidate (list 64 48 6)) #t 0.001)
    (check-within (candidate (list 75 34 58)) #t 0.001)
    (check-within (candidate (list 3 8 2 3)) #t 0.001)
    (check-within (candidate (list 43 15 41)) #f 0.001)
    (check-within (candidate (list 43 65 79)) #f 0.001)
    (check-within (candidate (list 47 7 19)) #f 0.001)
    (check-within (candidate (list 4 6 1 1)) #t 0.001)
    (check-within (candidate (list 4 10 1 7)) #t 0.001)
    (check-within (candidate (list 49 73 81)) #f 0.001)
    (check-within (candidate (list 6 3 10 6)) #t 0.001)
    (check-within (candidate (list 6 4 2 8)) #t 0.001)
    (check-within (candidate (list 6 12 12 24)) #t 0.001)
    (check-within (candidate (list 7 2 4 4)) #t 0.001)
    (check-within (candidate (list 8 6 4 32)) #t 0.001)
    (check-within (candidate (list 8 12 16 2)) #t 0.001)
    (check-within (candidate (list 12 64 16 8)) #t 0.001)
    (check-within (candidate (list 16 4 8 16)) #t 0.001)
    (check-within (candidate (list 51 33 25)) #f 0.001)
    (check-within (candidate (list 16 8 64 4)) #t 0.001)
    (check-within (candidate (list 16 16 12 8)) #t 0.001)
    (check-within (candidate (list 51 43 9)) #f 0.001)
))

(test-humaneval)