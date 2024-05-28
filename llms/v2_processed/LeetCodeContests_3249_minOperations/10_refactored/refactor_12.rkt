#lang racket

;; Function: minOperations
;; Description: Calculates the minimum number of bit flips required to make the XOR of all elements in
;;              the array equal to a given value k.
;;
;; @param nums - List of integers representing the array.
;; @param k    - Integer representing the target XOR value.
;; @return     - Integer representing the minimum number of bit flips required.
;; You are given a 0-indexed integer array nums and a positive integer k.
;; You can apply the following operation on the array any number of times:
;; Choose any element of the array and flip a bit in its binary representation. Flipping a bit means changing a 0 to 1 or vice versa.
;; Return the minimum number of operations required to make the bitwise XOR of all elements of the final array equal to k.
;; Note that you can flip leading zero bits in the binary representation of elements. For example, for the number (101)2 you can flip the fourth bit and obtain (1101)2.
;; Example 1:
;; Input: nums = [2,1,3,4], k = 1
;; Output: 2
;; Explanation: We can do the following operations:
;; - Choose element 2 which is 3 == (011)2, we flip the first bit and we obtain (010)2 == 2. nums becomes [2,1,2,4].
;; - Choose element 0 which is 2 == (010)2, we flip the third bit and we obtain (110)2 = 6. nums becomes [6,1,2,4].
;; The XOR of elements of the final array is (6 XOR 1 XOR 2 XOR 4) == 1 == k.
;; It can be shown that we cannot make the XOR equal to k in less than 2 operations.
;; Example 2:
;; Input: nums = [2,0,2,0], k = 0
;; Output: 0
;; Explanation: The XOR of elements of the array is (2 XOR 0 XOR 2 XOR 0) == 0 == k. So no operation is needed.
;; Constraints:
;; 1 <= nums.length <= 105
;; 0 <= nums[i] <= 106
;; 0 <= k <= 106
(define (minOperations nums k)
  ;; Calculate the XOR of all elements in nums
  (define (xor-all elements)
    (foldl bitwise-xor 0 elements))
  
  ;; Calculate the number of 1 bits in a number (Hamming weight)
  (define (count-bits n)
    (if (zero? n)
        0
        (+ (bitwise-and n 1) (count-bits (arithmetic-shift n -1)))))
  
  ;; Calculate current XOR of all elements
  (define current-xor (xor-all nums))
  
  ;; XOR with k to determine differing bits
  (define xor-with-k (bitwise-xor current-xor k))
  
  ;; Count the number of differing bits (each '1' in xor-with-k requires one flip)
  (count-bits xor-with-k))

;; Example usage
(minOperations '(2 1 3 4) 1)  ; Output: 2
(minOperations '(2 0 2 0) 0) ; Output: 0

(require rackunit)


(define (test-humaneval) 

  (let (( candidate minOperations))
    (check-within (candidate (list 2 1 3 4) 1) 2 0.001)
    (check-within (candidate (list 2 0 2 0) 0) 0 0.001)
    (check-within (candidate (list 4) 7) 2 0.001)
    (check-within (candidate (list 3 13 9 8 5 18 11 10) 13) 2 0.001)
    (check-within (candidate (list 9 7 9 14 8 6) 12) 3 0.001)
    (check-within (candidate (list 13 9 10 16 11 8 1) 17) 3 0.001)
    (check-within (candidate (list 12 14) 1) 2 0.001)
    (check-within (candidate (list 18 18) 20) 2 0.001)
    (check-within (candidate (list 3 5 1 1) 19) 3 0.001)
    (check-within (candidate (list 7 0 0 0) 8) 4 0.001)
    (check-within (candidate (list 13 15 19 18 2 9 18 11 0 7) 6) 1 0.001)
    (check-within (candidate (list 9 15 19 15 10 15 14 0 2 5) 20) 1 0.001)
    (check-within (candidate (list 19 4 19 6 3 19 14 4 16 12) 4) 0 0.001)
    (check-within (candidate (list 2 10 5 5 12 3 14 6 11 14) 3) 2 0.001)
    (check-within (candidate (list 11 20) 10) 3 0.001)
    (check-within (candidate (list 10 12 5 3 16 0) 1) 2 0.001)
    (check-within (candidate (list 0 4 4 7 14 13) 1) 2 0.001)
    (check-within (candidate (list 16 2 20 13 15 20 13) 16) 3 0.001)
    (check-within (candidate (list 19 11 11 0 16 2 2 0 9) 4) 3 0.001)
    (check-within (candidate (list 10 17 19 8 15) 19) 3 0.001)
    (check-within (candidate (list 8 17 7 18) 6) 2 0.001)
    (check-within (candidate (list 10 20) 7) 3 0.001)
    (check-within (candidate (list 11 14 5 9 19 3 1) 10) 2 0.001)
    (check-within (candidate (list 19 13 16) 4) 2 0.001)
    (check-within (candidate (list 12 18 13 2 1 5 8 5 8 6) 7) 2 0.001)
    (check-within (candidate (list 15) 9) 2 0.001)
    (check-within (candidate (list 8 5 4 5 13 18) 0) 3 0.001)
    (check-within (candidate (list 9 18) 3) 2 0.001)
    (check-within (candidate (list 6 9 15 17 16) 19) 2 0.001)
    (check-within (candidate (list 14 0 17) 2) 4 0.001)
    (check-within (candidate (list 12 1 14 13) 4) 2 0.001)
    (check-within (candidate (list 4 10 6 10 10 16) 18) 2 0.001)
    (check-within (candidate (list 2 11 6 12 2 15 4 8 11) 3) 2 0.001)
    (check-within (candidate (list 7 3 12 5 1 12 8) 11) 2 0.001)
    (check-within (candidate (list 11 14 18 14 6 18 4 16 20 5) 16) 2 0.001)
    (check-within (candidate (list 20 2 6 0 7) 20) 2 0.001)
    (check-within (candidate (list 9 18 19 16 8 11 15) 14) 3 0.001)
    (check-within (candidate (list 0 3 20 0 15 7 17 4) 3) 2 0.001)
    (check-within (candidate (list 15 6) 8) 1 0.001)
    (check-within (candidate (list 2 7 13 16 2 2) 15) 4 0.001)
    (check-within (candidate (list 2 12 11 11 2 12) 17) 2 0.001)
    (check-within (candidate (list 10 8 10) 11) 2 0.001)
    (check-within (candidate (list 1 10 2 13) 0) 1 0.001)
    (check-within (candidate (list 1 20 4 19 12 18 5 3 11 8) 14) 4 0.001)
    (check-within (candidate (list 16 12 12) 20) 1 0.001)
    (check-within (candidate (list 2 1 7 3 4 9) 6) 2 0.001)
    (check-within (candidate (list 20 0 19 14 7 0) 18) 3 0.001)
    (check-within (candidate (list 0 15 9 1 15) 11) 2 0.001)
    (check-within (candidate (list 9 11 8 20 10) 0) 2 0.001)
    (check-within (candidate (list 2 10 2 14 7 13 4 9 2) 20) 3 0.001)
    (check-within (candidate (list 7 12 8) 14) 3 0.001)
    (check-within (candidate (list 20 11 5 8 1 8 4 16) 7) 1 0.001)
    (check-within (candidate (list 8 2 19 9 8) 9) 2 0.001)
    (check-within (candidate (list 17) 8) 3 0.001)
    (check-within (candidate (list 19 6) 13) 2 0.001)
    (check-within (candidate (list 12 3 20 19) 4) 2 0.001)
    (check-within (candidate (list 4 10 18 17 20 6 4) 10) 2 0.001)
    (check-within (candidate (list 8 6 12 6 6) 4) 2 0.001)
    (check-within (candidate (list 18 12 9 18 12 12 1) 12) 1 0.001)
    (check-within (candidate (list 14 4 0 18 18 8 4 9) 17) 4 0.001)
    (check-within (candidate (list 7 16 16 6) 19) 2 0.001)
    (check-within (candidate (list 7 16 2 13 0 17 16) 18) 3 0.001)
    (check-within (candidate (list 3 17 4 2 3 9) 12) 2 0.001)
    (check-within (candidate (list 13 14 9 19 5 13) 8) 3 0.001)
    (check-within (candidate (list 4 15 10 15 11 1 3 5 18 13) 16) 3 0.001)
    (check-within (candidate (list 9 7 8) 11) 3 0.001)
    (check-within (candidate (list 7 4 6 20 9 9 6 6) 10) 4 0.001)
    (check-within (candidate (list 1 9 13 19 19 0 16 20 4) 2) 3 0.001)
    (check-within (candidate (list 20 3 9 6 5 8) 20) 1 0.001)
    (check-within (candidate (list 11 20 5 16 15 11 8) 11) 3 0.001)
    (check-within (candidate (list 12 10 16 18 17 4 2 19 17 2) 19) 0 0.001)
    (check-within (candidate (list 15 2) 10) 3 0.001)
    (check-within (candidate (list 13 3 10 2 9 13 5 11 5) 20) 4 0.001)
    (check-within (candidate (list 20 12 9 3 2 11) 13) 3 0.001)
    (check-within (candidate (list 3 19 0 18 6) 15) 3 0.001)
    (check-within (candidate (list 12 6) 18) 2 0.001)
    (check-within (candidate (list 10 11 12 6 10 1 15) 8) 3 0.001)
    (check-within (candidate (list 12 8 1 16 6 12) 2) 4 0.001)
    (check-within (candidate (list 11 5 9) 2) 2 0.001)
    (check-within (candidate (list 2 7) 6) 2 0.001)
    (check-within (candidate (list 20 1) 8) 4 0.001)
    (check-within (candidate (list 9) 16) 3 0.001)
    (check-within (candidate (list 9 5 7 11 8 18 5 1 4) 8) 2 0.001)
    (check-within (candidate (list 1 8 7 19 3 20 13 9 10) 1) 2 0.001)
    (check-within (candidate (list 19 18 6) 8) 4 0.001)
    (check-within (candidate (list 19 12 3 18 12 19 5 20) 0) 0 0.001)
    (check-within (candidate (list 6 18 12 9 20) 13) 1 0.001)
    (check-within (candidate (list 19 5 5 7 4 7 15) 7) 5 0.001)
    (check-within (candidate (list 17 7 19) 18) 4 0.001)
    (check-within (candidate (list 14 13 3 15 18 20 2 9 3) 14) 4 0.001)
    (check-within (candidate (list 16 10 3 2 3 19) 4) 4 0.001)
    (check-within (candidate (list 2 0 8) 18) 2 0.001)
    (check-within (candidate (list 19 5 5 12 20 2 10 17) 12) 4 0.001)
    (check-within (candidate (list 6 0 0 1 15 9 19 12) 6) 2 0.001)
    (check-within (candidate (list 8 16 13 8 18 9 16 16 19 11) 12) 2 0.001)
    (check-within (candidate (list 3 7 6 7 4 3 2) 2) 1 0.001)
    (check-within (candidate (list 0 2 9) 10) 1 0.001)
    (check-within (candidate (list 14 10 16 9 6 13 11 13 11 16) 20) 5 0.001)
    (check-within (candidate (list 13 19 0 12 11 10 11 2 6) 7) 4 0.001)
    (check-within (candidate (list 6 1 4 9 1 7 11 15 15 0) 8) 4 0.001)
))

(test-humaneval)