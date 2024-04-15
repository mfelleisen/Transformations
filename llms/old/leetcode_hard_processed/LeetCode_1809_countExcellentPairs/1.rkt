#lang racket

;; Function to count the number of set bits in an integer
(define (count-set-bits n)
  (if (= n 0)
      0
      (+ 1 (count-set-bits (bitwise-and n (- n 1))))))

;; Function to calculate the number of excellent pairs
;; You are given a 0-indexed positive integer list nums and a positive integer k.
;; A pair of numbers (num1, num2) is called excellent if the following conditions are satisfied:
;; Both the numbers num1 and num2 exist in the list nums.
;; The sum of the number of set bits in num1 OR num2 and num1 AND num2 is greater than or equal to k, where OR is the bitwise OR operation and AND is the bitwise AND operation.
;; Return the number of distinct excellent pairs.
;; Two pairs (a, b) and (c, d) are considered distinct if either a != c or b != d. For example, (1, 2) and (2, 1) are distinct.
;; Note that a pair (num1, num2) such that num1 == num2 can also be excellent if you have at least one occurrence of num1 in the list.
;; Example 1:
;; Input: nums = [1,2,3,1], k = 3
;; Output: 5
;; Explanation: The excellent pairs are the following:
;; - (3, 3). (3 AND 3) and (3 OR 3) are both equal to (11) in binary. The total number of set bits is 2 + 2 = 4, which is greater than or equal to k = 3.
;; - (2, 3) and (3, 2). (2 AND 3) is equal to (10) in binary, and (2 OR 3) is equal to (11) in binary. The total number of set bits is 1 + 2 = 3.
;; - (1, 3) and (3, 1). (1 AND 3) is equal to (01) in binary, and (1 OR 3) is equal to (11) in binary. The total number of set bits is 1 + 2 = 3.
;; So the number of excellent pairs is 5.
;; Example 2:
;; Input: nums = [5,1,1], k = 10
;; Output: 0
;; Explanation: There are no excellent pairs for this list.
;; Constraints:
;; 1 <= nums.length <= 105
;; 1 <= nums[i] <= 109
;; 1 <= k <= 60
(define (countExcellentPairs nums k)
  (let ([freqs (make-hash)]) ; Create a hash to store frequencies of nums
    ;; Fill the hash with frequencies
    (for ([num nums])
      (hash-update! freqs num add1 0))
    (let ([count 0]) ; Initialize count of excellent pairs
      ;; Iterate over all unique pairs (including a number with itself)
      (for* ([num1 (hash-keys freqs)]
             [num2 (hash-keys freqs)]
             #:when (>= (+ (count-set-bits (bitwise-ior num1 num2))
                           (count-set-bits (bitwise-and num1 num2)))
                        k))
        ;; Update count based on the frequency of the numbers
        (set! count (+ count 
                       (if (= num1 num2)
                           ;; If the numbers are the same, calculate combinations
                           (* (hash-ref freqs num1) (- (hash-ref freqs num1) 1) 1/2)
                           ;; If different, multiply their frequencies
                           (* (hash-ref freqs num1) (hash-ref freqs num2))))))
      count))) ; Return the final count
(require rackunit)

(define (test-humaneval) 

  (let (( candidate countExcellentPairs))
    (check-within (candidate (list 1 1 1 2 2 2) 3) 0 0.001)
    (check-within (candidate (list 3 3) 5) 0 0.001)
    (check-within (candidate (list 1 2 3) 3) 4 0.001)
    (check-within (candidate (list 3 3 3 1) 4) 3 0.001)
    (check-within (candidate (list 1 2 3 1) 6) 0 0.001)
    (check-within (candidate (list 1 1 1 1) 4) 0 0.001)
    (check-within (candidate (list 3 3 3 1) 5) 0 0.001)
    (check-within (candidate (list 3 3 3 3) 3) 6 0.001)
    (check-within (candidate (list 1 1 1 1 1) 5) 0 0.001)
    (check-within (candidate (list 1 2 3 4 5) 4) 2 0.001)
    (check-within (candidate (list 1 1 1 2 2 2) 5) 0 0.001)
    (check-within (candidate (list 3 4 5 6) 6) 0 0.001)
    (check-within (candidate (list 1 2 3 4 5) 5) 0 0.001)
    (check-within (candidate (list 3 3 3 3 3) 6) 0 0.001)
    (check-within (candidate (list 1 2 3 4 5) 1) 20 0.001)
    (check-within (candidate (list 1 2 3 4 5 6) 5) 0 0.001)
    (check-within (candidate (list 1 1 1 2) 5) 0 0.001)
    (check-within (candidate (list 1 2 3 4 5) 2) 20 0.001)
    (check-within (candidate (list 2 3 4 5 6) 5) 0 0.001)
    (check-within (candidate (list 2 2 2 2) 2) 6 0.001)
    (check-within (candidate (list 2 2 2 2) 3) 0 0.001)
    (check-within (candidate (list 1 1 1 1) 1) 6 0.001)
    (check-within (candidate (list 1 2 3 4 5 6) 6) 0 0.001)
    (check-within (candidate (list 2 3 4 5 6) 2) 20 0.001)
    (check-within (candidate (list 1 2 3) 4) 0 0.001)
    (check-within (candidate (list 2 2 3 3) 5) 0 0.001)
    (check-within (candidate (list 5 1 1) 10) 0 0.001)
))

(test-humaneval)