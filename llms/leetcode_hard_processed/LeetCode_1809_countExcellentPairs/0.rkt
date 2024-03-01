#lang racket

(define (count-set-bits n)
  ;; Counts the number of set bits in a number
  (if (= n 0)
      0
      (+ (bitwise-and n 1) (count-set-bits (arithmetic-shift n -1)))))

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
  ;; Given a list of numbers and an integer k, returns the number of excellent pairs.
  (let* ((unique-nums (remove-duplicates nums)) ;; Remove duplicates to work with unique numbers
         (frequencies (make-hasheq)) ;; Create a hash table for frequencies
         (count 0)) ;; Initialize count of excellent pairs
    ;; Populate the frequencies hash table
    (for ([num nums])
      (hash-update! frequencies num add1 0))
    ;; Iterate over each unique pair of numbers
    (for ([num1 unique-nums])
      (for ([num2 unique-nums])
        (let* ((or (bitwise-ior num1 num2)) ;; Calculate num1 OR num2
               (and (bitwise-and num1 num2)) ;; Calculate num1 AND num2
               (set-bits (+ (count-set-bits or) (count-set-bits and)))) ;; Sum of set bits
          (when (>= set-bits k)
            ;; If the condition is met, update count based on frequencies
            (set! count (+ count
                           (if (= num1 num2)
                               ;; If both numbers are the same, choose 2 from frequency of num1
                               (* (hash-ref frequencies num1) (- (hash-ref frequencies num1) 1) 1/2)
                               ;; Else, multiply their frequencies
                               (* (hash-ref frequencies num1) (hash-ref frequencies num2)))))))))
    count))

;; Example usage:
(countExcellentPairs '(1 2 3 1) 3) ;; Output: 5
(countExcellentPairs '(5 1 1) 10) ;; Output: 0
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