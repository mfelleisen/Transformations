#lang racket

;; You are given an array nums consisting of positive integers.
;; Return the total frequencies of elements in numssuch that those elements all have the maximum frequency.
;; The frequency of an element is the number of occurrences of that element in the array.
;; Example 1:
;; Input: nums = [1,2,2,3,1,4]
;; Output: 4
;; Explanation: The elements 1 and 2 have a frequency of 2 which is the maximum frequency in the array.
;; So the number of elements in the array with maximum frequency is 4.
;; Example 2:
;; Input: nums = [1,2,3,4,5]
;; Output: 5
;; Explanation: All elements of the array have a frequency of 1 which is the maximum.
;; So the number of elements in the array with maximum frequency is 5.
;; Constraints:
;; 1 <= nums.length <= 100
;; 1 <= nums[i] <= 100
(define (maxFrequencyElements nums)
  ;; Compute the frequency of each element using `for/fold` to accumulate counts in an immutable hash
  (define freqs
    (for/fold ([freqs (make-immutable-hash)])
              ([num (in-list nums)])
      (hash-update freqs num (lambda (count) (add1 (or count 0))) 0)))
  
  ;; Find the maximum frequency by taking the maximum of the hash values
  (define max-freq (apply max (hash-values freqs)))
  
  ;; Calculate the total number of elements that have the maximum frequency
  (for/sum ([freq (in-hash-values freqs)])
    (if (= freq max-freq) freq 0)))

;; Example usage:
;; (maxFrequencyElements '(1 2 2 3 1 4)) ; Output: 4
;; (maxFrequencyElements '(1 2 3 4 5))   ; Output: 5

(require rackunit)


(define (test-humaneval) 

  (let (( candidate maxFrequencyElements))
    (check-within (candidate (list 1 2 2 3 1 4)) 4 0.001)
    (check-within (candidate (list 1 2 3 4 5)) 5 0.001)
    (check-within (candidate (list 15)) 1 0.001)
    (check-within (candidate (list 10 12 11 9 6 19 11)) 2 0.001)
    (check-within (candidate (list 2 12 17 18 11)) 5 0.001)
    (check-within (candidate (list 19 19 19 20 19 8 19)) 5 0.001)
    (check-within (candidate (list 1 1 1 1)) 4 0.001)
    (check-within (candidate (list 10 1 12 10 10 19 10)) 4 0.001)
    (check-within (candidate (list 1 1 1 20 6 1)) 4 0.001)
    (check-within (candidate (list 17 17)) 2 0.001)
    (check-within (candidate (list 6 13 15 15 11 6 7 12 4 11)) 6 0.001)
    (check-within (candidate (list 1 2)) 2 0.001)
    (check-within (candidate (list 14 14 17)) 2 0.001)
    (check-within (candidate (list 17 17 2 12 20 17 12)) 3 0.001)
    (check-within (candidate (list 3 9 11 11 20)) 2 0.001)
    (check-within (candidate (list 8 15 8 11 8 13 12 11 8)) 4 0.001)
    (check-within (candidate (list 17 8 17 19 17 13 17 17 17 5)) 6 0.001)
    (check-within (candidate (list 11)) 1 0.001)
    (check-within (candidate (list 5)) 1 0.001)
    (check-within (candidate (list 4 4 10)) 2 0.001)
    (check-within (candidate (list 15 13 2 16 2 5 1 18 8 16)) 4 0.001)
    (check-within (candidate (list 1 17 12 7 17 3)) 2 0.001)
    (check-within (candidate (list 8 2 8 6 1 1)) 4 0.001)
    (check-within (candidate (list 3 9 7 9)) 2 0.001)
    (check-within (candidate (list 20 20 20 5 12 20 9 16)) 4 0.001)
    (check-within (candidate (list 2 14 3 8 16 4 4 3)) 4 0.001)
    (check-within (candidate (list 6 12 3 3 11 2)) 2 0.001)
    (check-within (candidate (list 5 2 13 19 15 20)) 6 0.001)
    (check-within (candidate (list 2 13 13)) 2 0.001)
    (check-within (candidate (list 4 5)) 2 0.001)
    (check-within (candidate (list 20 20 15 20 20 20)) 5 0.001)
    (check-within (candidate (list 16 16 16 16 1 10 16 9)) 5 0.001)
    (check-within (candidate (list 5 3 5 8 5 3 5 15)) 4 0.001)
    (check-within (candidate (list 17)) 1 0.001)
    (check-within (candidate (list 2 2 3 3 9)) 4 0.001)
    (check-within (candidate (list 5 11 4 2)) 4 0.001)
    (check-within (candidate (list 13 13 7)) 2 0.001)
    (check-within (candidate (list 2 15 10 10 10 4 13)) 3 0.001)
    (check-within (candidate (list 3 7 1)) 3 0.001)
    (check-within (candidate (list 19 6 19 19 19 19 19)) 6 0.001)
    (check-within (candidate (list 15 3 12 4 9 14 10)) 7 0.001)
    (check-within (candidate (list 1 19 12 1 12 12 1 6)) 6 0.001)
    (check-within (candidate (list 17 7 3 3 6 5 6 2)) 4 0.001)
    (check-within (candidate (list 12 4 2 9 17 14 1 12 6)) 2 0.001)
    (check-within (candidate (list 16 11)) 2 0.001)
    (check-within (candidate (list 11 11 11 11 10 11 3 11 11)) 7 0.001)
    (check-within (candidate (list 16 4 20 10 12)) 5 0.001)
    (check-within (candidate (list 3 11 3 11)) 4 0.001)
    (check-within (candidate (list 13 9 13 13 13 13 2 13)) 6 0.001)
    (check-within (candidate (list 2 8 9 4 3)) 5 0.001)
    (check-within (candidate (list 19 6 9 12 12)) 2 0.001)
    (check-within (candidate (list 20)) 1 0.001)
    (check-within (candidate (list 1 11)) 2 0.001)
    (check-within (candidate (list 6 4 7 19 20 10 13 14)) 8 0.001)
    (check-within (candidate (list 16 8 5)) 3 0.001)
    (check-within (candidate (list 15 15 4 7 15 15 15 15 15 7)) 7 0.001)
    (check-within (candidate (list 5 20)) 2 0.001)
    (check-within (candidate (list 13)) 1 0.001)
    (check-within (candidate (list 7 15 13 18 3 11 13 7 1 13)) 3 0.001)
    (check-within (candidate (list 17 5 17 5 5)) 3 0.001)
    (check-within (candidate (list 4 5 3 5)) 2 0.001)
    (check-within (candidate (list 11 2)) 2 0.001)
    (check-within (candidate (list 1 17 17 20 2 2)) 4 0.001)
    (check-within (candidate (list 2 5 2 2)) 3 0.001)
    (check-within (candidate (list 1 1 1 3 8 1)) 4 0.001)
    (check-within (candidate (list 1 19 19 5 14 13 1 20 6)) 4 0.001)
    (check-within (candidate (list 19 12 8 20 3 1 12 17)) 2 0.001)
    (check-within (candidate (list 7 15 1 1 6 3)) 2 0.001)
    (check-within (candidate (list 8 8 8 3 8 8 3)) 5 0.001)
    (check-within (candidate (list 5 1 2 2 2 1 1)) 6 0.001)
    (check-within (candidate (list 12 13 6)) 3 0.001)
    (check-within (candidate (list 18 12 8 2 16 19)) 6 0.001)
    (check-within (candidate (list 15 10 2 18 11 14 9)) 7 0.001)
    (check-within (candidate (list 19 17 9 13 1 13)) 2 0.001)
    (check-within (candidate (list 4 12 15 1 4 4 2)) 3 0.001)
    (check-within (candidate (list 16 16 16 8)) 3 0.001)
    (check-within (candidate (list 2)) 1 0.001)
    (check-within (candidate (list 13 15 1)) 3 0.001)
    (check-within (candidate (list 10 10 5 16 17 6 18)) 2 0.001)
    (check-within (candidate (list 3 2 14 2 18 7)) 2 0.001)
    (check-within (candidate (list 16 16 3)) 2 0.001)
    (check-within (candidate (list 1 8 10 11 8 15)) 2 0.001)
    (check-within (candidate (list 8 19 2 7 5 6 3 4)) 8 0.001)
    (check-within (candidate (list 9)) 1 0.001)
    (check-within (candidate (list 13 6 13 10)) 2 0.001)
    (check-within (candidate (list 14 13 14 4 4 14)) 3 0.001)
    (check-within (candidate (list 9 9 1 9 9 1)) 4 0.001)
    (check-within (candidate (list 14 4 11 14 14 4 4)) 6 0.001)
    (check-within (candidate (list 4 20 20 4 1)) 4 0.001)
    (check-within (candidate (list 5 11 8 3 11 11 11)) 4 0.001)
    (check-within (candidate (list 3 2 18 5)) 4 0.001)
    (check-within (candidate (list 3 8 20 7 16 20 18 13)) 2 0.001)
    (check-within (candidate (list 19 9 16 4 10 3 18)) 7 0.001)
    (check-within (candidate (list 11 2 2 3 19 3 11 2 14 1)) 3 0.001)
    (check-within (candidate (list 19 14 11 7 19 1 11 2 16)) 4 0.001)
    (check-within (candidate (list 18 15 3 2 8 12 19 14 12)) 2 0.001)
    (check-within (candidate (list 5 6 11 9 5 5 5)) 4 0.001)
    (check-within (candidate (list 8 4 4 12 8 1)) 4 0.001)
    (check-within (candidate (list 9 1 9 9 3)) 3 0.001)
    (check-within (candidate (list 18)) 1 0.001)
))

(test-humaneval)