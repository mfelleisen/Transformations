#lang racket

;; You are given a 0-indexed integer array nums. You have to find the maximum sum of a pair of numbers from nums such that the maximum digit in both numbers are equal.
;; Return the maximum sum or -1 if no such pair exists.
;; Example 1:
;; Input: nums = [51,71,17,24,42]
;; Output: 88
;; Explanation:
;; For i = 1 and j = 2, nums[i] and nums[j] have equal maximum digits with a pair sum of 71 + 17 = 88.
;; For i = 3 and j = 4, nums[i] and nums[j] have equal maximum digits with a pair sum of 24 + 42 = 66.
;; It can be shown that there are no other pairs with equal maximum digits, so the answer is 88.
;; Example 2:
;; Input: nums = [1,2,3,4]
;; Output: -1
;; Explanation: No pair exists in nums with equal maximum digits.
;; Constraints:
;;  * 2 <= nums.length <= 100
;;  * 1 <= nums[i] <= 104
(define (maxSum nums)
  ;; Helper function to extract the maximum digit from a number
  (define (max-digit n)
    (apply max (map (lambda (c) (- (char->integer c) (char->integer #\0)))
                    (string->list (number->string n)))))
  
  ;; Use a hash table to store the maximum sums keyed by max digit
  (define results (make-hash))

  ;; Process each number in the list to update the hash table
  (for-each (lambda (num)
              (define digit (max-digit num))
              (define current-max (hash-ref results digit 0))
              (hash-set! results digit (max current-max num)))
            nums)

  ;; Compute the maximum sum by finding pairs with equal maximum digits
  (define max-sum -1)
  (for-each (lambda (digit)
              (define current-max (hash-ref results digit))
              (for-each (lambda (num)
                          (when (and (= (max-digit num) digit) (not (= num current-max)))
                            (set! max-sum (max max-sum (+ num current-max)))))
                        nums))
            (hash-keys results))

  max-sum)

;; Example usage:
(maxSum '(51 71 17 24 42))  ; Output: 88
(maxSum '(1 2 3 4))         ; Output: -1

(require rackunit)


(define (test-humaneval) 

  (let (( candidate maxSum))
    (check-within (candidate (list 51 71 17 24 42)) 88 0.001)
    (check-within (candidate (list 1 2 3 4)) -1 0.001)
    (check-within (candidate (list 31 25 72 79 74)) 146 0.001)
    (check-within (candidate (list 84 91 18 59 27 9 81 33 17 58)) 165 0.001)
    (check-within (candidate (list 8 75 28 35 21 13 21)) 42 0.001)
    (check-within (candidate (list 35 52 74 92 25 65 77 1 73 32)) 151 0.001)
    (check-within (candidate (list 68 8 100 84 80 14 88)) 172 0.001)
    (check-within (candidate (list 53 98 69 64 40 60 23)) 167 0.001)
    (check-within (candidate (list 21 76)) -1 0.001)
    (check-within (candidate (list 99 63 23 70 18 64)) 127 0.001)
    (check-within (candidate (list 21 21 78)) 42 0.001)
    (check-within (candidate (list 58 88 58 99 26 92)) 191 0.001)
    (check-within (candidate (list 10 24 25 20 92 73 63 51)) 76 0.001)
    (check-within (candidate (list 87 6 17 32 14 42 46 65 43 9)) 111 0.001)
    (check-within (candidate (list 96 46 85 19 29)) 125 0.001)
    (check-within (candidate (list 5 24)) -1 0.001)
    (check-within (candidate (list 26 76 24 96 82 97 97 72 35)) 194 0.001)
    (check-within (candidate (list 77 82 30 94)) -1 0.001)
    (check-within (candidate (list 76 94 51 82 3 89 52 96)) 190 0.001)
    (check-within (candidate (list 27 59 57 97 6 46 88 41 52 46)) 156 0.001)
    (check-within (candidate (list 17 2)) -1 0.001)
    (check-within (candidate (list 62 69)) -1 0.001)
    (check-within (candidate (list 63 24 1)) -1 0.001)
    (check-within (candidate (list 55 46 4 61 78 21 85 52 83 77)) 168 0.001)
    (check-within (candidate (list 21 73 2 80 99 98 89)) 197 0.001)
    (check-within (candidate (list 94 63 50 43 62 14 83 91)) 185 0.001)
    (check-within (candidate (list 66 17 17 35 46 77 7 15 38)) 112 0.001)
    (check-within (candidate (list 61 90 34 29 68 35)) 119 0.001)
    (check-within (candidate (list 18 82 78)) 160 0.001)
    (check-within (candidate (list 8 71 2 59 70 12)) 141 0.001)
    (check-within (candidate (list 55 88 59)) -1 0.001)
    (check-within (candidate (list 49 47 46 65 37 24 75 81 54 39)) 122 0.001)
    (check-within (candidate (list 73 79 48 45 57 73 51 78 67 78)) 156 0.001)
    (check-within (candidate (list 2 82 80 74 34 54 65)) 162 0.001)
    (check-within (candidate (list 9 62 85 95 36 62 21 38 16 12)) 124 0.001)
    (check-within (candidate (list 50 80 34 9 86 20 67 94 65 82)) 168 0.001)
    (check-within (candidate (list 79 74 92 84 37 19)) 171 0.001)
    (check-within (candidate (list 85 20 79)) -1 0.001)
    (check-within (candidate (list 89 55 67 84 3)) -1 0.001)
    (check-within (candidate (list 16 44 2 54 58 94)) -1 0.001)
    (check-within (candidate (list 71 14 24 13 21 14 100 18 84 37)) 108 0.001)
    (check-within (candidate (list 13 26)) -1 0.001)
    (check-within (candidate (list 82 30 53 72 56 94 72 67)) 144 0.001)
    (check-within (candidate (list 14 80 92 65 85 70)) 165 0.001)
    (check-within (candidate (list 81 39 43 31 53 43 87 19 93)) 168 0.001)
    (check-within (candidate (list 27 12 80 38 94 92 67 54 56 20)) 186 0.001)
    (check-within (candidate (list 52 32 24 6 3 89 100 3 5 3)) 57 0.001)
    (check-within (candidate (list 93 1 13 88 47 48 46 63)) 136 0.001)
    (check-within (candidate (list 3 55 40 93 97 37 31 31)) 190 0.001)
    (check-within (candidate (list 58 41 10 74 40 17)) 91 0.001)
    (check-within (candidate (list 58 33 78 53 88 1 15 44 82)) 170 0.001)
    (check-within (candidate (list 41 48 96 71 35 89 57 71)) 185 0.001)
    (check-within (candidate (list 43 4 69 29 37 50)) 98 0.001)
    (check-within (candidate (list 65 88 2)) -1 0.001)
    (check-within (candidate (list 86 42 59 44 76 6)) 86 0.001)
    (check-within (candidate (list 29 96 1 10 27 78 56 62)) 125 0.001)
    (check-within (candidate (list 100 48 6)) -1 0.001)
    (check-within (candidate (list 33 17)) -1 0.001)
    (check-within (candidate (list 8 91)) -1 0.001)
    (check-within (candidate (list 91 13 72 42 28)) -1 0.001)
    (check-within (candidate (list 5 53 35 88 77 1 66 57)) 134 0.001)
    (check-within (candidate (list 50 27 52 70 67 60 65)) 137 0.001)
    (check-within (candidate (list 84 82 31 45 94 62 45 32)) 166 0.001)
    (check-within (candidate (list 61 61 61 23 47 34 21 6 65 25)) 126 0.001)
    (check-within (candidate (list 60 21 11 99)) -1 0.001)
    (check-within (candidate (list 22 83 62 12 63 100 41 33)) 125 0.001)
    (check-within (candidate (list 92 58 85)) 143 0.001)
    (check-within (candidate (list 93 5 46 26 25 36 27 12 30)) 82 0.001)
    (check-within (candidate (list 52 30 16)) -1 0.001)
    (check-within (candidate (list 22 57 33 26 76 14 67)) 143 0.001)
    (check-within (candidate (list 90 72 37 30)) 109 0.001)
    (check-within (candidate (list 44 87 16)) -1 0.001)
    (check-within (candidate (list 19 12 52 8 3 58)) 66 0.001)
    (check-within (candidate (list 88 52 35 6 58 47 62 82 47 86)) 174 0.001)
    (check-within (candidate (list 84 1 48 76 16 10 11 60)) 132 0.001)
    (check-within (candidate (list 12 60 69 63 78 22 28)) 123 0.001)
    (check-within (candidate (list 16 28 82 77 41 22)) 110 0.001)
    (check-within (candidate (list 97 31 63 2 94 14 47)) 191 0.001)
    (check-within (candidate (list 93 100 45 74 31 41 84 90 18 21)) 183 0.001)
    (check-within (candidate (list 21 12 38 64 57 24)) 33 0.001)
    (check-within (candidate (list 33 17 99 2 58 59 72 9 62)) 158 0.001)
    (check-within (candidate (list 36 11 23 98 14 89 90 53)) 188 0.001)
    (check-within (candidate (list 57 90 5 78 84 51)) 162 0.001)
    (check-within (candidate (list 73 73 76 48 30)) 149 0.001)
    (check-within (candidate (list 2 74 37 75)) 149 0.001)
    (check-within (candidate (list 84 35 65 12)) -1 0.001)
    (check-within (candidate (list 95 46 23 81 35)) -1 0.001)
    (check-within (candidate (list 64 76 46 54 64 94 90 95)) 189 0.001)
    (check-within (candidate (list 77 52 74 84 47 89 53)) 151 0.001)
    (check-within (candidate (list 29 31 52 12 89 88 10 18)) 118 0.001)
    (check-within (candidate (list 28 57 28 41 25 89 20)) 56 0.001)
    (check-within (candidate (list 31 28)) -1 0.001)
    (check-within (candidate (list 51 1 98 73 84 11 100 100 75)) 200 0.001)
    (check-within (candidate (list 76 2 26 49 78 36 2 70 64)) 146 0.001)
    (check-within (candidate (list 34 63 21 49)) -1 0.001)
    (check-within (candidate (list 35 19 1 21 11 59 38)) 78 0.001)
    (check-within (candidate (list 1 35 74 58 56 54 75)) 149 0.001)
    (check-within (candidate (list 20 49)) -1 0.001)
    (check-within (candidate (list 97 92 13 30)) 189 0.001)
    (check-within (candidate (list 89 49 10 36 37)) 138 0.001)
))

(test-humaneval)