#lang racket

;; Let's say a positive integer is a super-palindrome if it is a palindrome, and it is also the square of a palindrome.
;; Given two positive integers left and right represented as strings, return the number of super-palindromes integers in the inclusive range [left, right].
;; Example 1:
;; Input: left = "4", right = "1000"
;; Output: 4
;; Explanation: 4, 9, 121, and 484 are superpalindromes.
;; Note that 676 is not a superpalindrome: 26 * 26 = 676, but 26 is not a palindrome.
;; Example 2:
;; Input: left = "1", right = "2"
;; Output: 1
;; Constraints:
;; 1 <= left.length, right.length <= 18
;; left and right consist of only digits.
;; left and right cannot have leading zeros.
;; left and right represent integers in the range [1, 1018 - 1].
;; left is less than or equal to right.
(define (superpalindromesInRange left right)
  ;; Function to check if a number is a palindrome.
  (define (isPalindrome num)
    (let* ((str-num (number->string num))  ; Convert the number to a string.
           (rev-str-num (list->string (reverse (string->list str-num)))))  ; Reverse the string.
      (string=? str-num rev-str-num)))  ; Check if the original and reversed strings are equal.

  ;; Convert the input strings to integers for comparison.
  (define l (string->number left))
  (define r (string->number right))
  
  ;; Initialize a counter for super-palindromes.
  (define cnt 0)
  
  ;; Use a loop to check all numbers that could be the square root of a super-palindrome.
  (let loop ((base 1))
    (when (<= (* base base) r)  ; Continue looping as long as base squared is less than or equal to r.
      (when (and (isPalindrome base)  ; Check if base is a palindrome
                 (isPalindrome (* base base))  ; and its square is a palindrome
                 (>= (* base base) l))  ; and within the specified range.
        (set! cnt (+ cnt 1)))  ; Increment the counter if all conditions are met.
      (loop (+ base 1))))  ; Increment base and continue looping.
  cnt)  ; Return the count of super-palindromes found.

;; Example usage:
;; (superpalindromesInRange "4" "1000")  ; Should return 4
;; (superpalindromesInRange "1" "2")     ; Should return 1
(require rackunit)

(define (test-humaneval) 

  (let (( candidate superpalindromesInRange))
    (check-within (candidate "500000000000" "1000000000000") 0 0.001)
    (check-within (candidate "1000000000" "5000000000") 0 0.001)
    (check-within (candidate "50000000000" "100000000000") 0 0.001)
    (check-within (candidate "1" "1000") 5 0.001)
    (check-within (candidate "1" "1") 1 0.001)
    (check-within (candidate "500" "1000") 0 0.001)
    (check-within (candidate "200" "500") 1 0.001)
    (check-within (candidate "100000000000" "500000000000") 0 0.001)
    (check-within (candidate "10000000" "100000000") 0 0.001)
    (check-within (candidate "5000000000" "10000000000") 0 0.001)
))

(test-humaneval)