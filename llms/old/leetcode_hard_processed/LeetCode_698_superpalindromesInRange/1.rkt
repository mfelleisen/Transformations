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
  ;; Function to check if a number is a palindrome
  (define (isPalindrome num)
    (let* ((s (number->string num))
           (rev-s (list->string (reverse (string->list s)))))
      (string=? s rev-s)))
  
  ;; Convert input strings to numbers
  (define l (string->number left))
  (define r (string->number right))
  (define cnt 0) ;; Counter for super-palindromes
  (define base 1) ;; Starting base to check for super-palindromes
  
  ;; Iterate over potential palindrome bases
  (let loop ((base base))
    (when (<= (* base base) r) ;; Check if squared base is within range
      (when (and (isPalindrome base) ;; Check if base and its square are palindromes
                 (isPalindrome (* base base)))
        (when (>= (* base base) l) ;; Check if squared base is above lower limit
          (set! cnt (+ cnt 1)))) ;; Increment counter if conditions met
      (loop (+ base 1)))) ;; Increment base and loop
  cnt) ;; Return count of super-palindromes
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