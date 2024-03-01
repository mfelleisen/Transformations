#lang racket

;; Given a string s, return the number of distinct non-empty subsequences of s. Since the answer may be very large, return it modulo 109 + 7.
;; A subsequence of a string is a new string that is formed from the original string by deleting some (can be none) of the characters without disturbing the relative positions of the remaining characters. (i.e., "ace" is a subsequence of "abcde" while "aec" is not.
;; Example 1:
;; Input: s = "abc"
;; Output: 7
;; Explanation: The 7 distinct subsequences are "a", "b", "c", "ab", "ac", "bc", and "abc".
;; Example 2:
;; Input: s = "aba"
;; Output: 6
;; Explanation: The 6 distinct subsequences are "a", "b", "ab", "aa", "ba", and "aba".
;; Example 3:
;; Input: s = "aaa"
;; Output: 3
;; Explanation: The 3 distinct subsequences are "a", "aa" and "aaa".
;; Constraints:
;; 1 <= s.length <= 2000
;; s consists of lowercase English letters.
(define (distinctSubseqII s)
  (let* ([mod (+ 1 (expt 10 9))] ; The modulus 10^9 + 7
         [n (string-length s)] ; Length of the input string
         [dp (make-vector (+ n 1) 0)] ; Dynamic programming array
         [last (make-vector 26 -1)]) ; Last occurrence of each letter
    (vector-set! dp 0 1) ; Base case: empty string has one subsequence: itself
    (for ([i (in-range 1 (+ n 1))]) ; Iterate through the string
      (let* ([char (string-ref s (- i 1))] ; Current character
             [char-index (- (char->integer char) (char->integer #\a))]) ; Index in 'last' array
        (vector-set! dp i (modulo (* 2 (vector-ref dp (- i 1))) mod)) ; Update dp[i]
        (when (not (= (vector-ref last char-index) -1)) ; If the character has occurred before
          (vector-set! dp i (modulo (- (vector-ref dp i) (vector-ref dp (vector-ref last char-index))) mod))) ; Adjust dp[i] accordingly
        (vector-set! last char-index (- i 1)))) ; Update last occurrence of char
    (modulo (- (vector-ref dp n) 1) mod))) ; Subtract 1 to exclude the empty subsequence, then return

;; Example usage:
(distinctSubseqII "abc") ; Should return 7
(distinctSubseqII "aba") ; Should return 6
(distinctSubseqII "aaa") ; Should return 3
(require rackunit)

(define (test-humaneval) 

  (let (( candidate distinctSubseqII))
    (check-within (candidate "ab") 3 0.001)
    (check-within (candidate "abcdefghijklm") 8191 0.001)
    (check-within (candidate "abcd") 15 0.001)
    (check-within (candidate "abcdefghi") 511 0.001)
    (check-within (candidate "aaa") 3 0.001)
    (check-within (candidate "abcde") 31 0.001)
    (check-within (candidate "abc") 7 0.001)
    (check-within (candidate "aaa") 3 0.001)
    (check-within (candidate "ba") 3 0.001)
    (check-within (candidate "aba") 6 0.001)
    (check-within (candidate "abcdefg") 127 0.001)
    (check-within (candidate "abcdefghijklmnopqrstuvwx") 16777215 0.001)
    (check-within (candidate "abcdefghij") 1023 0.001)
))

(test-humaneval)