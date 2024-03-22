#lang racket

;; Given a string s, returns the number of distinct non-empty subsequences of s.
;; Since the answer may be very large, return it modulo 10^9 + 7.
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
  (let* ([mod (+ (expt 10 9) 7)] ; Modulo value for the final answer
         [n (string-length s)] ; Length of the input string
         [dp (make-vector (+ n 1) 0)] ; DP table to store intermediate results
         [last (make-vector 26 -1)]) ; Last occurrence of each character
    ; Initialize dp[0] to 1 (empty subsequence)
    (vector-set! dp 0 1)
    ; Iterate over each character in the string
    (for ([i (in-range 1 (+ n 1))])
      (let* ([char-index (- (char->integer (string-ref s (- i 1))) (char->integer #\a))]
             [prev-occurrence (vector-ref last char-index)])
        ; Update dp[i] based on previous computations
        (vector-set! dp i (modulo (* 2 (vector-ref dp (- i 1))) mod))
        ; If the character has appeared before, subtract the dp value of its last occurrence
        (when (not (= prev-occurrence -1))
          (vector-set! dp i (modulo (- (+ (vector-ref dp i) mod) (vector-ref dp prev-occurrence)) mod)))
        ; Update the last occurrence of the current character
        (vector-set! last char-index (- i 1))))
    ; Subtract 1 to exclude the empty subsequence, then return the result modulo mod
    (modulo (- (vector-ref dp n) 1) mod)))

;; Example usage:
(distinctSubseqII "abc") ; Output: 7
(distinctSubseqII "aba") ; Output: 6
(distinctSubseqII "aaa") ; Output: 3
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