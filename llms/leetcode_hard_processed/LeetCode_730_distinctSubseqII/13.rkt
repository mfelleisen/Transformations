#lang racket

;; Function to calculate the number of distinct non-empty subsequences of a string s.
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
  (let* ([mod (+ 1 (expt 10 9))] ; Define the modulus 10^9 + 7
         [n (string-length s)] ; Length of the string s
         [dp (make-vector (+ n 1) 0)] ; Initialize the dynamic programming table with zeros
         [last (make-vector 26 -1)]) ; Initialize the last occurrence array with -1
    (vector-set! dp 0 1) ; Base case: there's 1 subsequence for an empty string
    (for ([i (in-range 1 (+ n 1))]) ; Loop through each character in s
      (let* ([char (string-ref s (- i 1))] ; Current character
             [char-index (- (char->integer char) (char->integer #\a))] ; Index of the character in 'last' array
             [dp-val (modulo (* 2 (vector-ref dp (- i 1))) mod)]) ; Calculate dp value without considering last occurrence
        (when (not (= (vector-ref last char-index) -1)) ; If the character has appeared before
          ; Subtract the dp value of the position before its last occurrence to remove duplicates
          (set! dp-val (modulo (- dp-val (vector-ref dp (vector-ref last char-index))) mod)))
        (vector-set! dp i (modulo (+ dp-val mod) mod)) ; Update the dp table considering mod
        (vector-set! last char-index (- i 1)))) ; Update the last occurrence of the character
    (modulo (- (vector-ref dp n) 1) mod))) ; Subtract 1 to exclude the empty subsequence and apply mod

;; The function iterates over each character in the string, maintaining a dynamic programming table 'dp'
;; where dp[i] represents the number of distinct subsequences up to the ith character, modulo 10^9+7.
;; 'last' is an array to keep track of the last occurrence of each character to avoid counting duplicates.
;; Finally, we adjust for the modulo and exclude the empty sequence by subtracting 1 from the final count.
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