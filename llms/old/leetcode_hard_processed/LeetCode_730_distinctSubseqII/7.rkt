#lang racket

;; Function to calculate the number of distinct non-empty subsequences of a string modulo 10^9 + 7.
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
  (let* ([mod (+ (expt 10 9) 7)] ; Define the modulo value.
         [n (string-length s)] ; Get the length of the input string.
         [dp (make-vector (+ n 1) 0)] ; Initialize the DP array with zeros, of length n+1.
         [last (make-vector 26 -1)]) ; Initialize the 'last' array to track the last occurrence of each character.
    (vector-set! dp 0 1) ; Base case: there's one way to form a subsequence of an empty string.
    (for ([i (in-range 1 (+ n 1))]) ; Loop over the string.
      (let* ([char (string-ref s (- i 1))] ; Get the current character.
             [char-index (- (char->integer char) (char->integer #\a))]) ; Map 'a' to 0, 'b' to 1, etc.
        ;; Double the number of subsequences, taking modulo.
        (vector-set! dp i (modulo (* 2 (vector-ref dp (- i 1))) mod))
        ;; If the character was seen before, subtract the number of subsequences before its first occurrence.
        (when (not (= (vector-ref last char-index) -1))
          (vector-set! dp i (modulo (- (+ (vector-ref dp i) mod) (vector-ref dp (vector-ref last char-index))) mod)))
        ;; Update the last occurrence of the current character.
        (vector-set! last char-index (- i 1))))
    ;; Subtract one to exclude the empty subsequence and return the result.
    (modulo (- (+ (vector-ref dp n) mod) 1) mod)))

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