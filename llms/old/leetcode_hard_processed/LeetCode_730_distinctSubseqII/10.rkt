#lang racket

;; Function: distinctSubseqII
;; Given a string s, it returns the number of distinct non-empty subsequences of s modulo 10^9 + 7.
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
  ;; Define the modulo constant.
  (define mod (+ (expt 10 9) 7))
  ;; Compute the length of the string.
  (define n (string-length s))
  ;; Initialize the dp list with n+1 zeros, dp[0] is set to 1.
  (define dp (cons 1 (make-list n 0)))
  ;; Initialize the list to track the last occurrence of each character (-1 means not yet occurred).
  (define last (make-list 26 -1))
  
  ;; Iterate through the string characters.
  (for ([i (in-range 1 (+ n 1))])
    ;; Update the current dp value based on the previous value and modulo.
    (set! dp (list-set dp i (modulo (* 2 (list-ref dp (- i 1))) mod)))
    ;; Calculate the index in 'last' for the current character.
    (define index (- (char->integer (string-ref s (- i 1))) (char->integer #\a)))
    ;; If the character has appeared before, adjust the dp value.
    (when (not (= (list-ref last index) -1))
      (set! dp (list-set dp i (modulo (- (+ (list-ref dp i) mod)
                                           (list-ref dp (list-ref last index)))
                                       mod))))
    ;; Update the last occurrence of the current character.
    (set! last (list-set last index (- i 1))))
  
  ;; Adjust the final value to exclude the empty subsequence and return.
  (modulo (- (+ (list-ref dp n) mod) 1) mod))

;; Example usage:
;; (distinctSubseqII "abc")  ; Returns 7
;; (distinctSubseqII "aba")  ; Returns 6
;; (distinctSubseqII "aaa")  ; Returns 3
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