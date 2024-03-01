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
  ;; Define the modulo constant.
  (define mod (+ (expt 10 9) 7))
  ;; Get the length of the string.
  (define n (string-length s))
  ;; Initialize the dp list with 0s, and set the first element to 1.
  (define dp (cons 1 (make-list n 0)))
  ;; Initialize the last occurrence list with -1s for all 26 letters.
  (define last (make-list 26 -1))

  ;; Iterate over each character in the string.
  (for ([i (in-range 1 (+ n 1))])
    ;; Double the previous value in dp, and take modulo mod.
    (define doubled (modulo (* 2 (list-ref dp (- i 1))) mod))
    (set! dp (list-set dp i doubled))
    
    ;; Get the index of the current character in the alphabet (0-25).
    (define idx (- (char->integer (string-ref s (- i 1))) (char->integer #\a)))
    ;; If the current character was seen before, subtract the dp value at its last occurrence.
    (when (not (= (list-ref last idx) -1))
      (define subtracted (modulo (- (list-ref dp i) (list-ref dp (list-ref last idx))) mod))
      (set! dp (list-set dp i (modulo (+ subtracted mod) mod))))
    ;; Update the last occurrence of the current character.
    (set! last (list-set last idx (- i 1))))

  ;; Subtract 1 from the final result to exclude the empty subsequence, and take modulo mod.
  (modulo (- (list-ref dp n) 1) mod))

;; Example usage:
(distinctSubseqII "abc") ;; Output: 7
(distinctSubseqII "aba") ;; Output: 6
(distinctSubseqII "aaa") ;; Output: 3
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