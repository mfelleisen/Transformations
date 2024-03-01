#lang racket

;; Given a string s, return the number of distinct non-empty subsequences of s.
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
  (let* ([mod (+ (expt 10 9) 7)] ; Define the modulo value
         [n (string-length s)] ; Length of the input string
         [dp (make-vector (+ n 1) 0)] ; Initialize dp vector with 0s, size n+1
         [last (make-vector 26 -1)]) ; Track last occurrence of each character, initialized to -1
    (vector-set! dp 0 1) ; Base case: one way to form an empty subsequence
    (for ([i (in-range 1 (+ n 1))]) ; Iterate through characters of s
      (let* ([ch (string-ref s (- i 1))] ; Current character
             [idx (- (char->integer ch) (char->integer #\a))]) ; Index in 'last' vector
        ;; Update dp[i] considering all subsequences ending with ch
        (vector-set! dp i (modulo (* 2 (vector-ref dp (- i 1))) mod))
        ;; If ch has appeared before, subtract the count of subsequences
        ;; before its first occurrence to avoid counting duplicates
        (when (not (= (vector-ref last idx) -1))
          (vector-set! dp i (modulo (- (vector-ref dp i) (vector-ref dp (vector-ref last idx))) mod)))
        ;; Update the last occurrence of ch
        (vector-set! last idx (- i 1))))
    ;; Subtract 1 to exclude the empty subsequence, then return
    (modulo (- (vector-ref dp n) 1) mod)))

;; Note: Racket's `modulo` function is used for modulo operation,
;; `vector-set!` to update elements in mutable vectors,
;; and `for` loop with `in-range` for iteration.
;; This implementation closely follows the Python version's logic
;; while adapting it to Racket's idioms and types.
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