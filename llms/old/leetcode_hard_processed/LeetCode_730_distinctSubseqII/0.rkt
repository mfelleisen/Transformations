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
  (let* ((mod 1000000007) ; Define the modulo as per the problem statement
         (n (string-length s)) ; Calculate the length of the input string
         (dp (make-vector (+ n 1) 0)) ; Initialize dp vector with zeros, size is n+1
         (last (make-vector 26 -1))) ; Initialize last occurrences with -1 for each character
    (vector-set! dp 0 1) ; Base case, empty subsequence
    (for ([i (in-range 1 (+ n 1))]) ; Loop through characters of the string
      (let* ((char-index (- (char->integer (string-ref s (- i 1))) (char->integer #\a))) ; Calculate current character index (0-25)
             (dp-val (remainder (* 2 (vector-ref dp (- i 1))) mod))) ; Calculate dp value
        ; Update dp[i] considering the subsequence ending with the current character
        (vector-set! dp i (if (= (vector-ref last char-index) -1)
                              dp-val
                              (remainder (- (+ dp-val mod) (vector-ref dp (vector-ref last char-index))) mod)))
        ; Update last occurrence of the current character
        (vector-set! last char-index (- i 1))))
    ; Adjust the final count by subtracting 1 (to exclude empty subsequence) and return
    (remainder (- (+ (vector-ref dp n) mod) 1) mod)))

;; Example usage
(distinctSubseqII "abc") ; Should return 7
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