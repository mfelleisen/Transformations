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
  (let* ([mod (+ (expt 10 9) 7)]  ; Define the modulo value.
         [n (string-length s)]  ; Get the length of the input string.
         [dp (make-vector (+ n 1) 0)]  ; Initialize the DP array with zeros.
         [last (make-vector 26 -1)])  ; Initialize the last occurrence array with -1.
    (vector-set! dp 0 1)  ; Base case: there's 1 way to form a subsequence of an empty string.
    (for ([i (in-range 1 (+ n 1))])  ; Loop through the string characters.
      (let* ([char-index (- (char->integer (string-ref s (- i 1))) (char->integer #\a))]  ; Get the index of the current character in the alphabet.
             [prev-index (vector-ref last char-index)])  ; Get the last occurrence of the current character.
        (vector-set! dp i (modulo (* 2 (vector-ref dp (- i 1))) mod))  ; Double the ways and apply modulo.
        (unless (= prev-index -1)  ; If the character has appeared before.
          (vector-set! dp i (modulo (- (vector-ref dp i) (vector-ref dp prev-index)) mod)))  ; Subtract the previous occurrence's ways and apply modulo.
        (vector-set! last char-index (- i 1))))  ; Update the last occurrence of the current character.
    (modulo (- (vector-ref dp n) 1) mod)))  ; Subtract 1 to exclude the empty subsequence and apply modulo.

;; Example usage:
(distinctSubseqII "abc")  ; => 7
(distinctSubseqII "aba")  ; => 6
(distinctSubseqII "aaa")  ; => 3
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