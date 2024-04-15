#lang racket

;; You are given a string s of length n, and an integer k. You are tasked to find the longest subsequence repeated k times in string s.
;; A subsequence is a string that can be derived from another string by deleting some or no characters without changing the order of the remaining characters.
;; A subsequence seq is repeated k times in the string s if seq * k is a subsequence of s, where seq * k represents a string constructed by concatenating seq k times.
;; For example, "bba" is repeated 2 times in the string "bababcba", because the string "bbabba", constructed by concatenating "bba" 2 times, is a subsequence of the string "bababcba".
;; Return the longest subsequence repeated k times in string s. If multiple such subsequences are found, return the lexicographically largest one. If there is no such subsequence, return an empty string.
;; Example 1:
;; Input: s = "letsleetcode", k = 2
;; Output: "let"
;; Explanation: There are two longest subsequences repeated 2 times: "let" and "ete".
;; "let" is the lexicographically largest one.
;; Example 2:
;; Input: s = "bb", k = 2
;; Output: "b"
;; Explanation: The longest subsequence repeated 2 times is "b".
;; Example 3:
;; Input: s = "ab", k = 2
;; Output: ""
;; Explanation: There is no subsequence repeated 2 times. Empty string is returned.
;; Constraints:
;; n == s.length
;; 2 <= n, k <= 2000
;; 2 <= n < k * 8
;; s consists of lowercase English letters.
(define (longest_subsequence_repeated_k s k)
  ;; Convert the input string `s` to a list of characters
  (define s-list (string->list s))
  
  ;; Initialize a list to keep track of character counts
  (define counts (make-vector 26 0))
  
  ;; Update the counts vector with frequency of each character in the string
  (for-each (lambda (char)
              (define index (- (char->integer char) (char->integer #\a)))
              (vector-set! counts index (+ 1 (vector-ref counts index))))
            s-list)
  
  ;; Generate the subsequence by concatenating characters based on their frequency divided by k
  ;; Start from 'z' to 'a' to ensure lexicographical order from largest to smallest
  (define seq
    (list->string
     (foldr (lambda (i acc)
              (append (make-list (quotient (vector-ref counts i) k) (integer->char (+ i (char->integer #\a))))
                      acc))
            '()
            (range 25 -1 -1))))
  
  seq)

;; Example usage
(longest_subsequence_repeated_k "letsleetcode" 2) ; Should return "let"
(require rackunit)

(define (test-humaneval) 

  (let (( candidate longest_subsequence_repeated_k))
    (check-within (candidate "aaaa" 1) "aaaa" 0.001)
    (check-within (candidate "aabbccddeeff" 3) "" 0.001)
    (check-within (candidate "abcdef" 3) "" 0.001)
    (check-within (candidate "ab" 2) "" 0.001)
    (check-within (candidate "bb" 2) "b" 0.001)
    (check-within (candidate "abcdef" 1) "fedcba" 0.001)
    (check-within (candidate "abacadaea" 2) "aa" 0.001)
    (check-within (candidate "ab" 2) "" 0.001)
    (check-within (candidate "bb" 2) "b" 0.001)
    (check-within (candidate "aaaaaa" 3) "aa" 0.001)
    (check-within (candidate "bb" 1) "bb" 0.001)
    (check-within (candidate "" 2) "" 0.001)
    (check-within (candidate "abcdef" 2) "" 0.001)
    (check-within (candidate "bba" 2) "b" 0.001)
    (check-within (candidate "abbb" 4) "" 0.001)
    (check-within (candidate "zzzzzzzzzzzzz" 1) "zzzzzzzzzzzzz" 0.001)
    (check-within (candidate "aaaaaa" 7) "" 0.001)
    (check-within (candidate "fdaebc" 1) "fedcba" 0.001)
))

(test-humaneval)