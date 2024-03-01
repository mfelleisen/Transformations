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
(define (longestSubsequenceRepeatedK s k)
  ;; Count the frequency of each character in the string
  (define counts (make-vector 26 0))
  (for ([c (string->list s)])
    (vector-set! counts (- (char->integer c) (char->integer #\a))
                 (+ 1 (vector-ref counts (- (char->integer c) (char->integer #\a))))))
  
  ;; Generate the sequence of characters that appear at least k times, repeated according to their frequency divided by k
  (define seq
    (list->string
     (foldl (lambda (i acc)
              (append acc (make-list (quotient (vector-ref counts i) k) (integer->char (+ i (char->integer #\a))))))
            '()
            (range 26))))
  
  ;; Memoization table for dynamic programming
  (define memo (make-hash))

  ;; Dynamic programming function to calculate the longest common subsequence length
  (define (dp i j)
    (cond [(hash-has-key? memo (cons i j)) (hash-ref memo (cons i j))]
          [(or (>= i (string-length seq)) (>= j (string-length seq))) 0]
          [else
           (define res
             (if (char=? (string-ref seq i) (string-ref seq j))
                 (+ 1 (dp (+ i 1) (+ j 1)))
                 (max (dp (+ i 1) j) (dp i (+ j 1)))))
           (hash-set! memo (cons i j) res)
           res]))

  ;; Construct the result string by selecting characters based on the dynamic programming results
  (define (construct-result)
    (let loop ([i 0] [j 0] [res '()])
      (if (and (< j (string-length seq)) (< (length res) (quotient (string-length seq) k)))
          (if (or (null? res) (char>? (string-ref seq j) (car res)))
              (if (>= (dp i j) (* (- (quotient (string-length seq) k) (length res)) 2))
                  (loop (+ j 1) (+ j 1) (cons (string-ref seq j) res))
                  (loop i (+ j 1) res))
              (loop i (+ j 1) res))
          (list->string (reverse res)))))

  (construct-result))

;; Example usage
(longestSubsequenceRepeatedK "letsleetcode" 2)
(require rackunit)

(define (test-humaneval) 

  (let (( candidate longestSubsequenceRepeatedK))
    (check-within (candidate "ab" 2) "" 0.001)
    (check-within (candidate "" 1) "" 0.001)
    (check-within (candidate "ab" 2) "" 0.001)
    (check-within (candidate "abcde" 1) "" 0.001)
    (check-within (candidate "bbcbbabcba" 6) "" 0.001)
    (check-within (candidate "bbcbbabcba" 4) "" 0.001)
    (check-within (candidate "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" 1001) "" 0.001)
    (check-within (candidate "" 2) "" 0.001)
    (check-within (candidate "" 1001) "" 0.001)
    (check-within (candidate "bbcbbabcba" 7) "" 0.001)
    (check-within (candidate "abcde" 3) "" 0.001)
    (check-within (candidate "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" 3) "a" 0.001)
    (check-within (candidate "abacadaeafagahaijj" 2) "a" 0.001)
    (check-within (candidate "aabbccddeeffgghhiijj" 3) "" 0.001)
    (check-within (candidate "bbcbbabcba" 5) "" 0.001)
    (check-within (candidate "abacadaeafagahaiaj" 2) "a" 0.001)
    (check-within (candidate "abababab" 2) "ab" 0.001)
    (check-within (candidate "ab" 2) "" 0.001)
    (check-within (candidate "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" 1000) "" 0.001)
))

(test-humaneval)