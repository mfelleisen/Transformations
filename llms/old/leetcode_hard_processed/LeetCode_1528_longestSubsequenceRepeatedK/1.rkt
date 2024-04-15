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
  ;; Count occurrences of each character in the string
  (define counts (make-vector 26 0))
  (for ([c (string->list s)])
    (vector-set! counts (- (char->integer c) (char->integer #\a))
                 (+ 1 (vector-ref counts (- (char->integer c) (char->integer #\a))))))
  
  ;; Generate the sequence with characters that appear at least k times, each repeated floor(count/k) times
  (define seq
    (list->string
     (foldl (lambda (i acc)
              (append acc (make-list (quotient (vector-ref counts i) k) (integer->char (+ i (char->integer #\a))))))
            '() (range 26))))
  
  ;; Memoization for dynamic programming approach
  (define memo (make-hash))
  
  ;; Recursive helper function for dynamic programming
  (define (dp i j)
    (match (hash-ref memo (cons i j) #f)
      [#f (let ([res (if (>= i (string-length seq))
                         0
                         (if (>= j (string-length seq))
                             0
                             (if (char=? (string-ref seq i) (string-ref seq j))
                                 (+ 1 (dp (+ i 1) (+ j 1)))
                                 (max (dp (+ i 1) j) (dp i (+ j 1))))))])
            (hash-set! memo (cons i j) res)
            res)]
      [cached-val cached-val]))
  
  ;; Build the longest subsequence repeated k times
  (define (build-result i j res)
    (if (or (>= j (string-length seq)) (= (length res) (quotient (string-length seq) k)))
        (list->string (reverse res))
        (if (or (null? res) (char>? (string-ref seq j) (car res)))
            (if (>= (dp i j) (* (- (quotient (string-length seq) k) (length res)) 2))
                (build-result (+ j 1) (+ j 1) (cons (string-ref seq j) res))
                (build-result i (+ j 1) res))
            (build-result i (+ j 1) res))))
  
  ;; Kick off the result building process
  (build-result 0 0 '()))

;; Example usage:
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