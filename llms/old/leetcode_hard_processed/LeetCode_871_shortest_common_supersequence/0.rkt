#lang racket

;; Given two strings str1 and str2, return the shortest string that has both str1 and str2 as subsequences. If there are multiple valid strings, return any of them.
;; A string s is a subsequence of string t if deleting some number of characters from t (possibly 0) results in the string s.
;; Example 1:
;; Input: str1 = "abac", str2 = "cab"
;; Output: "cabac"
;; Explanation: 
;; str1 = "abac" is a subsequence of "cabac" because we can delete the first "c".
;; str2 = "cab" is a subsequence of "cabac" because we can delete the last "ac".
;; The answer provided is the shortest such string that satisfies these properties.
;; Example 2:
;; Input: str1 = "aaaaaaaa", str2 = "aaaaaaaa"
;; Output: "aaaaaaaa"
;; Constraints:
;; 1 <= str1.length, str2.length <= 1000
;; str1 and str2 consist of lowercase English letters.
(define (shortest_common_supersequence str1 str2)
  ;; Calculate lengths of input strings
  (define m (string-length str1))
  (define n (string-length str2))
  ;; Initialize a 2D list (matrix) for dynamic programming
  (define dp (make-matrix (+ m 1) (+ n 1) 0))
  
  ;; Fill the dp matrix
  (for ([i (in-range 0 (+ m 1))])
    (for ([j (in-range 0 (+ n 1))])
      (set-matrix! dp i j
                   (cond
                     [(zero? i) j]
                     [(zero? j) i]
                     [(char=? (string-ref str1 (- i 1)) (string-ref str2 (- j 1)))
                      (+ 1 (matrix-ref dp (- i 1) (- j 1)))]
                     [else
                      (+ 1 (min (matrix-ref dp (- i 1) j)
                                (matrix-ref dp i (- j 1))))]))))
  
  ;; Backtrack to find the result
  (define (backtrack i j)
    (cond
      [(and (zero? i) (zero? j)) ""]
      [(zero? i) (string-append (backtrack i (- j 1)) (string (string-ref str2 (- j 1))))]
      [(zero? j) (string-append (backtrack (- i 1) j) (string (string-ref str1 (- i 1))))]
      [(char=? (string-ref str1 (- i 1)) (string-ref str2 (- j 1)))
       (string-append (backtrack (- i 1) (- j 1)) (string (string-ref str1 (- i 1))))]
      [(> (matrix-ref dp (- i 1) j) (matrix-ref dp i (- j 1)))
       (string-append (backtrack i (- j 1)) (string (string-ref str2 (- j 1))))]
      [else
       (string-append (backtrack (- i 1) j) (string (string-ref str1 (- i 1))))]))
  
  ;; Start backtracking from bottom right corner of dp matrix
  (backtrack m n))

;; Helper functions to manipulate matrices
(define (make-matrix rows cols fill)
  (for/list ([_ (in-range rows)])
    (make-vector cols fill)))

(define (set-matrix! m i j val)
  (vector-set! (list-ref m i) j val))

(define (matrix-ref m i j)
  (vector-ref (list-ref m i) j))
(require rackunit)

(define (test-humaneval) 

  (let (( candidate shortest_common_supersequence))
    (check-within (candidate "aaabbb" "bbbccc") "aaabbbccc" 0.001)
    (check-within (candidate "abc" "") "abc" 0.001)
    (check-within (candidate "" "") "" 0.001)
    (check-within (candidate "aaabbb" "bbbccc") "aaabbbccc" 0.001)
    (check-within (candidate "abcd" "bcd") "abcd" 0.001)
    (check-within (candidate "aaaaaaaa" "aaaaaaaa") "aaaaaaaa" 0.001)
    (check-within (candidate "abc" "bcd") "abcd" 0.001)
    (check-within (candidate "abac" "cabac") "cabac" 0.001)
    (check-within (candidate "abc" "cde") "abcde" 0.001)
    (check-within (candidate "abac" "cab") "cabac" 0.001)
    (check-within (candidate "baaa" "aaa") "baaa" 0.001)
    (check-within (candidate "abcabc" "abcabc") "abcabc" 0.001)
    (check-within (candidate "abc" "abc") "abc" 0.001)
    (check-within (candidate "ab" "bc") "abc" 0.001)
    (check-within (candidate "" "aaaa") "aaaa" 0.001)
    (check-within (candidate "" "a") "a" 0.001)
    (check-within (candidate "abcdefghij" "defghijabc") "abcdefghijabc" 0.001)
    (check-within (candidate "abcde" "cde") "abcde" 0.001)
    (check-within (candidate "abcdefgh" "abcdefgh") "abcdefgh" 0.001)
    (check-within (candidate "" "def") "def" 0.001)
    (check-within (candidate "aaabbb" "bbb") "aaabbb" 0.001)
    (check-within (candidate "abcdef" "abcdef") "abcdef" 0.001)
    (check-within (candidate "aaaaaa" "aaaaaa") "aaaaaa" 0.001)
    (check-within (candidate "aaaaaaaa" "aaaaaaaa") "aaaaaaaa" 0.001)
    (check-within (candidate "ab" "ab") "ab" 0.001)
))

(test-humaneval)