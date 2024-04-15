#lang racket

;; Given two strings s and t, return the number of distinct subsequences of s which equals t modulo 109 + 7.
;; Given two strings s and t, return the number of distinct subsequences of s which equals t modulo 109 + 7.
;; Example 1:
;; Input: s = "rabbbit", t = "rabbit"
;; Output: 3
;; Explanation:
;; As shown below, there are 3 ways you can generate "rabbit" from s.
;; rabbbit
;; rabbbit
;; rabbbit
;; Example 2:
;; Input: s = "babgbag", t = "bag"
;; Output: 5
;; Explanation:
;; As shown below, there are 5 ways you can generate "bag" from s.
;; babgbag
;; babgbag
;; babgbag
;; babgbag
;; babgbag
;; Constraints:
;; 1 <= s.length, t.length <= 1000
;; s and t consist of English letters.
(define (numDistinct s t)
  (let* ([m (string-length s)]             ; Length of string s
         [n (string-length t)]             ; Length of string t
         [dp (make-vector (+ m 1) 0)])     ; Initialize a vector dp with m+1 elements, each initialized to 0
    ;; Initialize dp as a two-dimensional vector where each element is a vector itself
    (for ([i (in-range (+ m 1))])
      (vector-set! dp i (make-vector (+ n 1) 0)))
    ;; Base case initialization, where the number of ways to form an empty string t from any prefix of s is 1
    (for ([i (in-range (+ m 1))])
      (vector-set! (vector-ref dp i) 0 1))
    ;; Fill dp table
    (for ([i (in-range 1 (+ m 1))])
      (for ([j (in-range 1 (+ n 1))])
        (let ([s-char (string-ref s (- i 1))]  ; Get i-th character of s (0-indexed)
              [t-char (string-ref t (- j 1))]) ; Get j-th character of t (0-indexed)
          ;; If characters match, sum of two cases:
          ;; 1. Count excluding the current character in s
          ;; 2. Count including the current character in s
          ;; Otherwise, take the count excluding the current character in s
          (vector-set! (vector-ref dp i) j
                       (if (char=? s-char t-char)
                           (+ (vector-ref (vector-ref dp (- i 1)) (- j 1))
                              (vector-ref (vector-ref dp (- i 1)) j))
                           (vector-ref (vector-ref dp (- i 1)) j))))))
    ;; Return the bottom-right value of the dp table modulo 10^9 + 7
    (modulo (vector-ref (vector-ref dp m) n) (+ (expt 10 9) 7))))

;; Example usage
(define s "rabbbit")
(define t "rabbit")
(numDistinct s t)  ; Output: 3
(require rackunit)

(define (test-humaneval) 

  (let (( candidate numDistinct))
    (check-within (candidate "abc" "abcabc") 0 0.001)
    (check-within (candidate "xyz" "xy") 1 0.001)
    (check-within (candidate "xyz" "x") 1 0.001)
    (check-within (candidate "xyz" "yz") 1 0.001)
    (check-within (candidate "xyz" "x") 1 0.001)
    (check-within (candidate "xyz" "y") 1 0.001)
    (check-within (candidate "abcabc" "ab") 3 0.001)
    (check-within (candidate "abcabc" "bc") 3 0.001)
    (check-within (candidate "rabbbit" "rabbit") 3 0.001)
    (check-within (candidate "aaaaaa" "aa") 15 0.001)
    (check-within (candidate "abcabcabcabcabcabc" "abcabcabcabcabcabc") 1 0.001)
    (check-within (candidate "xyz" "xy") 1 0.001)
    (check-within (candidate "" "") 1 0.001)
    (check-within (candidate "xyz" "xz") 1 0.001)
    (check-within (candidate "a" "") 1 0.001)
    (check-within (candidate "abcabc" "abcabca") 0 0.001)
    (check-within (candidate "xyz" "w") 0 0.001)
    (check-within (candidate "xyz" "y") 1 0.001)
    (check-within (candidate "xyz" "xyz") 1 0.001)
    (check-within (candidate "abcabcabcabcabcabc" "abcabcabcabcabcabca") 0 0.001)
    (check-within (candidate "babgbag" "bag") 5 0.001)
    (check-within (candidate "abcabcabcabcabcabc" "a") 6 0.001)
    (check-within (candidate "abcabcabcabcabcabc" "abcabcabcabcabcabc") 1 0.001)
    (check-within (candidate "xyz" "z") 1 0.001)
    (check-within (candidate "abcabc" "abcabc") 1 0.001)
    (check-within (candidate "abcde" "ace") 1 0.001)
    (check-within (candidate "a" "") 1 0.001)
    (check-within (candidate "abcabcabcabcabcabc" "a") 6 0.001)
    (check-within (candidate "abcabc" "abcabca") 0 0.001)
    (check-within (candidate "abcabcabcabcabcabc" "abcabcabcabcabcabca") 0 0.001)
    (check-within (candidate "xyz" "w") 0 0.001)
    (check-within (candidate "xyz" "xz") 1 0.001)
    (check-within (candidate "abcabc" "bc") 3 0.001)
    (check-within (candidate "abcabc" "b") 2 0.001)
    (check-within (candidate "xyz" "z") 1 0.001)
    (check-within (candidate "babgbag" "bag") 5 0.001)
    (check-within (candidate "abcabc" "a") 2 0.001)
    (check-within (candidate "abcabc" "c") 2 0.001)
    (check-within (candidate "" "") 1 0.001)
    (check-within (candidate "abcde" "ace") 1 0.001)
    (check-within (candidate "abcabc" "a") 2 0.001)
    (check-within (candidate "rabbbit" "rabbit") 3 0.001)
    (check-within (candidate "xyz" "xyz") 1 0.001)
    (check-within (candidate "abc" "abcabc") 0 0.001)
))

(test-humaneval)