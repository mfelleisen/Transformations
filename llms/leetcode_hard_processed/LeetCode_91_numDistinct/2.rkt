#lang racket

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
  (let* ([m (string-length s)] ; Length of string s
         [n (string-length t)] ; Length of string t
         [mod 1000000007] ; Modulus value
         [dp (make-vector (+ m 1) #f)]) ; Initialize DP vector of vectors
    ;; Initialize DP matrix
    (for ([i (in-range 0 (+ m 1))])
      (vector-set! dp i (make-vector (+ n 1) 0))
      (vector-set! (vector-ref dp i) 0 1))
    ;; DP computation
    (for ([i (in-range 1 (+ m 1))])
      (for ([j (in-range 1 (+ n 1))])
        (let* ([si (substring s (- i 1) i)] ; ith char of s
               [tj (substring t (- j 1) j)]) ; jth char of t
          (vector-set! (vector-ref dp i) j
                       (if (string=? si tj)
                           (modulo (+ (vector-ref (vector-ref dp (- i 1)) (- j 1))
                                      (vector-ref (vector-ref dp (- i 1)) j))
                                   mod)
                           (vector-ref (vector-ref dp (- i 1)) j))))))
    (vector-ref (vector-ref dp m) n))) ; Return final DP value

;; Example calls
(define s1 "rabbbit")
(define t1 "rabbit")
(define s2 "babgbag")
(define t2 "bag")

(numDistinct s1 t1) ; Output: 3
(numDistinct s2 t2) ; Output: 5
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