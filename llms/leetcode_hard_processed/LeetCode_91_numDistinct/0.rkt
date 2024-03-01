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
  (let* ((m (string-length s))
         (n (string-length t))
         ;; Initialize a 2D list (m+1 x n+1) with zeros
         (dp (for/list ([i (in-range (add1 m))])
               (make-vector (add1 n) 0))))
    ;; Fill the first column with 1s
    (for ([i (in-range (add1 m))])
      (vector-set! (list-ref dp i) 0 1))
    ;; Loop through each character of s and t to fill the dp table
    (for ([i (in-range 1 (add1 m))])
      (for ([j (in-range 1 (add1 n))])
        (let ((s-char (string-ref s (sub1 i)))
              (t-char (string-ref t (sub1 j))))
          (vector-set! (list-ref dp i) j
                       (if (char=? s-char t-char)
                           (modulo (+ (vector-ref (list-ref dp (sub1 i)) (sub1 j))
                                      (vector-ref (list-ref dp (sub1 i)) j))
                                    1000000007)
                           (vector-ref (list-ref dp (sub1 i)) j))))))
    ;; Return the bottom-right value of the dp table
    (vector-ref (list-ref dp m) n)))

;; Example usage:
(numDistinct "rabbbit" "rabbit") ; Should return 3
(numDistinct "babgbag" "bag")    ; Should return 5
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