#lang racket

;; You are given a string s. You can convert s to a palindrome by adding characters in front of it.
;; Return the shortest palindrome you can find by performing this transformation.
;; Example 1:
;; Input: s = "aacecaaa"
;; Output: "aaacecaaa"
;; Example 2:
;; Input: s = "abcd"
;; Output: "dcbabcd"
;; Constraints:
;; 0 <= s.length <= 5 * 104
;; s consists of lowercase English letters only.
(define (shortest_palindrome s)
  ;; Calculate the length of the input string.
  (define n (string-length s))
  ;; Reverse the input string.
  (define rev_s (list->string (reverse (string->list s))))
  
  (let loop ((i 0)) ; Define a loop starting from index 0
    (cond
      ((= i n) "") ; If i equals the length of the string, return an empty string.
      ;; Check if the substring of `s` from start to length-n+i is equal to
      ;; the substring of `rev_s` from i to the end. If they are equal, concatenate
      ;; the substring of `rev_s` from start to i (the part to be prepended) with `s`
      ;; to form the shortest palindrome.
      ((string=? (substring s 0 (- n i)) (substring rev_s i))
       (string-append (substring rev_s 0 i) s))
      ;; If not equal, increment i and continue the loop.
      (else (loop (+ i 1)))))
)

;; Example tests
(displayln (shortest_palindrome "aacecaaa")) ; Output: "aaacecaaa"
(displayln (shortest_palindrome "abcd")) ; Output: "dcbabcd"
(require rackunit)

(define (test-humaneval) 

  (let (( candidate shortest_palindrome))
    (check-within (candidate "a") "a" 0.001)
    (check-within (candidate "rotor") "rotor" 0.001)
    (check-within (candidate "abababababababa") "abababababababa" 0.001)
    (check-within (candidate "level") "level" 0.001)
    (check-within (candidate "madam") "madam" 0.001)
    (check-within (candidate "aa") "aa" 0.001)
    (check-within (candidate "aaacecaaa") "aaacecaaa" 0.001)
    (check-within (candidate "abacaba") "abacaba" 0.001)
    (check-within (candidate "xxx") "xxx" 0.001)
    (check-within (candidate "aaabaaa") "aaabaaa" 0.001)
    (check-within (candidate "aba") "aba" 0.001)
    (check-within (candidate "") "" 0.001)
    (check-within (candidate "cbaabc") "cbaabc" 0.001)
    (check-within (candidate "civic") "civic" 0.001)
    (check-within (candidate "abcddcba") "abcddcba" 0.001)
    (check-within (candidate "redder") "redder" 0.001)
    (check-within (candidate "x") "x" 0.001)
    (check-within (candidate "racecar") "racecar" 0.001)
    (check-within (candidate "b") "b" 0.001)
    (check-within (candidate "xxxx") "xxxx" 0.001)
    (check-within (candidate "aacecaaa") "aaacecaaa" 0.001)
    (check-within (candidate "a") "a" 0.001)
    (check-within (candidate "hannah") "hannah" 0.001)
    (check-within (candidate "repaper") "repaper" 0.001)
    (check-within (candidate "deified") "deified" 0.001)
    (check-within (candidate "xxxxx") "xxxxx" 0.001)
    (check-within (candidate "abababa") "abababa" 0.001)
    (check-within (candidate "deed") "deed" 0.001)
    (check-within (candidate "abc") "cbabc" 0.001)
    (check-within (candidate "a") "a" 0.001)
    (check-within (candidate "abcd") "dcbabcd" 0.001)
    (check-within (candidate "dewed") "dewed" 0.001)
))

(test-humaneval)