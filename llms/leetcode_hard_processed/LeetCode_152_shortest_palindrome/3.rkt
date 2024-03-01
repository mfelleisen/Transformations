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
  ;; Calculate the length of the input string
  (define n (string-length s))
  ;; Reverse the input string
  (define rev_s (list->string (reverse (string->list s))))
  
  ;; Loop over the string to find the point where
  ;; the start of the string matches the end of the reversed string.
  ;; This indicates the minimum addition needed to make it a palindrome.
  (let loop ([i 0])
    (cond
      ;; If we have checked all possible prefixes,
      ;; return an empty string (base case, should not happen as per problem statement).
      [(= i n) ""]
      
      ;; When the prefix of the original string matches the suffix of the reversed string,
      ;; concatenate the necessary part of the reversed string to the original string
      ;; to make the shortest palindrome.
      [(string=? (substring s 0 (- n i)) (substring rev_s i))
       (string-append (substring rev_s 0 i) s)]
      
      ;; Otherwise, keep checking by increasing i.
      [else (loop (add1 i))])))

;; Example usage:
(shortest_palindrome "aacecaaa") ; Output: "aaacecaaa"
(shortest_palindrome "abcd")     ; Output: "dcbabcd"
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