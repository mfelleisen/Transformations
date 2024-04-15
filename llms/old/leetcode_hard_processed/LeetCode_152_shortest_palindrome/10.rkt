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
  ;; Convert the input string to a list of characters for easier manipulation
  (define chars (string->list s))
  ;; Reverse the list of characters to help find the matching prefix for the palindrome
  (define rev-chars (reverse chars))
  
  (let loop ((i 0))  ; Start a loop with an index i, initially 0, to track the prefix length
    (if (= i (length chars))  ; If i equals the length of the list, we've checked all prefixes
        (list->string chars)  ; All characters are part of the palindrome, return the original string
        ;; Check if the prefix of the original string (up to length - i) matches the suffix of the reversed string
        (if (equal? (take chars (- (length chars) i)) (drop rev-chars i))
            ;; If they match, construct the palindrome by combining the necessary part of the reversed string
            ;; with the original string, and convert back to a string
            (list->string (append (take rev-chars i) chars))
            (loop (add1 i)))))  ; If not a match, increment i and try the next prefix length
)
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