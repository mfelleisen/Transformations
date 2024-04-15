#lang racket

;; Function to compute the minimum number of swaps required to transform s1 into s2
;; Strings s1 and s2 are k-similar (for some non-negative integer k) if we can swap the positions of two letters in s1 exactly k times so that the resulting string equals s2.
;; Given two anagrams s1 and s2, return the smallest k for which s1 and s2 are k-similar.
;; Example 1:
;; Input: s1 = "ab", s2 = "ba"
;; Output: 1
;; Explanation: The two string are 1-similar because we can use one swap to change s1 to s2: "ab" --> "ba".
;; Example 2:
;; Input: s1 = "abc", s2 = "bca"
;; Output: 2
;; Explanation: The two strings are 2-similar because we can use two swaps to change s1 to s2: "abc" --> "bac" --> "bca".
;; Constraints:
;; 1 <= s1.length <= 20
;; s2.length == s1.length
;; s1 and s2 contain only lowercase letters from the set {'a', 'b', 'c', 'd', 'e', 'f'}.
;; s2 is an anagram of s1.
(define (kSimilarity s1 s2)
  ;; Initialize the number of swaps (k) to 0
  (let loop ([k 0] [s1 (string->list s1)] [s2 (string->list s2)])
    (cond
      ;; Base case: if s1 and s2 are equal, return the number of swaps (k)
      [(equal? s1 s2) k]
      [else
       ;; Find the first position where s1 and s2 differ
       (let* ([i (for/first ([idx (in-range (length s1))]
                             #:when (not (char=? (list-ref s1 idx) (list-ref s2 idx))))
                   idx)]
              ;; Find a character in s1, starting from the next position of i, that matches the ith character of s2
              ;; and ensure the corresponding character in s2 is not already in its correct position (to avoid unnecessary swaps)
              [j (for/first ([idx (in-range (add1 i) (length s1))]
                             #:when (and (char=? (list-ref s1 idx) (list-ref s2 i))
                                         (not (char=? (list-ref s2 idx) (list-ref s1 idx)))))
                   idx)])
         ;; Swap the characters in s1 at positions i and j
         (define swapped-s1 (swap-list-elements s1 i j))
         ;; Recur with the swapped string, incrementing k by 1
         (loop (add1 k) swapped-s1 s2))])))

;; Helper function to swap elements in a list at given indices
(define (swap-list-elements lst idx1 idx2)
  (for/list ([idx (in-range (length lst))])
    (cond
      [(= idx idx1) (list-ref lst idx2)]
      [(= idx idx2) (list-ref lst idx1)]
      [else (list-ref lst idx)])))

;; Example usage:
(kSimilarity "ab" "ba") ; Output: 1
(kSimilarity "abc" "bca") ; Output: 2
(require rackunit)

(define (test-humaneval) 

  (let (( candidate kSimilarity))
    (check-within (candidate "abcabcabc" "abcabcacb") 1 0.001)
    (check-within (candidate "abcde" "edcba") 2 0.001)
    (check-within (candidate "aabbccdd" "ddccbbaa") 4 0.001)
    (check-within (candidate "abcabcabc" "abcabccab") 2 0.001)
    (check-within (candidate "abcabcabc" "abcabccba") 1 0.001)
    (check-within (candidate "abcdef" "fedcba") 3 0.001)
    (check-within (candidate "ab" "ba") 1 0.001)
    (check-within (candidate "abcde" "abced") 1 0.001)
    (check-within (candidate "abcabcabc" "abccbaacb") 2 0.001)
    (check-within (candidate "abcd" "dcba") 2 0.001)
    (check-within (candidate "abcabcabc" "cbaabcabc") 1 0.001)
    (check-within (candidate "abcdefabcdef" "fedcbafedcba") 6 0.001)
    (check-within (candidate "abc" "acb") 1 0.001)
    (check-within (candidate "abc" "bca") 2 0.001)
    (check-within (candidate "empty" "empty") 0 0.001)
    (check-within (candidate "abcde" "abcde") 0 0.001)
    (check-within (candidate "abcabcabc" "abcabcabc") 0 0.001)
    (check-within (candidate "abcdef" "abcdef") 0 0.001)
    (check-within (candidate "abc" "cab") 2 0.001)
    (check-within (candidate "qwertyuioplkjhgfdsazxcvbnmqwerty" "qwertyuioplkjhgfdsazxcvbnmqwerty") 0 0.001)
    (check-within (candidate "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv" "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv") 0 0.001)
    (check-within (candidate "abcdef" "acbdef") 1 0.001)
    (check-within (candidate "abcabcabc" "abccbaabc") 1 0.001)
    (check-within (candidate "a" "a") 0 0.001)
))

(test-humaneval)