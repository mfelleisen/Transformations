#lang racket

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
(define (k_similarity s1 s2)
  ;; Convert strings to lists of characters for easier manipulation
  (let loop ([k 0] [a (string->list s1)] [b (string->list s2)])
    (cond
      ;; If the strings (lists) are equal, return the current count of swaps (k)
      [(equal? a b) k]
      ;; Otherwise, iterate through the characters to find a mismatch and perform a swap
      [else
       (let iter ([i 0])
         (if (= i (length a))
             k
             (if (not (char=? (list-ref a i) (list-ref b i)))
                 (let inner-loop ([j (add1 i)])
                   (if (= j (length b))
                       (iter (add1 i))
                       (if (and (char=? (list-ref a i) (list-ref b j))
                                (not (char=? (list-ref a j) (list-ref b j))))
                           ;; Perform the swap and increment k
                           (loop (add1 k)
                                 (swap a i j)
                                 b)
                           (inner-loop (add1 j)))))
                 (iter (add1 i)))))])))

;; Helper function to swap elements in a list at indices i and j
(define (swap lst i j)
  (let ([tmp (list-ref lst i)])
    (list-set lst i (list-ref lst j))
    (list-set lst j tmp)))
(require rackunit)

(define (test-humaneval) 

  (let (( candidate k_similarity))
    (check-within (candidate "This is a test" "This is a test") 0 0.001)
    (check-within (candidate "abcdef" "badefc") 2 0.001)
    (check-within (candidate "abcdefghijklm" "abcdefghijklm") 0 0.001)
    (check-within (candidate "word order matters" "word order matters") 0 0.001)
    (check-within (candidate "identical" "identical") 0 0.001)
    (check-within (candidate "only words" "only words") 0 0.001)
    (check-within (candidate "abab" "baba") 2 0.001)
    (check-within (candidate "same" "same") 0 0.001)
    (check-within (candidate "This is a test" "This is a test") 0 0.001)
    (check-within (candidate "12345" "12345") 0 0.001)
    (check-within (candidate "f1.0" "f1.0") 0 0.001)
    (check-within (candidate "abcde" "bacde") 1 0.001)
    (check-within (candidate "same length" "same length") 0 0.001)
    (check-within (candidate "only words" "no numbers") 2 0.001)
    (check-within (candidate "This is not a common phrase." "This is not a common phrase.") 0 0.001)
    (check-within (candidate "ab" "ba") 1 0.001)
    (check-within (candidate "single word" "single word") 0 0.001)
    (check-within (candidate "abcdef" "abcdef") 0 0.001)
    (check-within (candidate "1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijlmnopqrstuvwxyz" "1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijlmnopqrstuvwxyz") 0 0.001)
    (check-within (candidate "abcd" "abcd") 0 0.001)
    (check-within (candidate "same chars repeated" "same chars repeated") 0 0.001)
    (check-within (candidate "big word" "big word") 0 0.001)
    (check-within (candidate "a2b3c4d5" "a2b3c4d5") 0 0.001)
    (check-within (candidate "one word" "one word") 0 0.001)
    (check-within (candidate "1ab234cd" "1ab234dc") 1 0.001)
))

(test-humaneval)