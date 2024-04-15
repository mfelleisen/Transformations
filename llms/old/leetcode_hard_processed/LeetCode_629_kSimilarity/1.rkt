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
(define (kSimilarity s1 s2)
  ;; This function calculates the minimum number of swaps needed
  ;; to make two strings identical, assuming they are anagrams.
  (let loop ([k 0] ; Counter for swaps
             [temp (string->list s1)] ; Convert s1 into a list of characters for easy manipulation
             [index 0]) ; Current index in the string we are checking
    (if (< index (length temp))
        (if (char=? (list-ref temp index) (string-ref s2 index))
            (loop k temp (+ index 1)) ; If characters match, move to the next character
            (let find-swap ([j (+ index 1)]) ; Otherwise, find a character to swap
              (if (or (not (char=? (list-ref temp j) (string-ref s2 index))) ; Conditions to find a suitable swap partner
                      (char=? (string-ref s2 j) (list-ref temp j)))
                  (find-swap (+ j 1)) ; Keep looking if not found
                  (loop (+ k 1) ; Increment swap count
                        (swap temp index j) ; Swap the characters
                        (+ index 1))))) ; Move to the next character after swapping
        k))) ; Return the swap count when done

(define (swap lst i j)
  ;; Helper function to swap elements in a list
  (let ([temp-i (list-ref lst i)])
    (list-set (list-set lst i (list-ref lst j)) j temp-i)))

;; Usage example:
(kSimilarity "ab" "ba") ; Should return 1
(kSimilarity "abc" "bca") ; Should return 2
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