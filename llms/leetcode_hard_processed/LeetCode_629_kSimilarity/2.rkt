#lang racket

;; Defines a function that calculates the smallest number of swaps needed to make two strings identical.
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
  ;; Initialize the number of swaps to 0.
  (let loop ([k 0]
             [temp (string->list s1)]  ; Convert the string to a list of characters for easy manipulation.
             [i 0])                     ; Start index.
    ;; Check if the current index is less than the length of the string.
    (if (< i (length temp))
        (if (not (char=? (list-ref temp i) (string-ref s2 i)))  ; If the characters at the current index do not match,
            (let loop-inner ([j (+ i 1)])                      ; start looking for a match to swap with.
              (if (and (char=? (list-ref temp j) (string-ref s2 i))  ; If a swap candidate is found
                       (not (char=? (string-ref s2 j) (list-ref temp j))))
                  (let* ([temp-swap (swap temp i j)]                 ; Perform the swap.
                         [temp-list (list->string temp-swap)])        ; Convert back to string to keep the types consistent.
                    (loop (+ k 1) temp-swap (+ i 1)))                 ; Recurse with incremented swap count and index.
                  (loop-inner (+ j 1))))  ; Else, continue searching for a swap candidate.
            (loop k temp (+ i 1)))  ; If characters match, move to the next index without swapping.
        k)))  ; Once all characters have been checked, return the swap count.

;; Helper function to swap two elements in a list.
(define (swap lst i j)
  (let ([temp (list-ref lst i)])  ; Save the ith element.
    (list-set (list-set lst i (list-ref lst j)) j temp)))  ; Swap the elements.

;; Example usage:
(kSimilarity "ab" "ba")  ; Should return 1
(kSimilarity "abc" "bca")  ; Should return 2
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