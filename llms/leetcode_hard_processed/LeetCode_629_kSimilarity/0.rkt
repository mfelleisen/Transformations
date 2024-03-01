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
  (let loop ([temp (string->list s1)] ; Convert string s1 to a list of characters for manipulation
             [k 0]                    ; Initialize the swap count k to 0
             [index 0])               ; Start index at 0 for iteration
    (if (= index (length temp))       ; If index reaches the length of temp, we're done
        k                             ; Return the swap count k
        (if (char=? (list-ref temp index) (string-ref s2 index)) ; If characters match, move to next
            (loop temp k (+ index 1)) ; No swap needed, increment index
            (let find-swap ([j (+ index 1)]) ; Otherwise, find a character to swap
              (if (and (char=? (list-ref temp j) (string-ref s2 index)) ; If a suitable swap found
                       (not (char=? (string-ref s2 j) (list-ref temp j))))
                  (let ([new-temp (swap temp index j)]) ; Swap in a copy of temp
                    (loop new-temp (+ k 1) (+ index 1))) ; Recur with the swapped list, increment k and index
                  (find-swap (+ j 1)))))))) ; Otherwise, continue searching for a swap

(define (swap lst i j)
  ; Swap the elements at indices i and j in lst
  (let ([temp (list-ref lst i)])
    (list-set (list-set lst i (list-ref lst j)) j temp)))

;; Example usage:
(kSimilarity "abc" "bca") ; Output: 2
(kSimilarity "ab" "ba")   ; Output: 1
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