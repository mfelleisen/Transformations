#lang racket

;; Given a string s, return true if it is possible to split the string s into three non-empty palindromic substrings. Otherwise, return false.​​​​​
;; A string is said to be palindrome if it the same string when reversed.
;; Example 1:
;; Input: s = "abcbdd"
;; Output: true
;; Explanation: "abcbdd" = "a" + "bcb" + "dd", and all three substrings are palindromes.
;; Example 2:
;; Input: s = "bcbddxy"
;; Output: false
;; Explanation: s cannot be split into 3 palindromes.
;; Constraints:
;; 3 <= s.length <= 2000
;; s​​​​​​ consists only of lowercase English letters.
(define (check_partitioning s)
  ;; Define a helper function to check if a substring is a palindrome.
  ;; It takes the string s, and the start and end indices of the substring.
  (define (is-palindrome? s start end)
    (let loop ((start start) (end end))
      ;; Base case: If start >= end, we have checked all characters, so return true.
      (if (>= start end)
          #t
          ;; Check if the characters at the current start and end indices are the same.
          ;; If they are, continue checking the next set of characters.
          ;; Otherwise, return false.
          (and (char=? (string-ref s start) (string-ref s end))
               (loop (add1 start) (sub1 end))))))
  ;; Get the length of the string.
  (define n (string-length s))
  ;; Loop through the string to find a valid partition.
  ;; The outer loop goes from the start of the string to the third-to-last character.
  (for/or ([i (in-range 0 (- n 2))])
    (and (is-palindrome? s 0 i)
         ;; The inner loop starts from the character after the end of the first palindrome
         ;; and goes up to the second-to-last character.
         (for/or ([j (in-range (+ i 1) (- n 1))])
           ;; Check if the second and third substrings are palindromes.
           (and (is-palindrome? s (+ i 1) j)
                (is-palindrome? s (+ j 1) (- n 1)))))))
(require rackunit)

(define (test-humaneval) 

  (let (( candidate check_partitioning))
    (check-within (candidate "abcdddddcbcbbbbabbbbbabbbbbabbbbbbabcddddddddd") #f 0.001)
    (check-within (candidate "abcdedcbaefggefg") #f 0.001)
    (check-within (candidate "abcbddxyzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzabcbaabcbaabcba") #f 0.001)
    (check-within (candidate "abcbdd") #t 0.001)
    (check-within (candidate "aabaaba") #t 0.001)
    (check-within (candidate "abcdefghihgfedcbaabcbaabcbaabcba") #t 0.001)
    (check-within (candidate "abcdefedcbazzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzabcbaabcbaabcba") #t 0.001)
    (check-within (candidate "abcbddxyzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzabcbaabcbaabcba") #f 0.001)
    (check-within (candidate "abcbaabcbaabcba") #t 0.001)
    (check-within (candidate "abcdefghihgfedcbaabcba") #f 0.001)
    (check-within (candidate "bcbddxybbddxybbddxybbddxybbddxybbddxybbddxybbddxybbddxybbddxybbddxybbddxybbddxybbddxybbddxybbddxybbddxy") #f 0.001)
    (check-within (candidate "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbcccc") #t 0.001)
    (check-within (candidate "abcdefghijklmnoponmlkjihgfedcbaabcbaabcbaabcbaabcba") #t 0.001)
    (check-within (candidate "aaabbbccc") #t 0.001)
    (check-within (candidate "abcdefedcbazzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzabcbaabcbaabcba") #t 0.001)
    (check-within (candidate "aabaaaabbbbbaababbbbbbbababaabbbbabababbaaabbbabbbaaaabaaaaababaaababbbbababababbbaabaaababaaaabaaababbbbababababbbabbbaaaabbbbabbbbaabbababbbbbabbabababaababaabbbabbbbaabbbaabababaaaaabbbbbabbbaabbaabbaabbababbbbaabbaabbaabbbaabbaabababaaabababbbabababbbabbbabbbbbb") #f 0.001)
    (check-within (candidate "abcbddxyabcdddcbaabcddabcabcddeabcdddabcddcbaabcddd") #f 0.001)
    (check-within (candidate "bcbddxy") #f 0.001)
    (check-within (candidate "aabbcc") #t 0.001)
    (check-within (candidate "aaabbbbcccc") #t 0.001)
    (check-within (candidate "abcbddxy") #f 0.001)
    (check-within (candidate "aabaabaaaabbbbbabbbbbaabaabaaabaabaaaabaaaaababaa") #f 0.001)
    (check-within (candidate "abcdefedcbaabcba") #f 0.001)
))

(test-humaneval)