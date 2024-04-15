#lang racket

;; Function to check if a substring (defined by start and end indices) is a palindrome.
;; It iterates from both ends towards the center, comparing characters.
(define (is-palindrome? s start end)
  ;; Loop until start index is less than or equal to end index.
  (let loop ((start start) (end end))
    (cond
      ((> start end) #t) ; If start > end, all characters matched, it's a palindrome.
      ((not (char=? (string-ref s start) (string-ref s end))) #f) ; If chars at start and end don't match, not a palindrome.
      (else (loop (+ start 1) (- end 1)))))) ; Otherwise, move towards the center and continue checking.

;; Main function to check if the string can be partitioned into 3 palindromic substrings.
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
  (let ((n (string-length s)))
    ;; Outer loop to try partitioning after every character starting from 0 to n-3 (inclusive).
    (let loop1 ((i 0))
      (cond
        ((= i (- n 2)) #f) ; If i reaches n-2, partitioning not possible, return false.
        ;; Check if substring from 0 to i is a palindrome.
        ((is-palindrome? s 0 i)
         ;; If it is, start another loop to find the second partition.
         (let loop2 ((j (+ i 1)))
           (cond
             ((= j (- n 1)) (loop1 (+ i 1))) ; If j reaches n-1, try next partition for i.
             ;; Check if both substrings (i+1 to j) and (j+1 to n-1) are palindromes.
             ((and (is-palindrome? s (+ i 1) j) (is-palindrome? s (+ j 1) (- n 1)))
               #t) ; If true, partitioning found, return true.
             (else (loop2 (+ j 1)))))) ; Otherwise, try next partition for j.
        (else (loop1 (+ i 1))))))) ; If not a palindrome, try next partition for i.
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