#lang racket

;; Check if it is possible to split the string s into three non-empty palindromic substrings
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
  ;; Helper function to check if a substring (defined by start and end indexes) is a palindrome
  (define (is-palindrome start end)
    (let loop ((start start) (end end))
      (or (> start end)
          (and (char=? (string-ref s start) (string-ref s end))
               (loop (add1 start) (sub1 end))))))
  ;; Length of the string
  (define n (string-length s))
  ;; Iterate over the string to find a valid partitioning
  (let loop1 ((i 0))
    (if (< i (- n 2))
        (if (is-palindrome 0 i)
            (let loop2 ((j (+ i 1)))
              (if (< j (- n 1))
                  (if (and (is-palindrome (+ i 1) j) (is-palindrome (+ j 1) (- n 1)))
                      #t
                      (loop2 (add1 j)))
                  (loop1 (add1 i))))
            (loop1 (add1 i)))
        #f)))

;; Example usage
(check_partitioning "abcbdd")  ; returns #t
(check_partitioning "bcbddxy") ; returns #f
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