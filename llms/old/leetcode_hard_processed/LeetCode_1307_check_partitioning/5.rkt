#lang racket

;; Given a string s, return true if it is possible to split the string s into three non-empty palindromic substrings. Otherwise, return false.
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
  ;; Helper function to check if a substring (defined by start and end indices) is a palindrome.
  (define (is-palindrome? s start end)
    (let loop ((start start) (end end))
      (or (>= start end)
          (and (equal? (string-ref s start) (string-ref s end))
               (loop (add1 start) (sub1 end))))))
  
  ;; Main logic to check for the partitioning.
  ;; Iterate over all possible splits, checking if each part is a palindrome.
  (let ((n (string-length s)))
    (let loop ((i 0))
      (if (< i (- n 2))
          (let loop-inner ((j (+ i 1)))
            (if (< j (- n 1))
                (if (and (is-palindrome? s 0 i)
                         (is-palindrome? s (+ i 1) j)
                         (is-palindrome? s (+ j 1) (- n 1)))
                    #t
                    (loop-inner (add1 j)))
                (loop (add1 i))))
          #f))))

;; Example usage:
;; (check_partitioning "abcbdd") ; Returns: #t
;; (check_partitioning "bcbddxy") ; Returns: #f
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