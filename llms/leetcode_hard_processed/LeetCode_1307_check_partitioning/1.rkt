#lang racket

;; Checks if a substring of `s` starting at `start` and ending at `end` is a palindrome.
(define (is-palindrome? s start end)
  ;; Loop until the start index is greater than or equal to the end index.
  (let loop ((start start) (end end))
    (cond
      ;; If start is greater than or equal to end, we've checked all characters, so return true.
      ((>= start end) #t)
      ;; If the characters at the start and end indexes are not the same, return false.
      ((not (= (char->integer (string-ref s start))
               (char->integer (string-ref s end))))
       #f)
      ;; Otherwise, move towards the center of the string and continue checking.
      (else (loop (+ start 1) (- end 1))))))

;; Given a string `s`, returns true if it is possible to split the string `s`
;; into three non-empty palindromic substrings. Otherwise, returns false.
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
    (let loop-outer ((i 0))
      (cond
        ;; If `i` has reached `n-2`, we have checked all possible partitions, so return false.
        ((>= i (- n 2)) #f)
        ;; If the substring from 0 to `i` is a palindrome, check the rest of the string.
        ((is-palindrome? s 0 i)
         (let loop-inner ((j (+ i 1)))
           (cond
             ;; If `j` has reached `n-1`, move to the next `i`.
             ((>= j (- n 1)) (loop-outer (+ i 1)))
             ;; If the substring from `i+1` to `j` and the substring from `j+1` to `n-1` are palindromes,
             ;; we have found a valid partitioning, so return true.
             ((and (is-palindrome? s (+ i 1) j)
                   (is-palindrome? s (+ j 1) (- n 1)))
              #t)
             ;; Otherwise, check the next `j`.
             (else (loop-inner (+ j 1))))))
        ;; If the substring from 0 to `i` is not a palindrome, check the next `i`.
        (else (loop-outer (+ i 1)))))))

;; Example usage
(check_partitioning "abcbdd")  ; returns #t (true)
(check_partitioning "bcbddxy") ; returns #f (false)
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