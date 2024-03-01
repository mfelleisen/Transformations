#lang racket

;; Given a string s, return the last substring of s in lexicographical order.
;; Example 1:
;; Input: s = "abab"
;; Output: "bab"
;; Explanation: The substrings are ["a", "ab", "aba", "abab", "b", "ba", "bab"]. The lexicographically maximum substring is "bab".
;; Example 2:
;; Input: s = "leetcode"
;; Output: "tcode"
;; Constraints:
;; 1 <= s.length <= 4 * 105
;; s contains only lowercase English letters.
(define (lastSubstring s)
  ;; Initialize maxIndex to 0, representing the starting index of the current maximum substring
  (let loop ((maxIndex 0) (curIndex 1))
    (if (< curIndex (string-length s))
        ;; If curIndex is within the bounds of the string
        (let inner-loop ((i 0))
          (cond
           ;; If we reach the end of the string with curIndex + i, stop the search
           ((= (+ curIndex i) (string-length s)) (substring s maxIndex))
           ;; If characters at maxIndex + i and curIndex + i are the same, continue searching
           ((char=? (string-ref s (+ maxIndex i)) (string-ref s (+ curIndex i)))
            (inner-loop (+ i 1)))
           ;; If we find a lexicographically larger character at curIndex + i, update maxIndex
           ((char<? (string-ref s (+ maxIndex i)) (string-ref s (+ curIndex i)))
            (loop curIndex (+ curIndex 1)))
           ;; If the character at maxIndex + i is larger, simply move to the next index
           (else (loop maxIndex (+ curIndex 1)))))
        ;; If curIndex is beyond the string length, return the substring starting from maxIndex
        (substring s maxIndex))))

;; Example usage:
;; (lastSubstring "abab") ; => "bab"
;; (lastSubstring "leetcode") ; => "tcode"
(require rackunit)

(define (test-humaneval) 

  (let (( candidate lastSubstring))
    (check-within (candidate "bbbbaaaaaaaaf") "f" 0.001)
    (check-within (candidate "abab") "bab" 0.001)
    (check-within (candidate "data") "ta" 0.001)
    (check-within (candidate "coding") "oding" 0.001)
    (check-within (candidate "ecbacbac") "ecbacbac" 0.001)
    (check-within (candidate "bbaaaaaaaaaf") "f" 0.001)
    (check-within (candidate "cat") "t" 0.001)
    (check-within (candidate "abcab") "cab" 0.001)
    (check-within (candidate "zabcz") "zabcz" 0.001)
    (check-within (candidate "abcdefg") "g" 0.001)
    (check-within (candidate "abcdef") "f" 0.001)
    (check-within (candidate "aaaaaaaaa") "aaaaaaaaa" 0.001)
    (check-within (candidate "qpqpqpq") "qpqpqpq" 0.001)
    (check-within (candidate "a") "a" 0.001)
    (check-within (candidate "qqqqqq") "qqqqqq" 0.001)
    (check-within (candidate "qpqpqpqp") "qpqpqpqp" 0.001)
    (check-within (candidate "qpqpqp") "qpqpqp" 0.001)
    (check-within (candidate "abababab") "bababab" 0.001)
    (check-within (candidate "helloworld") "world" 0.001)
    (check-within (candidate "hbhbhbhbhbhbhbhb") "hbhbhbhbhbhbhbhb" 0.001)
    (check-within (candidate "aaaaaaaaaaf") "f" 0.001)
    (check-within (candidate "aaab") "b" 0.001)
    (check-within (candidate "python") "ython" 0.001)
    (check-within (candidate "leetcode") "tcode" 0.001)
    (check-within (candidate "abcabc") "cabc" 0.001)
    (check-within (candidate "aaa") "aaa" 0.001)
    (check-within (candidate "hello") "o" 0.001)
    (check-within (candidate "aaaaaaab") "b" 0.001)
    (check-within (candidate "aa") "aa" 0.001)
    (check-within (candidate "eee") "eee" 0.001)
    (check-within (candidate "cbda") "da" 0.001)
    (check-within (candidate "banana") "nana" 0.001)
    (check-within (candidate "bbbbaaaaaaaaaf") "f" 0.001)
    (check-within (candidate "zzzzz") "zzzzz" 0.001)
))

(test-humaneval)