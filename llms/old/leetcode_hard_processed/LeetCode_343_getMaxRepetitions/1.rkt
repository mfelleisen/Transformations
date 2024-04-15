#lang racket

;; We define str = [s, n] as the string str which consists of the string s concatenated n times.
;; For example, str == ["abc", 3] =="abcabcabc".
;; We define that string s1 can be obtained from string s2 if we can remove some characters from s2 such that it becomes s1.
;; For example, s1 = "abc" can be obtained from s2 = "abdbec" based on our definition by removing the bolded underlined characters.
;; You are given two strings s1 and s2 and two integers n1 and n2. You have the two strings str1 = [s1, n1] and str2 = [s2, n2].
;; Return the maximum integer m such that str = [str2, m] can be obtained from str1.
;; Example 1:
;; Input: s1 = "acb", n1 = 4, s2 = "ab", n2 = 2
;; Output: 2
;; Example 2:
;; Input: s1 = "acb", n1 = 1, s2 = "acb", n2 = 1
;; Output: 1
;; Constraints:
;; 1 <= s1.length, s2.length <= 100
;; s1 and s2 consist of lowercase English letters.
;; 1 <= n1, n2 <= 106
(define (getMaxRepetitions s1 n1 s2 n2)
  (let loop ([count1 0] [count2 0] [i 0] [j 0])
    (if (< count1 n1)
        (let ([match? (equal? (string-ref s1 i) (string-ref s2 j))])
          (let* ([j (if match? (if (= j (- (string-length s2) 1)) 0 (+ j 1)) j)]
                 [count2 (if (and match? (= j 0)) (+ count2 1) count2)]
                 [i (if (= i (- (string-length s1) 1)) 0 (+ i 1))]
                 [count1 (if (= i 0) (+ count1 1) count1)])
            (loop count1 count2 i j)))
        (quotient count2 n2))))

;; The function getMaxRepetitions takes four arguments:
;; s1 and s2 are strings, n1 and n2 are integers indicating the number of times s1 and s2 are repeated, respectively.
;; It returns the maximum integer m such that str2 (s2 repeated n2 times) can be obtained m times from str1 (s1 repeated n1 times).
;; The function uses a loop with internal state (count1, count2, i, j) to track progress through s1 and s2,
;; counting how many times s2 can be found in s1 when both are repeated their specified number of times.
;; It iterates over s1 and s2, checking for matches and adjusting counters accordingly.
;; When the end of either string is reached, the respective counters and indices are reset or incremented.
;; Finally, the function returns the integer division of count2 by n2, which represents the maximum repetitions of s2 in s1.
(require rackunit)

(define (test-humaneval) 

  (let (( candidate getMaxRepetitions))
    (check-within (candidate "abcdabcdabcdabcdabcdabcd" 1 "abcdabcd" 3) 1 0.001)
    (check-within (candidate "abcd" 1 "ab" 2) 0 0.001)
    (check-within (candidate "abcd" 1 "abcd" 1) 1 0.001)
    (check-within (candidate "abc" 4 "ab" 2) 2 0.001)
    (check-within (candidate "a" 0 "a" 1) 0 0.001)
    (check-within (candidate "abcdabcd" 1 "abcd" 1) 2 0.001)
    (check-within (candidate "abc" 3 "abcabc" 3) 0 0.001)
    (check-within (candidate "abcabcabc" 5 "abc" 3) 5 0.001)
    (check-within (candidate "abcdefg" 6 "abc" 1) 6 0.001)
    (check-within (candidate "abcdefg" 5 "abc" 1) 5 0.001)
    (check-within (candidate "abcdefg" 3 "abc" 1) 3 0.001)
    (check-within (candidate "abcabc" 2 "abc" 3) 1 0.001)
    (check-within (candidate "abcd" 1 "a" 2) 0 0.001)
    (check-within (candidate "abca" 2 "abc" 2) 1 0.001)
    (check-within (candidate "abc" 3 "abc" 3) 1 0.001)
    (check-within (candidate "a" 100 "aa" 50) 1 0.001)
    (check-within (candidate "abcdefg" 2 "abc" 1) 2 0.001)
    (check-within (candidate "abcdefg" 4 "abc" 1) 4 0.001)
    (check-within (candidate "abcabc" 2 "abc" 2) 2 0.001)
    (check-within (candidate "bba" 1 "ab" 2) 0 0.001)
    (check-within (candidate "abc" 100 "a" 1) 100 0.001)
    (check-within (candidate "abcd" 1 "ab" 3) 0 0.001)
    (check-within (candidate "abcdefg" 1 "abcdefg" 1) 1 0.001)
    (check-within (candidate "abcdefg" 3 "abcdef" 3) 1 0.001)
    (check-within (candidate "ab" 2 "a" 1) 2 0.001)
    (check-within (candidate "abcabcabcabcabcabcabcabcabc" 1 "abcabcabcabcabcabcabcabcabc" 1) 1 0.001)
    (check-within (candidate "abcabcabcabc" 4 "abcabc" 2) 4 0.001)
    (check-within (candidate "bba" 1 "ab" 10) 0 0.001)
    (check-within (candidate "abcd" 1 "a" 4) 0 0.001)
    (check-within (candidate "abcd" 1 "a" 3) 0 0.001)
    (check-within (candidate "abc" 1 "abc" 1) 1 0.001)
    (check-within (candidate "abcabcabc" 1 "abc" 3) 1 0.001)
    (check-within (candidate "abcabcab" 3 "abc" 2) 3 0.001)
    (check-within (candidate "abcdabcdabcdabcdabcd" 1 "abcd" 5) 1 0.001)
    (check-within (candidate "abc" 5 "ab" 1) 5 0.001)
    (check-within (candidate "abc" 3 "abcabcabc" 1) 1 0.001)
    (check-within (candidate "abcdefg" 1 "abc" 1) 1 0.001)
    (check-within (candidate "b" 99 "aa" 1) 0 0.001)
    (check-within (candidate "abc" 3 "abc" 1) 3 0.001)
    (check-within (candidate "abcabcabcabcabcabc" 1 "abcabcabc" 2) 1 0.001)
    (check-within (candidate "a" 1 "a" 1) 1 0.001)
    (check-within (candidate "abcabc" 1 "abc" 1) 2 0.001)
    (check-within (candidate "abcdefg" 7 "abc" 1) 7 0.001)
    (check-within (candidate "s1" 1 "s2" 1) 0 0.001)
    (check-within (candidate "abcd" 1 "ab" 4) 0 0.001)
    (check-within (candidate "bba" 1 "ab" 1) 0 0.001)
))

(test-humaneval)