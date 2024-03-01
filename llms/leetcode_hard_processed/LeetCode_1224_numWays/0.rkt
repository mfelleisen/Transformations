#lang racket

;; Function to calculate the number of ways to form the target string from the given words.
;; You are given a list of strings of the same length words and a string target.
;; Your task is to form target using the given words under the following rules:
;; target should be formed from left to right.
;; To form the ith character (0-indexed) of target, you can choose the kth character of the jth string in words if target[i] = words[j][k].
;; Once you use the kth character of the jth string of words, you can no longer use the xth character of any string in words where x <= k. In other words, all characters to the left of or at index k become unusuable for every string.
;; Repeat the process until you form the string target.
;; Notice that you can use multiple characters from the same string in words provided the conditions above are met.
;; Return the number of ways to form target from words. Since the answer may be too large, return it modulo 109 + 7.
;; Example 1:
;; Input: words = ["acca","bbbb","caca"], target = "aba"
;; Output: 6
;; Explanation: There are 6 ways to form target.
;; "aba" -> index 0 ("acca"), index 1 ("bbbb"), index 3 ("caca")
;; "aba" -> index 0 ("acca"), index 2 ("bbbb"), index 3 ("caca")
;; "aba" -> index 0 ("acca"), index 1 ("bbbb"), index 3 ("acca")
;; "aba" -> index 0 ("acca"), index 2 ("bbbb"), index 3 ("acca")
;; "aba" -> index 1 ("caca"), index 2 ("bbbb"), index 3 ("acca")
;; "aba" -> index 1 ("caca"), index 2 ("bbbb"), index 3 ("caca")
;; Example 2:
;; Input: words = ["abba","baab"], target = "bab"
;; Output: 4
;; Explanation: There are 4 ways to form target.
;; "bab" -> index 0 ("baab"), index 1 ("baab"), index 2 ("abba")
;; "bab" -> index 0 ("baab"), index 1 ("baab"), index 3 ("baab")
;; "bab" -> index 0 ("baab"), index 2 ("baab"), index 3 ("baab")
;; "bab" -> index 1 ("abba"), index 2 ("baab"), index 3 ("baab")
;; Constraints:
;; 1 <= words.length <= 1000
;; 1 <= words[i].length <= 1000
;; All strings in words have the same length.
;; 1 <= target.length <= 1000
;; words[i] and target contain only lowercase English letters.
(define (numWays words target)
  ;; Define the modulus value for the result.
  (define mod 1000000007)
  ;; Calculate the number of words and the length of the target.
  (define n (length words))
  (define m (string-length target))
  ;; Initialize the count matrix to store the count of each character in each word.
  (define count (for/list ([i (in-range n)])
                  (make-vector 26 0)))
  ;; Initialize the dp matrix for dynamic programming.
  (define dp (for/list ([i (in-range m)])
                (make-vector n 0)))
  ;; Fill the count matrix with the frequency of each character in each word.
  (for ([word (in-list words)] [i (in-range n)])
    (for ([c (in-string word)])
      (vector-set! (list-ref count i) (- (char->integer c) (char->integer #\a))
                   (+ 1 (vector-ref (list-ref count i) (- (char->integer c) (char->integer #\a)))))))
  ;; Initialize the first row of the dp matrix.
  (for ([i (in-range n)])
    (vector-set! (list-ref dp 0) i
                 (vector-ref (list-ref count i) (- (char->integer (string-ref target 0)) (char->integer #\a)))))
  ;; Fill the dp matrix.
  (for ([i (in-range 1 m)])
    (for ([j (in-range n)])
      (for ([k (in-range n)])
        (vector-set! (list-ref dp i) j
                     (modulo (+ (vector-ref (list-ref dp i) j)
                                (* (vector-ref (list-ref dp (- i 1)) k)
                                   (vector-ref (list-ref count j) (- (char->integer (string-ref target i)) (char->integer #\a)))))
                               mod)))))
  ;; Calculate and return the sum of the last row of the dp matrix modulo mod.
  (modulo (apply + (vector->list (list-ref dp (- m 1)))) mod))

;; Example usage:
(define words '("acca" "bbbb" "caca"))
(define target "aba")
(numWays words target)  ; => 6

(define words2 '("abba" "baab"))
(define target2 "bab")
(numWays words2 target2)  ; => 4
(require rackunit)

(define (test-humaneval) 

  (let (( candidate numWays))
    (check-within (candidate (list "abc" "def" "ghi") "def") 1 0.001)
    (check-within (candidate (list "abcd" "efgh" "ijkl" "mnop" "qrst" "uvwx" "yz") "qrst") 1 0.001)
    (check-within (candidate (list "abc" "def" "ghi") "ghi") 1 0.001)
    (check-within (candidate (list "abc" "def" "ghi") "de") 1 0.001)
    (check-within (candidate (list "abcd") "abcd") 1 0.001)
    (check-within (candidate (list "abc" "def" "ghi") "xyz") 0 0.001)
    (check-within (candidate (list "a" "a" "a" "a" "a" "a") "eeee") 0 0.001)
    (check-within (candidate (list "abcd") "ab") 1 0.001)
    (check-within (candidate (list "abc" "def" "ghi") "abc") 1 0.001)
    (check-within (candidate (list "aaa" "aaa" "aaa") "abcc") 0 0.001)
))

(test-humaneval)