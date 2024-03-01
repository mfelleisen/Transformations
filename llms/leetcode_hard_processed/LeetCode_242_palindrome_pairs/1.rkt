#lang racket

;; You are given a 0-indexed list of unique strings words.
;; A palindrome pair is a pair of integers (i, j) such that:
;; 0 <= i, j < words.length,
;; i != j, and
;; words[i] + words[j] (the concatenation of the two strings) is a palindrome.
;; Return a list of all the palindrome pairs of words.
;; You must write an algorithm with O(sum of words[i].length) runtime complexity.
;; Example 1:
;; Input: words = ["abcd","dcba","lls","s","sssll"]
;; Output: [[0,1],[1,0],[3,2],[2,4]]
;; Explanation: The palindromes are ["abcddcba","dcbaabcd","slls","llssssll"]
;; Example 2:
;; Input: words = ["bat","tab","cat"]
;; Output: [[0,1],[1,0]]
;; Explanation: The palindromes are ["battab","tabbat"]
;; Example 3:
;; Input: words = ["a",""]
;; Output: [[0,1],[1,0]]
;; Explanation: The palindromes are ["a","a"]
;; Constraints:
;; 1 <= words.length <= 5000
;; 0 <= words[i].length <= 300
;; words[i] consists of lowercase English letters.
(define (palindrome_pairs words)
  ;; Define a helper function to check if a string is a palindrome.
  (define (is-palindrome? s)
    (equal? s (list->string (reverse (string->list s)))))
  
  ;; Define a helper function to generate all unique pairs of indices from the list,
  ;; skipping pairs with the same indices.
  (define (generate-pairs n)
    (for*/list ((i (in-range n))
                (j (in-range n))
                #:when (not (= i j)))
      (list i j)))
  
  ;; Use the helper functions to find and return all palindrome pairs.
  (filter-map
   (lambda (pair)
     (let* ((i (first pair))
            (j (second pair))
            (word1 (list-ref words i))
            (word2 (list-ref words j))
            (concat (string-append word1 word2)))
       (if (is-palindrome? concat)
           pair
           #f))) ; Filter out non-palindrome pairs
   (generate-pairs (length words))))

;; The function `palindrome_pairs` takes a list of words and returns all unique pairs
;; of indices (i, j) such that the concatenation of words[i] and words[j] forms a palindrome,
;; ensuring i != j.
(require rackunit)

(define (test-humaneval) 

  (let (( candidate palindrome_pairs))
    (check-within (candidate (list "a" "")) (list (list 0 1) (list 1 0)) 0.001)
    (check-within (candidate (list "ab" "ba" "cd" "dc")) (list (list 0 1) (list 1 0) (list 2 3) (list 3 2)) 0.001)
    (check-within (candidate (list "abcd" "dcba" "lls" "s" "sssll")) (list (list 0 1) (list 1 0) (list 2 4) (list 3 2)) 0.001)
    (check-within (candidate (list "x" "y" "z")) (list ) 0.001)
    (check-within (candidate (list "abc" "def" "abc" "cba")) (list (list 0 3) (list 2 3) (list 3 0) (list 3 2)) 0.001)
    (check-within (candidate (list "radar" "dar" "")) (list (list 0 2) (list 2 0)) 0.001)
    (check-within (candidate (list "abc" "def" "ghi" "jkl")) (list ) 0.001)
    (check-within (candidate (list "bat" "tab" "cat")) (list (list 0 1) (list 1 0)) 0.001)
))

(test-humaneval)