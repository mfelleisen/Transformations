#lang racket

;; With respect to a given puzzle string, a word is valid if both the following conditions are satisfied:
;; word contains the first letter of puzzle.
;; For each letter in word, that letter is in puzzle.
;; For example, if the puzzle is "abcdefg", then valid words are "faced", "cabbage", and "baggage", while
;; invalid words are "beefed" (does not include 'a') and "based" (includes 's' which is not in the puzzle).
;; Return a list answer, where answer[i] is the number of words in the given word list words that is valid with respect to the puzzle puzzles[i].
;; Example 1:
;; Input: words = ["aaaa","asas","able","ability","actt","actor","access"], puzzles = ["aboveyz","abrodyz","abslute","absoryz","actresz","gaswxyz"]
;; Output: [1,1,3,2,4,0]
;; Explanation: 
;; 1 valid word for "aboveyz" : "aaaa" 
;; 1 valid word for "abrodyz" : "aaaa"
;; 3 valid words for "abslute" : "aaaa", "asas", "able"
;; 2 valid words for "absoryz" : "aaaa", "asas"
;; 4 valid words for "actresz" : "aaaa", "asas", "actt", "access"
;; There are no valid words for "gaswxyz" cause none of the words in the list contains letter 'g'.
;; Example 2:
;; Input: words = ["apple","pleas","please"], puzzles = ["aelwxyz","aelpxyz","aelpsxy","saelpxy","xaelpsy"]
;; Output: [0,1,3,2,0]
;; Constraints:
;; 1 <= words.length <= 105
;; 4 <= words[i].length <= 50
;; 1 <= puzzles.length <= 104
;; puzzles[i].length == 7
;; words[i] and puzzles[i] consist of lowercase English letters.
;; Each puzzles[i] does not contain repeated characters.
(define (find_num_of_valid_words words puzzles)
  ;; Map the puzzle-solving function over all puzzles
  (map (lambda (puzzle)
         ;; Count the number of words that are valid for this puzzle
         (count (lambda (word)
                  ;; A word is valid if it contains the first letter of the puzzle
                  ;; and all its letters are in the puzzle
                  (and (member (string-ref puzzle 0) (string->list word))
                       (for/and ([char (in-list (string->list word))])
                         (member char (string->list puzzle)))))
                words))
       puzzles))

;; The function first maps over each puzzle. For each puzzle, it counts the number of words
;; that satisfy the conditions:
;; 1. The word must contain the first letter of the puzzle.
;; 2. All letters in the word must be present in the puzzle.
;; This is achieved by using `member` to check if the first letter of the puzzle is in the word
;; and `for/and` to ensure every character in the word is in the puzzle.
(require rackunit)

(define (test-humaneval) 

  (let (( candidate find_num_of_valid_words))
    (check-within (candidate (list "a" "b" "c" "d") (list "abcd" "dcba" "badc" "cdab" "e")) (list 1 1 1 1 0) 0.001)
    (check-within (candidate (list "apple" "pleas" "please") (list "aelwxyz" "dcba" "dcba" "dcba" "dcba" "dcba" "dcba" "dcba" "dcba" "dcba" "dcba" "dcba" "dcba" "dcba" "dcba" "dcba" "dcba" "dcba" "dcba" "dcba")) (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 0.001)
    (check-within (candidate (list "ab" "cd" "ef" "gh") (list "abcdefg" "abcdefgh" "badcfehg" "hgfedcba")) (list 1 1 1 1) 0.001)
    (check-within (candidate (list "aaaa" "asas" "able" "ability" "actt" "actor" "access") (list "aboveyz")) (list 1) 0.001)
    (check-within (candidate (list "aaaa" "asas" "able" "ability" "actt" "actor" "access") (list "aboveyz" "abrodyz" "abslute" "absoryz" "actresz" "gaswxyz")) (list 1 1 3 2 4 0) 0.001)
    (check-within (candidate (list "apple" "pleas" "please") (list "aelwxyz" "aelpxyz" "aelpsxy" "saelpxy" "xaelpsy")) (list 0 1 3 2 0) 0.001)
    (check-within (candidate (list "apple" "pleas" "please") (list "aelwxyz")) (list 0) 0.001)
    (check-within (candidate (list "apple") (list "apple" "pleas" "please")) (list 1 1 1) 0.001)
    (check-within (candidate (list "a" "b" "c" "d") (list "abcd" "dcba" "badc" "cdab")) (list 1 1 1 1) 0.001)
))

(test-humaneval)