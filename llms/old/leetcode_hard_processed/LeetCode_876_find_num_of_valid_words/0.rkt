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
  ;; Map over each puzzle in the list of puzzles
  (map (lambda (puzzle)
         ;; Initialize count for each puzzle
         (let ([count 0])
           ;; Convert puzzle string into a list of characters for easy manipulation
           (let ([puzzle-chars (string->list puzzle)])
             ;; Filter words that contain the first letter of the puzzle
             (let ([filtered-words (filter (lambda (word) (member (car puzzle-chars) (string->list word))) words)])
               ;; For each filtered word, check if all its letters are in the puzzle
               (for ([word filtered-words])
                 (when (andmap (lambda (char) (member char puzzle-chars)) (string->list word))
                   ;; If all letters of the word are in the puzzle, increment count
                   (set! count (add1 count))))
               ;; Return the count for the current puzzle
               count))))
       puzzles))

;; Example usage
(find_num_of_valid_words '("aaaa" "asas" "able" "ability" "actt" "actor" "access")
                         '("aboveyz" "abrodyz" "abslute" "absoryz" "actresz" "gaswxyz"))
;; Output: '(1 1 3 2 4 0)
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