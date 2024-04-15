#lang racket

;; Function to find all words that can be constructed from a board
;; Given an m x n board of characters and a list of strings words, return all words on the board.
;; Each word must be constructed from letters of sequentially adjacent cells, where adjacent cells are horizontally or vertically neighboring. The same letter cell may not be used more than once in a word.
;; Example 1:
;; Input: board = [["o","a","a","n"],["e","t","a","e"],["i","h","k","r"],["i","f","l","v"]], words = ["oath","pea","eat","rain"]
;; Output: ["eat","oath"]
;; Example 2:
;; Input: board = [["a","b"],["c","d"]], words = ["abcb"]
;; Output: []
;; Constraints:
;; m == board.length
;; n == board[i].length
;; 1 <= m, n <= 12
;; board[i][j] is a lowercase English letter.
;; 1 <= words.length <= 3 * 104
;; 1 <= words[i].length <= 10
;; words[i] consists of lowercase English letters.
;; All the strings of words are unique.
(define (findWords board words)
  ;; Helper function for depth-first search
  (define (dfs board word index i j visited)
    ;; Base case: if the whole word is found
    (if (= index (string-length word))
        #t
        ;; Check bounds and if the cell has been visited or doesn't match the letter in the word
        (and (>= i 0) (< i (length board))
             (>= j 0) (< j (length (list-ref board 0)))
             (not (list-ref (list-ref visited i) j))
             (char=? (string-ref word index) (string-ref (list-ref (list-ref board i) j) 0))
             (let ((visited (update-visited visited i j #t))) ; Mark the cell as visited
               (or (dfs board word (+ index 1) (- i 1) j visited)
                   (dfs board word (+ index 1) (+ i 1) j visited)
                   (dfs board word (+ index 1) i (- j 1) visited)
                   (dfs board word (+ index 1) i (+ j 1) visited)
                   (begin
                     (update-visited visited i j #f) ; Reset the visited status before backtrack
                     #f))))))
  
  ;; Function to update the visited status of a cell
  (define (update-visited visited i j status)
    (map (λ (row idx) (if (= idx i)
                          (map (λ (cell idy) (if (= idy j) status cell))
                               row (build-list (length row) values))
                          row))
         visited (build-list (length visited) values)))

  ;; Main logic to find words in the board
  (define (find-words-helper words)
    (filter
     (λ (word)
       (ormap (λ (i)
                (ormap (λ (j)
                         (dfs board word 0 i j (map (λ (_) (make-list (length (list-ref board 0)) #f))
                                                    (build-list (length board) values))))
                       (build-list (length (list-ref board 0)) values)))
              (build-list (length board) values)))
     words))

  ;; Call the helper function with the provided words
  (find-words-helper words))

;; Example usage
(findWords '(("o" "a" "a" "n") ("e" "t" "a" "e") ("i" "h" "k" "r") ("i" "f" "l" "v"))
            '("oath" "pea" "eat" "rain"))
;; Expected output: '("eat" "oath")
(require rackunit)

(define (test-humaneval) 

  (let (( candidate findWords))
    (check-within (candidate (list (list "a" "b") (list "c" "d")) (list "ac" "bd")) (list "ac" "bd") 0.001)
    (check-within (candidate (list (list "a" "b") (list "c" "d")) (list "abcd")) (list ) 0.001)
    (check-within (candidate (list (list "o" "a" "b" "n") (list "o" "t" "a" "e") (list "a" "h" "k" "r") (list "a" "f" "l" "v")) (list "oa" "oaa")) (list "oa" "oaa") 0.001)
    (check-within (candidate (list (list "a")) (list "a")) (list "a") 0.001)
    (check-within (candidate (list (list "a" "b" "c") (list "d" "e" "f") (list "g" "h" "i")) (list "aei" "bdf" "cgh")) (list ) 0.001)
    (check-within (candidate (list (list "a" "a" "a") (list "a" "a" "a") (list "a" "a" "a")) (list "aaa" "aaaa" "aaaaa")) (list "aaa" "aaaa" "aaaaa") 0.001)
    (check-within (candidate (list (list "a" "b" "c") (list "d" "e" "f") (list "g" "h" "i")) (list "a" "e" "i")) (list "a" "e" "i") 0.001)
    (check-within (candidate (list (list "a" "b" "c") (list "d" "e" "f") (list "g" "h" "i")) (list "adg" "beh" "cfi")) (list "adg" "beh" "cfi") 0.001)
    (check-within (candidate (list (list "a" "b") (list "c" "d")) (list "abcb")) (list ) 0.001)
))

(test-humaneval)