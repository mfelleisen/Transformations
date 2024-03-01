#lang racket

;; A transformation sequence from word beginWord to word endWord using a hash wordList is a sequence of words beginWord -> s1 -> s2 -> ... -> sk such that:
;; Every adjacent pair of words differs by a single letter.
;; Every si for 1 <= i <= k is in wordList. Note that beginWord does not need to be in wordList.
;; sk == endWord
;; Given two words, beginWord and endWord, and a hash wordList, return all the shortest transformation sequences from beginWord to endWord, or an empty list if no such sequence exists. Each sequence should be returned as a list of the words [beginWord, s1, s2, ..., sk].
;; Example 1:
;; Input: beginWord = "hit", endWord = "cog", wordList = ["hot","dot","dog","lot","log","cog"]
;; Output: [["hit","hot","dot","dog","cog"],["hit","hot","lot","log","cog"]]
;; Explanation: There are 2 shortest transformation sequences:
;; "hit" -> "hot" -> "dot" -> "dog" -> "cog"
;; "hit" -> "hot" -> "lot" -> "log" -> "cog"
;; Example 2:
;; Input: beginWord = "hit", endWord = "cog", wordList = ["hot","dot","dog","lot","log"]
;; Output: []
;; Explanation: The endWord "cog" is not in wordList, therefore there is no valid transformation sequence.
;; Constraints:
;; 1 <= beginWord.length <= 5
;; endWord.length == beginWord.length
;; 1 <= wordList.length <= 500
;; wordList[i].length == beginWord.length
;; beginWord, endWord, and wordList[i] consist of lowercase English letters.
;; beginWord != endWord
;; All the words in wordList are unique.
;; The sum of all shortest transformation sequences does not exceed 105.
(define (findLadders beginWord endWord wordList)
  ;; Convert list to hash set for efficient lookup
  (define wordSet (list->set wordList))
  ;; Return empty list if endWord is not in wordSet
  (if (not (set-member? wordSet endWord))
      '()
      (let* ((adjacent (make-hash))
             (distance (make-hash))
             (queue (list beginWord)))
        (hash-set! distance beginWord 0)
        ;; Function to generate all possible neighbors of a word
        (define (neighbors word)
          (for*/list ((i (in-range (string-length word)))
                      (c (in-range 97 123))) ; ASCII values for 'a' to 'z'
            (string-append (substring word 0 i) 
                           (string (integer->char c)) 
                           (substring word (+ i 1)))))
        ;; BFS to find the shortest path and build the adjacency list
        (let loop ((q queue))
          (when (not (null? q))
            (let* ((current (car q))
                   (next-q (cdr q)))
              (when (not (equal? current endWord))
                (for-each (lambda (neighbor)
                            (when (and (set-member? wordSet neighbor)
                                       (or (not (hash-has-key? distance neighbor))
                                           (= (hash-ref distance neighbor) (+ 1 (hash-ref distance current)))))
                              (hash-set! distance neighbor (+ 1 (hash-ref distance current)))
                              (set! next-q (append next-q (list neighbor)))
                              (hash-update! adjacent current (lambda (lst) (cons neighbor lst)) '())))
                          (neighbors current)))
              (loop next-q))))
        ;; Backtracking to find all paths from beginWord to endWord
        (letrec ((result '())
                 (path (list beginWord))
                 (backtrack (lambda (word)
                              (when (equal? word endWord)
                                (set! result (cons (reverse path) result)))
                              (for-each (lambda (next-word)
                                          (set! path (cons next-word path))
                                          (backtrack next-word)
                                          (set! path (cdr path)))
                                        (hash-ref adjacent word '())))))
          (backtrack beginWord)
          result))))

;; Example usage
(findLadders "hit" "cog" '("hot" "dot" "dog" "lot" "log" "cog"))
(require rackunit)

(define (test-humaneval) 

  (let (( candidate findLadders))
    (check-within (candidate "qaaz" "qazz" (list "qazz")) (list (list "qaaz" "qazz")) 0.001)
    (check-within (candidate "abc" "def" (list "abc" "bbc" "dbc" "dec")) (list ) 0.001)
    (check-within (candidate "abc" "def" (list "def")) (list ) 0.001)
    (check-within (candidate "hit" "cog" (list "hot" "dot" "dog" "lot" "log")) (list ) 0.001)
    (check-within (candidate "a" "d" (list "a" "b" "c")) (list ) 0.001)
    (check-within (candidate "aaa" "bbb" (list "aaa" "aab" "abb" "bbb")) (list (list "aaa" "aab" "abb" "bbb")) 0.001)
    (check-within (candidate "hit" "cog" (list "hot" "dot" "dog" "lot" "log" "cog")) (list (list "hit" "hot" "dot" "dog" "cog") (list "hit" "hot" "lot" "log" "cog")) 0.001)
    (check-within (candidate "hot" "dog" (list "hot" "dot" "dog" "lot" "log")) (list (list "hot" "dot" "dog")) 0.001)
    (check-within (candidate "ant" "act" (list "ant" "act" "tct")) (list (list "ant" "act")) 0.001)
    (check-within (candidate "a" "c" (list "a" "b" "c")) (list (list "a" "c")) 0.001)
    (check-within (candidate "hot" "dot" (list "hot" "dot" "dog" "lot" "log" "cog")) (list (list "hot" "dot")) 0.001)
    (check-within (candidate "hit" "cog" (list "hot" "dot" "dog" "lot" "log" "cog")) (list (list "hit" "hot" "dot" "dog" "cog") (list "hit" "hot" "lot" "log" "cog")) 0.001)
    (check-within (candidate "abc" "abc" (list "abc")) (list (list "abc")) 0.001)
    (check-within (candidate "hot" "dog" (list "hot" "dog" "dot")) (list (list "hot" "dot" "dog")) 0.001)
    (check-within (candidate "hot" "dog" (list "hot" "dot" "dog" "lot" "log" "cog")) (list (list "hot" "dot" "dog")) 0.001)
))

(test-humaneval)