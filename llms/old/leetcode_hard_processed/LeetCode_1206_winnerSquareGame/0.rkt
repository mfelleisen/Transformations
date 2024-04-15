#lang racket

;; Alice and Bob take turns playing a game, with Alice starting first.
;; Initially, there are n stones in a pile. On each player's turn, that player makes a move consisting of removing any non-zero square number of stones in the pile.
;; Also, if a player cannot make a move, he/she loses the game.
;; Given a positive integer n, return true if and only if Alice wins the game otherwise return false, assuming both players play optimally.
;; Example 1:
;; Input: n = 1
;; Output: true
;; Explanation: Alice can remove 1 stone winning the game because Bob doesn't have any moves.
;; Example 2:
;; Input: n = 2
;; Output: false
;; Explanation: Alice can only remove 1 stone, after that Bob removes the last one winning the game (2 -> 1 -> 0).
;; Example 3:
;; Input: n = 4
;; Output: true
;; Explanation: n is already a perfect square, Alice can win with one move, removing 4 stones (4 -> 0).
;; Constraints:
;; 1 <= n <= 105
(define (winnerSquareGame n)
  ;; Create a list to store dynamic programming results, initialized with #f (false) for each position.
  ;; The list size is n + 1 to accommodate 'n' elements plus a zero-th element.
  (define dp (make-vector (+ n 1) #f))
  
  ;; Iterate through each number from 1 to n to fill the dp vector with the outcomes of each possible state.
  (for ([i (in-range 1 (+ n 1))])
    (let loop ([j 1])
      ;; Calculate j^2 and check if it's less than or equal to i to ensure it's a valid move.
      (when (<= (* j j) i)
        ;; If removing j^2 stones from i results in a losing position for the opponent, mark this position as winning.
        (unless (vector-ref dp (- i (* j j)))
          (vector-set! dp i #t))
        ;; Continue checking with the next square number.
        (loop (+ j 1)))))
  
  ;; Return the result for 'n' stones, indicating if Alice can win.
  (vector-ref dp n))

;; Example usage:
(winnerSquareGame 1) ;; Returns: #t (true)
(winnerSquareGame 2) ;; Returns: #f (false)
(winnerSquareGame 4) ;; Returns: #t (true)
(require rackunit)

(define (test-humaneval) 

  (let (( candidate winnerSquareGame))
    (check-within (candidate 14) #t 0.001)
    (check-within (candidate 19) #t 0.001)
    (check-within (candidate 9999) #t 0.001)
    (check-within (candidate 7684) #t 0.001)
    (check-within (candidate 28) #t 0.001)
    (check-within (candidate 31) #t 0.001)
    (check-within (candidate 7) #f 0.001)
    (check-within (candidate 29) #t 0.001)
    (check-within (candidate 16) #t 0.001)
    (check-within (candidate 9) #t 0.001)
    (check-within (candidate 27) #t 0.001)
    (check-within (candidate 160) #t 0.001)
    (check-within (candidate 4) #t 0.001)
    (check-within (candidate 63) #t 0.001)
    (check-within (candidate 22) #f 0.001)
    (check-within (candidate 13) #t 0.001)
    (check-within (candidate 9997) #t 0.001)
    (check-within (candidate 1000) #t 0.001)
    (check-within (candidate 1) #t 0.001)
    (check-within (candidate 8) #t 0.001)
    (check-within (candidate 81) #t 0.001)
    (check-within (candidate 21) #t 0.001)
    (check-within (candidate 11) #t 0.001)
    (check-within (candidate 18) #t 0.001)
    (check-within (candidate 2685) #f 0.001)
    (check-within (candidate 2469) #t 0.001)
    (check-within (candidate 15) #f 0.001)
    (check-within (candidate 30) #t 0.001)
    (check-within (candidate 32) #t 0.001)
    (check-within (candidate 23) #t 0.001)
    (check-within (candidate 33) #t 0.001)
    (check-within (candidate 0) #f 0.001)
    (check-within (candidate 5) #f 0.001)
    (check-within (candidate 6) #t 0.001)
    (check-within (candidate 85868) #t 0.001)
    (check-within (candidate 24) #t 0.001)
    (check-within (candidate 17) #f 0.001)
    (check-within (candidate 3) #t 0.001)
    (check-within (candidate 2) #f 0.001)
    (check-within (candidate 26) #t 0.001)
    (check-within (candidate 25) #t 0.001)
    (check-within (candidate 3456) #t 0.001)
    (check-within (candidate 8876) #t 0.001)
))

(test-humaneval)