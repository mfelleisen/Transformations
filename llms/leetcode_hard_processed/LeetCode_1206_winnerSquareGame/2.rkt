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
  ;; Define a mutable hash table to memoize intermediate results
  (define dp (make-hash))
  
  ;; Helper function to determine if the current player can win
  (define (can-win? n)
    ;; Check if the result for this `n` is already computed
    (match (hash-ref dp n #f)
      ;; If the result is found in the hash table, return it
      ('#f (let loop ((j 1))
             (if (> (* j j) n) 
                 ;; If j*j exceeds n, the current player loses, memoize and return false
                 (begin
                   (hash-set! dp n #f)
                   #f)
                 ;; If subtracting j*j from n leaves a losing position for the opponent,
                 ;; current player wins, memoize and return true
                 (if (not (can-win? (- n (* j j))))
                     (begin
                       (hash-set! dp n #t)
                       #t)
                     (loop (+ j 1))))))
      ;; Otherwise, return the memoized result
      (result result)))
  
  ;; Start the game with `n` stones
  (can-win? n))

;; Example usage:
(winnerSquareGame 1) ; Returns: #t (true)
(winnerSquareGame 2) ; Returns: #f (false)
(winnerSquareGame 4) ; Returns: #t (true)
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