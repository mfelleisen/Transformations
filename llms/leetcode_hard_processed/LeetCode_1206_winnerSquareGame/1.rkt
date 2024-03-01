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
  ;; Create a vector (similar to Python's list) of length n+1 filled with #f (False)
  (define dp (make-vector (+ n 1) #f))
  
  ;; Iterate from 1 to n
  (for ([i (in-range 1 (+ n 1))])
    ;; Initialize j to 1 for each iteration of i
    (let loop ([j 1])
      ;; Check if j*j is less than or equal to i (similar to while loop in Python)
      (when (<= (* j j) i)
        ;; Check if the current state dp[i-j*j] is False
        (unless (vector-ref dp (- i (* j j)))
          ;; If it is False, set dp[i] to True, because Alice can win by leaving dp[i-j*j] state for Bob
          (vector-set! dp i #t))
        ;; Increment j and continue the loop
        (loop (+ j 1)))))
  ;; Return the last element of the vector dp[n] which indicates if Alice can win or not
  (vector-ref dp n))

;; Examples
(winnerSquareGame 1) ;; Output: #t (true)
(winnerSquareGame 2) ;; Output: #f (false)
(winnerSquareGame 4) ;; Output: #t (true)
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