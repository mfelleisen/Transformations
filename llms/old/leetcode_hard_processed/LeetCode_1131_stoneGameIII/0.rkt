#lang racket

;; Alice and Bob continue their games with piles of stones. There are several stones arranged in a row, and each stone has an associated value which is an integer given in the list stoneValue.
;; Alice and Bob take turns, with Alice starting first. On each player's turn, that player can take 1, 2, or 3 stones from the first remaining stones in the row.
;; The score of each player is the sum of the values of the stones taken. The score of each player is 0 initially.
;; The objective of the game is to end with the highest score, and the winner is the player with the highest score and there could be a tie. The game continues until all the stones have been taken.
;; Assume Alice and Bob play optimally.
;; Return "Alice" if Alice will win, "Bob" if Bob will win, or "Tie" if they will end the game with the same score.
;; Example 1:
;; Input: stoneValue = [1,2,3,7]
;; Output: "Bob"
;; Explanation: Alice will always lose. Her best move will be to take three piles and the score become 6. Now the score of Bob is 7 and Bob wins.
;; Example 2:
;; Input: stoneValue = [1,2,3,-9]
;; Output: "Alice"
;; Explanation: Alice must choose all the three piles at the first move to win and leave Bob with negative score.
;; If Alice chooses one pile her score will be 1 and the next move Bob's score becomes 5. In the next move, Alice will take the pile with value = -9 and lose.
;; If Alice chooses two piles her score will be 3 and the next move Bob's score becomes 3. In the next move, Alice will take the pile with value = -9 and also lose.
;; Remember that both play optimally so here Alice will choose the scenario that makes her win.
;; Example 3:
;; Input: stoneValue = [1,2,3,6]
;; Output: "Tie"
;; Explanation: Alice cannot win this game. She can end the game in a draw if she decided to choose all the first three piles, otherwise she will lose.
;; Constraints:
;; 1 <= stoneValue.length <= 5 * 104
;; -1000 <= stoneValue[i] <= 1000
(define (stoneGameIII stoneValue)
  (let ([n (length stoneValue)])
    (define dp (make-vector (add1 n) 0)) ; Create a dynamic programming vector dp of size n+1, initialized to 0.
    (vector-set! dp (- n 1) (list-ref stoneValue (- n 1))) ; Set the last element of dp to the last stone's value.
    (for ([i (in-range (- n 2) -1 -1)]) ; Iterate backwards from n-2 to 0 (inclusive).
      (let ([current-value (list-ref stoneValue i)]) ; Get the current stone's value.
        (vector-set! dp i (- current-value (vector-ref dp (add1 i)))) ; Set dp[i] to current-value - dp[i+1].
        (for ([j (in-range 1 3)]) ; For each choice of taking 1, 2, or 3 stones.
          (when (< (+ i j) n) ; Ensure we don't go out of bounds.
            (vector-set! dp i (max (vector-ref dp i) 
                                   (- (list-ref stoneValue (+ i j)) 
                                      (vector-ref dp (add1 (+ i j))))))))))
    ; Determine the winner based on dp[0].
    (cond [(> (vector-ref dp 0) 0) "Alice"]
          [(< (vector-ref dp 0) 0) "Bob"]
          [else "Tie"])))
(require rackunit)

(define (test-humaneval) 

  (let (( candidate stoneGameIII))
    (check-within (candidate (list 1 2 3 4)) "Bob" 0.001)
    (check-within (candidate (list 1 -1 1 -1 1 -1 1 -1 1)) "Alice" 0.001)
    (check-within (candidate (list 1 -2 3 6 5 -10 13 -3 -5 13 -5 -2 11 -6 -3 -1 3 -1 -4 -6 -3 11)) "Alice" 0.001)
    (check-within (candidate (list 1 2 3 7 -9 -8 1 2 3 7 -9 -8 1 2 3 7 -9 -8 1 2 3 7 -9 -8 1 2 3 7 -9 -8)) "Bob" 0.001)
    (check-within (candidate (list 1 2 3 0 0 0 0 0)) "Alice" 0.001)
    (check-within (candidate (list 1 2 3 -9)) "Alice" 0.001)
    (check-within (candidate (list 1)) "Alice" 0.001)
    (check-within (candidate (list 1 -1 2 -2 3 -3 4 -4 5 -5 6 -6 7 -7 8 -8 9 -9)) "Alice" 0.001)
    (check-within (candidate (list 1 -2 3 6 5 -10 13 -3 -5 13 -5 -2 11 -6 -3 -1 3 -1 -4 -6 -3 11 -4 -6 13 -6)) "Alice" 0.001)
    (check-within (candidate (list 1 2 3 -1 -2 -3 7)) "Alice" 0.001)
    (check-within (candidate (list 1 2 3 4 5)) "Alice" 0.001)
    (check-within (candidate (list 1 2 3 7)) "Bob" 0.001)
    (check-within (candidate (list 0 0 0 0 0 0 0 0)) "Tie" 0.001)
))

(test-humaneval)