#lang racket

;; Your music player contains n different songs. You want to listen to goal songs (not necessarily different) during your trip. To avoid boredom, you will create a playlist so that:
;; Every song is played at least once.
;; A song can only be played again only if k other songs have been played.
;; Given n, goal, and k, return the number of possible playlists that you can create. Since the answer can be very large, return it modulo 109 + 7.
;; Example 1:
;; Input: n = 3, goal = 3, k = 1
;; Output: 6
;; Explanation: There are 6 possible playlists: [1, 2, 3], [1, 3, 2], [2, 1, 3], [2, 3, 1], [3, 1, 2], and [3, 2, 1].
;; Example 2:
;; Input: n = 2, goal = 3, k = 0
;; Output: 6
;; Explanation: There are 6 possible playlists: [1, 1, 2], [1, 2, 1], [2, 1, 1], [2, 2, 1], [2, 1, 2], and [1, 2, 2].
;; Example 3:
;; Input: n = 2, goal = 3, k = 1
;; Output: 2
;; Explanation: There are 2 possible playlists: [1, 2, 1] and [2, 1, 2].
;; Constraints:
;; 0 <= k < n <= goal <= 100
(define (numMusicPlaylists n goal k)
  (let ([MOD 1000000007]) ; Define the modulo as a constant
    (let ([dp (make-vector (add1 goal) #f)]) ; Initialize a vector for dynamic programming, length goal + 1
      ; Fill the dp vector with vectors of n+1 elements initialized to 0
      (for ([i (in-range (add1 goal))])
        (vector-set! dp i (make-vector (add1 n) 0)))
      ; Base case: 0 songs to achieve 0 goal
      (vector-set! (vector-ref dp 0) 0 1)
      ; Iterate over goals from 1 to goal, inclusive
      (for ([i (in-range 1 (add1 goal))])
        ; Iterate over number of unique songs from 1 to n, inclusive
        (for ([j (in-range 1 (add1 n))])
          ; Calculate the number of playlists using the formula and update the dp vector
          (let* ([val1 (vector-ref (vector-ref dp (sub1 i)) (sub1 j))]
                 [val2 (vector-ref (vector-ref dp (sub1 i)) j)]
                 [add-val (* val1 (modulo (- n (sub1 j)) MOD))]
                 [mult-val (* val2 (max 0 (- j k)))]
                 [sum (+ add-val mult-val)])
            (vector-set! (vector-ref dp i) j (modulo sum MOD)))))
      ; Return the final answer
      (vector-ref (vector-ref dp goal) n))))

; The function numMusicPlaylists calculates the number of possible playlists
; given the constraints that each song must be played at least once, and
; a song can only be replayed after k other songs have been played.
; It uses dynamic programming to efficiently solve this problem.
(require rackunit)

(define (test-humaneval) 

  (let (( candidate numMusicPlaylists))
    (check-within (candidate 6 6 1) 720 0.001)
    (check-within (candidate 3 3 1) 6 0.001)
    (check-within (candidate 2 2 1) 2 0.001)
    (check-within (candidate 10 1 3) 0 0.001)
    (check-within (candidate 4 4 1) 24 0.001)
    (check-within (candidate 10 10 1) 3628800 0.001)
    (check-within (candidate 10 10 0) 3628800 0.001)
    (check-within (candidate 1 1 0) 1 0.001)
    (check-within (candidate 4 4 0) 24 0.001)
    (check-within (candidate 1 1 1) 1 0.001)
    (check-within (candidate 1 1 10) 1 0.001)
    (check-within (candidate 3 3 2) 6 0.001)
    (check-within (candidate 10 10 4) 3628800 0.001)
    (check-within (candidate 6 6 0) 720 0.001)
    (check-within (candidate 10 10 2) 3628800 0.001)
    (check-within (candidate 5 5 2) 120 0.001)
    (check-within (candidate 2 2 10) 2 0.001)
    (check-within (candidate 10 10 5) 3628800 0.001)
    (check-within (candidate 1 1 2) 1 0.001)
    (check-within (candidate 10 10 10) 3628800 0.001)
    (check-within (candidate 5 5 0) 120 0.001)
    (check-within (candidate 10 10 7) 3628800 0.001)
    (check-within (candidate 2 2 2) 2 0.001)
    (check-within (candidate 10 10 8) 3628800 0.001)
    (check-within (candidate 10 10 3) 3628800 0.001)
    (check-within (candidate 2 2 0) 2 0.001)
    (check-within (candidate 2 3 0) 6 0.001)
    (check-within (candidate 3 3 0) 6 0.001)
    (check-within (candidate 10 10 9) 3628800 0.001)
    (check-within (candidate 10 10 6) 3628800 0.001)
))

(test-humaneval)