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
  (let ([MOD 1000000007])
    ;; Initialize a 2D list (matrix) for dynamic programming.
    (define dp (make-matrix (add1 goal) (add1 n) 0))
    ;; Base case: 0 songs chosen for 0 goals.
    (matrix-set! dp 0 0 1)
    ;; Iterate through goals (i) and number of distinct songs (j).
    (for ([i (in-range 1 (add1 goal))])
      (for ([j (in-range 1 (add1 n))])
        (matrix-set! dp i j
                     (modulo
                      (+ (* (matrix-ref dp (sub1 i) (sub1 j)) (modulo (- n (sub1 j)) MOD))
                         (* (matrix-ref dp (sub1 i) j) (max 0 (- j k))))
                      MOD))))
    (matrix-ref dp goal n)))

;; Helper functions to work with matrices.
(define (make-matrix rows cols fill)
  (for/list ([r (in-range rows)])
    (make-vector cols fill)))

(define (matrix-ref m r c)
  (vector-ref (list-ref m r) c))

(define (matrix-set! m r c val)
  (vector-set! (list-ref m r) c val))

;; Examples
(numMusicPlaylists 3 3 1) ; Should return 6
(numMusicPlaylists 2 3 0) ; Should return 6
(numMusicPlaylists 2 3 1) ; Should return 2
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