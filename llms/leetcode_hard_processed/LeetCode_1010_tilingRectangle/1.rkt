#lang racket
;; Given a rectangle of size n x m, return the minimum number of integer-sided squares that tile the rectangle.
;; Given a rectangle of size n x m, return the minimum number of integer-sided squares that tile the rectangle.
;; Example 1:
;; Input: n = 2, m = 3
;; Output: 3
;; Explanation: 3 squares are necessary to cover the rectangle.
;; 2 (squares of 1x1)
;; 1 (square of 2x2)
;; Example 2:
;; Input: n = 5, m = 8
;; Output: 5
;; Example 3:
;; Input: n = 11, m = 13
;; Output: 6
;; Constraints:
;; 1 <= n, m <= 13
(define (tilingRectangle n m)
  ;; Ensure that n is always the smaller dimension to simplify the problem.
  (let ([n (min n m)]
        [m (max n m)])
    (cond
      ;; Base case: If one side is 1, the entire area can be covered by 1x1 squares.
      [(= n 1) m]
      ;; Base case: If the rectangle is a square, only one square is needed.
      [(= n m) 1]
      ;; Recursive case: Try dividing the rectangle into smaller rectangles
      ;; and find the minimum number of squares needed.
      [else
       (let loop ([res +inf.0] [i 1])
         (cond
           ;; Loop through all possible splits by height
           [(<= i (/ n 2))
            (let ([new-res (min res (+ (tilingRectangle i m) (tilingRectangle (- n i) m)))])
              (loop new-res (+ i 1)))]
           ;; After trying all height splits, start with width splits
           [else
            (let loop-width ([res res] [j 1])
              (if (<= j (/ m 2))
                  (let ([new-res (min res (+ (tilingRectangle n j) (tilingRectangle n (- m j))))])
                    (loop-width new-res (+ j 1)))
                  res))]))])))

;; Examples:
(tilingRectangle 2 3) ;; Output: 3
(tilingRectangle 5 8) ;; Output: 5
(tilingRectangle 11 13) ;; Output: 6
(require rackunit)

(define (test-humaneval) 

  (let (( candidate tilingRectangle))
    (check-within (candidate 2 3) 3 0.001)
    (check-within (candidate 1 4) 4 0.001)
    (check-within (candidate 1 50) 50 0.001)
    (check-within (candidate 48 1) 48 0.001)
    (check-within (candidate 35 35) 1 0.001)
    (check-within (candidate 30 30) 1 0.001)
    (check-within (candidate 34 34) 1 0.001)
    (check-within (candidate 20 20) 1 0.001)
    (check-within (candidate 15 5) 3 0.001)
    (check-within (candidate 5 5) 1 0.001)
    (check-within (candidate 1 6) 6 0.001)
    (check-within (candidate 39 39) 1 0.001)
    (check-within (candidate 13 13) 1 0.001)
    (check-within (candidate 8 7) 7 0.001)
    (check-within (candidate 1 58) 58 0.001)
    (check-within (candidate 18 18) 1 0.001)
    (check-within (candidate 42 42) 1 0.001)
    (check-within (candidate 6 7) 5 0.001)
    (check-within (candidate 56 1) 56 0.001)
    (check-within (candidate 5 1) 5 0.001)
    (check-within (candidate 4 2) 2 0.001)
    (check-within (candidate 36 36) 1 0.001)
    (check-within (candidate 1 20) 20 0.001)
    (check-within (candidate 37 37) 1 0.001)
    (check-within (candidate 10 10) 1 0.001)
    (check-within (candidate 16 16) 1 0.001)
    (check-within (candidate 55 55) 1 0.001)
    (check-within (candidate 26 26) 1 0.001)
    (check-within (candidate 7 1) 7 0.001)
    (check-within (candidate 17 17) 1 0.001)
    (check-within (candidate 1 7) 7 0.001)
    (check-within (candidate 1 5) 5 0.001)
    (check-within (candidate 30 1) 30 0.001)
    (check-within (candidate 29 1) 29 0.001)
    (check-within (candidate 12 12) 1 0.001)
    (check-within (candidate 24 24) 1 0.001)
    (check-within (candidate 2 2) 1 0.001)
    (check-within (candidate 10 1) 10 0.001)
    (check-within (candidate 40 40) 1 0.001)
    (check-within (candidate 38 1) 38 0.001)
    (check-within (candidate 1 3) 3 0.001)
    (check-within (candidate 15 15) 1 0.001)
    (check-within (candidate 4 1) 4 0.001)
    (check-within (candidate 41 41) 1 0.001)
    (check-within (candidate 28 1) 28 0.001)
    (check-within (candidate 29 29) 1 0.001)
    (check-within (candidate 1 2) 2 0.001)
    (check-within (candidate 1 55) 55 0.001)
    (check-within (candidate 1 10) 10 0.001)
    (check-within (candidate 1 40) 40 0.001)
    (check-within (candidate 2 4) 2 0.001)
    (check-within (candidate 14 14) 1 0.001)
    (check-within (candidate 8 8) 1 0.001)
    (check-within (candidate 27 27) 1 0.001)
    (check-within (candidate 19 19) 1 0.001)
    (check-within (candidate 3 2) 3 0.001)
    (check-within (candidate 7 5) 5 0.001)
    (check-within (candidate 6 4) 3 0.001)
    (check-within (candidate 31 31) 1 0.001)
    (check-within (candidate 15 1) 15 0.001)
    (check-within (candidate 7 3) 5 0.001)
    (check-within (candidate 25 25) 1 0.001)
    (check-within (candidate 3 3) 1 0.001)
    (check-within (candidate 47 47) 1 0.001)
    (check-within (candidate 1 30) 30 0.001)
    (check-within (candidate 32 32) 1 0.001)
    (check-within (candidate 1 59) 59 0.001)
    (check-within (candidate 1 1) 1 0.001)
    (check-within (candidate 21 21) 1 0.001)
))

(test-humaneval)