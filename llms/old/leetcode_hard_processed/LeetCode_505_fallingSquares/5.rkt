#lang racket

;; There are several squares being dropped onto the X-axis of a 2D plane.
;; You are given a 2D integer list positions where positions[i] = [lefti, sideLengthi] represents the ith square with a side length of sideLengthi that is dropped with its left edge aligned with X-coordinate lefti.
;; Each square is dropped one at a time from a height above any landed squares. It then falls downward (negative Y direction) until it either lands on the top side of another square or on the X-axis. A square brushing the left/right side of another square does not count as landing on it. Once it lands, it freezes in place and cannot be moved.
;; After each square is dropped, you must record the height of the current tallest stack of squares.
;; Return an integer list ans where ans[i] represents the height described above after dropping the ith square.
;; Example 1:
;; Input: positions = [[1,2],[2,3],[6,1]]
;; Output: [2,5,5]
;; Explanation:
;; After the first drop, the tallest stack is square 1 with a height of 2.
;; After the second drop, the tallest stack is squares 1 and 2 with a height of 5.
;; After the third drop, the tallest stack is still squares 1 and 2 with a height of 5.
;; Thus, we return an answer of [2, 5, 5].
;; Example 2:
;; Input: positions = [[100,100],[200,100]]
;; Output: [100,100]
;; Explanation:
;; After the first drop, the tallest stack is square 1 with a height of 100.
;; After the second drop, the tallest stack is either square 1 or square 2, both with heights of 100.
;; Thus, we return an answer of [100, 100].
;; Note that square 2 only brushes the right side of square 1, which does not count as landing on it.
;; Constraints:
;; 1 <= positions.length <= 1000
;; 1 <= lefti <= 108
;; 1 <= sideLengthi <= 106
(define (fallingSquares positions)
  ;; Initialize an empty list to store the results
  (let loop ((positions positions) (ans '()) (intervals '()))
    ;; Base case: if there are no more positions, return the reversed list of answers
    (if (null? positions)
        (reverse ans)
        ;; Process each position
        (let* ((p (car positions)) ;; Current position
               (L (car p)) ;; Left edge of the current square
               (size (cadr p)) ;; Size of the current square
               (R (+ L size)) ;; Right edge of the current square
               ;; Calculate the height where the square will land
               (h (foldl (lambda (interval h)
                           (let ((h2 (car interval)) (R2 (cadr interval)))
                             (if (and (> R2 L) (< R2 R))
                                 (max h (+ size h2))
                                 h)))
                         size
                         intervals))
               ;; Calculate the maximum height so far
               (maxHeight (if (null? intervals) 0
                              (apply max (map car intervals)))))
          ;; Recurse with the rest of the positions, updated answers, and intervals
          (loop (cdr positions)
                (cons (max maxHeight h) ans)
                (cons (list h R) intervals))))))

;; Example usage:
(fallingSquares '((1 2) (2 3) (6 1)))
(fallingSquares '((100 100) (200 100)))
(require rackunit)

(define (test-humaneval) 

  (let (( candidate fallingSquares))
    (check-within (candidate (list (list 1 2) (list 5 3) (list 3 1))) (list 2 3 3) 0.001)
    (check-within (candidate (list (list 1 2) (list 2 2) (list 3 2))) (list 2 4 6) 0.001)
    (check-within (candidate (list (list 1 2) (list 2 2) (list 2 1))) (list 2 4 4) 0.001)
    (check-within (candidate (list (list 1 1))) (list 1) 0.001)
    (check-within (candidate (list (list 1 2) (list 2 3) (list 6 1))) (list 2 5 5) 0.001)
    (check-within (candidate (list (list 1 2) (list 3 2) (list 5 2))) (list 2 2 2) 0.001)
    (check-within (candidate (list (list 1 2) (list 2 2) (list 1 2))) (list 2 4 4) 0.001)
    (check-within (candidate (list (list 100 100) (list 200 100))) (list 100 100) 0.001)
))

(test-humaneval)