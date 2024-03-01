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
  ;; Initialize an empty list to store the result
  (define ans '())
  ;; Initialize an empty list to store intervals with their heights
  (define intervals '())
  
  ;; Loop through each position in the input list
  (for-each (lambda (p)
              ;; Extract left and size from the current position
              (define L (car p))
              (define size (cadr p))
              (define R (+ L size))
              ;; Initialize height of the current square
              (define h size)
              
              ;; Calculate the height of the current square by comparing it with existing intervals
              (for-each (lambda (interval)
                          (define h2 (car interval))
                          (define R2 (cadr interval))
                          (when (and (> R2 L) (> R R2))
                            (set! h (max h (+ size h2)))))
                        intervals)
              
              ;; Calculate the max height so far
              (define maxHeight (if (null? intervals) 0 (apply max (map car intervals))))
              ;; Add the max height after dropping the current square to the ans list
              (set! ans (append ans (list (max maxHeight h))))
              ;; Add the current interval with its height to the intervals list
              (set! intervals (append intervals (list (list h R)))))
            positions)
  ;; Return the ans list which contains heights after each drop
  ans)

;; Example calls to the function
(fallingSquares '((1 2) (2 3) (6 1))) ; Output: '(2 5 5)
(fallingSquares '((100 100) (200 100))) ; Output: '(100 100)
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