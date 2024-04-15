#lang racket

;; Alice is throwing n darts on a very large wall. You are given a list darts where darts[i] = [xi, yi] is the position of the ith dart that Alice threw on the wall.
;; Bob knows the positions of the n darts on the wall. He wants to place a dartboard of radius r on the wall so that the maximum number of darts that Alice throws lie on the dartboard.
;; Given the integer r, return the maximum number of darts that can lie on the dartboard.
;; Example 1:
;; Input: darts = [[-2,0],[2,0],[0,2],[0,-2]], r = 2
;; Output: 4
;; Explanation: Circle dartboard with center in (0,0) and radius = 2 contain all points.
;; Example 2:
;; Input: darts = [[-3,0],[3,0],[2,6],[5,4],[0,9],[7,8]], r = 5
;; Output: 5
;; Explanation: Circle dartboard with center in (0,4) and radius = 5 contain all points except the point (7,8).
;; Constraints:
;; 1 <= darts.length <= 100
;; darts[i].length == 2
;; -104 <= xi, yi <= 104
;; All the darts are unique
;; 1 <= r <= 5000
(define (numPoints darts r)
  ;; First, define the length of the darts list.
  (define n (length darts))
  ;; Initialize the result to 1 as there can be at least one dart on the board.
  (define result 1)
  ;; Iterate through each dart to use as a potential center.
  (for ([i (in-range n)])
    (for ([j (in-range n)])
      ;; Initialize a counter for each center position.
      (define cnt 0)
      ;; Check each dart to see if it lies within the radius of the dartboard centered at the ith dart.
      (for ([k (in-range n)])
        (define dx (- (list-ref (list-ref darts i) 0) (list-ref (list-ref darts k) 0)))
        (define dy (- (list-ref (list-ref darts i) 1) (list-ref (list-ref darts k) 1)))
        ;; If the distance from the center to the dart is less than or equal to r, increment cnt.
        (when (<= (sqrt (+ (* dx dx) (* dy dy))) r)
          (set! cnt (+ cnt 1))))
      ;; Update the result if the current count is greater than the previous result.
      (when (> cnt result)
        (set! result cnt))))
  ;; Return the maximum number of darts that can lie on the dartboard.
  result)

;; The function numPoints takes a list of darts' positions and a radius r,
;; and returns the maximum number of darts that can lie on a dartboard of radius r.
(require rackunit)

(define (test-humaneval) 

  (let (( candidate numPoints))
    (check-within (candidate (list (list 1 2) (list 3 4) (list 5 6) (list 7 8) (list 9 10)) 1) 1 0.001)
    (check-within (candidate (list (list 100 100) (list 200 200) (list 300 300) (list 400 400) (list 500 500)) 100) 1 0.001)
    (check-within (candidate (list (list 0 0) (list 1 1) (list 2 2) (list 3 3) (list 4 4)) 1) 1 0.001)
    (check-within (candidate (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5)) 5) 5 0.001)
))

(test-humaneval)