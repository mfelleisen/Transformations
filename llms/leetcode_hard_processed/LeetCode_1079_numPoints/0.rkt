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
  ;; Calculate the distance between two points
  (define (distance p1 p2)
    (sqrt (+ (sqr (- (first p1) (first p2)))
             (sqr (- (second p1) (second p2))))))
  ;; Initialize the result with 1 since a dartboard of radius r can contain at least one dart
  (let ([n (length darts)])
    (define result 1)
    ;; Iterate over all pairs of darts to find the maximum number of darts within distance r
    (for ([i (in-range n)])
      (for ([j (in-range n)])
        (let ([cnt 0])
          (for ([k (in-range n)])
            ;; Calculate the distance from the dart at index i to the dart at index k
            (when (<= (distance (list-ref darts i) (list-ref darts k)) r)
              (set! cnt (add1 cnt))))
          ;; Update the result if the current count is greater than the previous result
          (when (> cnt result)
            (set! result cnt)))))
    result))

;; The function `numPoints` takes a list of dart positions and a radius `r`, then calculates
;; the maximum number of darts that can lie on a dartboard of radius `r`.
(require rackunit)

(define (test-humaneval) 

  (let (( candidate numPoints))
    (check-within (candidate (list (list 1 2) (list 3 4) (list 5 6) (list 7 8) (list 9 10)) 1) 1 0.001)
    (check-within (candidate (list (list 100 100) (list 200 200) (list 300 300) (list 400 400) (list 500 500)) 100) 1 0.001)
    (check-within (candidate (list (list 0 0) (list 1 1) (list 2 2) (list 3 3) (list 4 4)) 1) 1 0.001)
    (check-within (candidate (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5)) 5) 5 0.001)
))

(test-humaneval)