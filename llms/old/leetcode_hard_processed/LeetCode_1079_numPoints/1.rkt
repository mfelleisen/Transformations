#lang racket

;; Calculates the maximum number of darts that can lie on the dartboard.
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
  (let ([n (length darts)]) ; Get the total number of darts
    (let loop-outer ([i 0] [result 1]) ; Start with the first dart, result initialized to 1
      (if (< i n) ; If we haven't looked at all darts yet
          (let loop-inner ([j 0] [max-result result]) ; Inner loop to compare with every other dart
            (if (< j n) ; If we haven't compared with all darts yet
                (let ([cnt (for/sum ([k (in-range n)]) ; Count how many darts are within radius
                               (let* ([dx (- (first (list-ref darts i)) (first (list-ref darts k)))]
                                      [dy (- (second (list-ref darts i)) (second (list-ref darts k)))]
                                      [distance (sqrt (+ (* dx dx) (* dy dy)))]) ; Calculate distance
                                 (if (<= distance r) 1 0)))])
                  (loop-inner (+ j 1) (max max-result cnt))) ; Update the result if needed and continue
                (loop-outer (+ i 1) max-result))) ; Move to the next dart and carry the result
          result)))) ; Return the final result

;; Helper functions to get the first and second elements of a list
(define (first lst)
  (car lst))

(define (second lst)
  (cadr lst))
(require rackunit)

(define (test-humaneval) 

  (let (( candidate numPoints))
    (check-within (candidate (list (list 1 2) (list 3 4) (list 5 6) (list 7 8) (list 9 10)) 1) 1 0.001)
    (check-within (candidate (list (list 100 100) (list 200 200) (list 300 300) (list 400 400) (list 500 500)) 100) 1 0.001)
    (check-within (candidate (list (list 0 0) (list 1 1) (list 2 2) (list 3 3) (list 4 4)) 1) 1 0.001)
    (check-within (candidate (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5)) 5) 5 0.001)
))

(test-humaneval)