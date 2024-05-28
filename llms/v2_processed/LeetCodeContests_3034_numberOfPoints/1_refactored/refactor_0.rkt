#lang racket

;; You are given a 0-indexed 2D integer array nums representing the coordinates of the cars parking on a number line. For any index i, nums[i] = [starti, endi] where starti is the starting point of the ith car and endi is the ending point of the ith car.
;; Return the number of integer points on the line that are covered with any part of a car.
;; Example 1:
;; Input: nums = [[3,6],[1,5],[4,7]]
;; Output: 7
;; Explanation: All the points from 1 to 7 intersect at least one car, therefore the answer would be 7.
;; Example 2:
;; Input: nums = [[1,3],[5,8]]
;; Output: 7
;; Explanation: Points intersecting at least one car are 1, 2, 3, 5, 6, 7, 8. There are a total of 7 points, therefore the answer would be 7.
;; Constraints:
;;  * 1 <= nums.length <= 100
;;  * nums[i].length == 2
;;  * 1 <= starti <= endi <= 100
(define (numberOfPoints nums)
  ;; Use `for/set` to create a set of points covered by any car, avoiding duplicates automatically.
  (define covered-points
    (for*/set ([start-end nums]       ; For each pair in the input list `nums`
               [point (in-range (first start-end) (add1 (second start-end)))]) ; Generate range from start to end inclusive
      point)) ; Collect each point into a set
  
  ;; Return the number of unique points by getting the size of the set
  (set-count covered-points))

;; Example usage:
;; (numberOfPoints '((3 6) (1 5) (4 7))) ;; Expected output: 7
;; (numberOfPoints '((1 3) (5 8)))       ;; Expected output: 7

(require rackunit)


(define (test-humaneval) 

  (let (( candidate numberOfPoints))
    (check-within (candidate (list (list 3 6) (list 1 5) (list 4 7))) 7 0.001)
    (check-within (candidate (list (list 1 3) (list 5 8))) 7 0.001)
    (check-within (candidate (list (list 4 4) (list 9 10) (list 9 10) (list 3 8))) 8 0.001)
    (check-within (candidate (list (list 2 5) (list 3 8) (list 1 6) (list 4 10))) 10 0.001)
    (check-within (candidate (list (list 2 3) (list 3 9) (list 5 7) (list 4 10) (list 9 10))) 9 0.001)
    (check-within (candidate (list (list 4 10))) 7 0.001)
    (check-within (candidate (list (list 1 9) (list 2 10) (list 6 7) (list 8 9) (list 5 8) (list 1 3))) 10 0.001)
    (check-within (candidate (list (list 5 10) (list 3 8) (list 3 9))) 8 0.001)
    (check-within (candidate (list (list 2 3) (list 3 10) (list 5 8) (list 4 8) (list 2 7) (list 3 4) (list 3 10) (list 7 8))) 9 0.001)
    (check-within (candidate (list (list 1 3) (list 2 4) (list 6 6) (list 6 9) (list 2 10) (list 4 10) (list 3 6) (list 1 4) (list 1 3))) 10 0.001)
    (check-within (candidate (list (list 4 10) (list 3 9) (list 3 5) (list 4 10) (list 7 10) (list 1 7) (list 7 9) (list 4 8))) 10 0.001)
    (check-within (candidate (list (list 1 6) (list 6 7) (list 1 6) (list 1 3) (list 1 8) (list 2 9) (list 3 8) (list 1 9))) 9 0.001)
    (check-within (candidate (list (list 1 6) (list 8 10) (list 3 7) (list 6 10) (list 3 10) (list 1 10) (list 7 8))) 10 0.001)
    (check-within (candidate (list (list 6 8) (list 2 8) (list 3 9) (list 3 5) (list 6 10) (list 1 2) (list 5 5))) 10 0.001)
    (check-within (candidate (list (list 4 5) (list 5 9) (list 2 3) (list 5 10) (list 1 9) (list 1 8) (list 2 9) (list 2 10))) 10 0.001)
    (check-within (candidate (list (list 8 9) (list 6 7) (list 6 9) (list 3 5) (list 7 10) (list 5 9) (list 10 10))) 8 0.001)
    (check-within (candidate (list (list 6 8) (list 7 10) (list 9 10) (list 6 10) (list 1 10) (list 5 10))) 10 0.001)
    (check-within (candidate (list (list 9 9) (list 2 8) (list 5 8) (list 3 5) (list 2 2) (list 7 9) (list 5 10))) 9 0.001)
    (check-within (candidate (list (list 3 9) (list 5 9))) 7 0.001)
    (check-within (candidate (list (list 5 10) (list 2 3) (list 3 10) (list 4 7) (list 1 9) (list 5 10) (list 2 6) (list 1 7) (list 8 9) (list 2 9))) 10 0.001)
    (check-within (candidate (list (list 2 3) (list 2 3) (list 1 5))) 5 0.001)
    (check-within (candidate (list (list 4 7) (list 4 7))) 4 0.001)
    (check-within (candidate (list (list 7 9) (list 5 9) (list 2 10) (list 9 9) (list 5 8) (list 4 6) (list 6 7) (list 3 9) (list 2 4))) 9 0.001)
    (check-within (candidate (list (list 5 9) (list 7 7) (list 3 10) (list 7 9) (list 3 4) (list 1 1) (list 1 1) (list 1 7) (list 1 2) (list 6 6))) 10 0.001)
    (check-within (candidate (list (list 7 8) (list 1 7) (list 5 5) (list 4 4) (list 5 8) (list 2 6))) 8 0.001)
    (check-within (candidate (list (list 3 5) (list 8 8) (list 5 10) (list 1 7) (list 2 6) (list 7 10) (list 6 6) (list 5 9) (list 8 9) (list 5 6))) 10 0.001)
    (check-within (candidate (list (list 4 9))) 6 0.001)
    (check-within (candidate (list (list 2 7) (list 1 9) (list 5 6) (list 6 8) (list 1 10))) 10 0.001)
    (check-within (candidate (list (list 1 4) (list 2 4) (list 7 10) (list 2 8) (list 1 6) (list 1 10) (list 3 5))) 10 0.001)
    (check-within (candidate (list (list 1 4))) 4 0.001)
    (check-within (candidate (list (list 6 9) (list 4 7))) 6 0.001)
    (check-within (candidate (list (list 5 7))) 3 0.001)
    (check-within (candidate (list (list 1 9) (list 6 8) (list 4 7) (list 7 9) (list 8 9) (list 7 9) (list 4 6) (list 6 8) (list 4 9) (list 8 8))) 9 0.001)
    (check-within (candidate (list (list 3 6) (list 3 5) (list 1 9) (list 3 4) (list 3 8) (list 2 7) (list 3 8) (list 2 8))) 9 0.001)
    (check-within (candidate (list (list 2 5) (list 8 8) (list 1 6) (list 4 4) (list 4 5) (list 2 4))) 7 0.001)
    (check-within (candidate (list (list 4 7) (list 2 6))) 6 0.001)
    (check-within (candidate (list (list 5 8) (list 4 10) (list 2 9))) 9 0.001)
    (check-within (candidate (list (list 5 9) (list 2 4) (list 2 6))) 8 0.001)
    (check-within (candidate (list (list 2 3) (list 1 7) (list 1 8) (list 7 9) (list 1 5))) 9 0.001)
    (check-within (candidate (list (list 6 8) (list 6 7) (list 1 6) (list 2 10) (list 2 2) (list 6 8) (list 2 8) (list 8 9))) 10 0.001)
    (check-within (candidate (list (list 3 4) (list 2 5) (list 4 10) (list 3 6) (list 4 6) (list 1 8) (list 2 6) (list 6 9) (list 4 10) (list 3 6))) 10 0.001)
    (check-within (candidate (list (list 3 5) (list 2 5) (list 8 8))) 5 0.001)
    (check-within (candidate (list (list 5 8) (list 1 3) (list 8 8))) 7 0.001)
    (check-within (candidate (list (list 2 8) (list 5 7) (list 2 3) (list 2 7) (list 5 8) (list 1 10) (list 4 7) (list 10 10) (list 6 10))) 10 0.001)
    (check-within (candidate (list (list 1 3) (list 5 10) (list 3 10) (list 5 9))) 10 0.001)
    (check-within (candidate (list (list 4 10) (list 3 6))) 8 0.001)
    (check-within (candidate (list (list 7 8) (list 6 10) (list 7 8) (list 6 10) (list 7 10))) 5 0.001)
    (check-within (candidate (list (list 7 7) (list 4 4) (list 2 7) (list 2 3) (list 4 6) (list 4 8))) 7 0.001)
    (check-within (candidate (list (list 3 4) (list 1 4) (list 4 8) (list 1 7) (list 2 10) (list 8 10))) 10 0.001)
    (check-within (candidate (list (list 1 4) (list 7 10) (list 1 5) (list 8 9) (list 3 5) (list 3 8) (list 6 7) (list 3 5) (list 1 3) (list 2 8))) 10 0.001)
    (check-within (candidate (list (list 1 6) (list 5 10) (list 7 8) (list 7 10) (list 1 3))) 10 0.001)
    (check-within (candidate (list (list 2 3) (list 4 4) (list 2 7) (list 5 5) (list 4 7) (list 6 9) (list 2 4))) 8 0.001)
    (check-within (candidate (list (list 6 8) (list 6 8) (list 6 10))) 5 0.001)
    (check-within (candidate (list (list 3 10) (list 3 5) (list 2 3) (list 7 9))) 9 0.001)
    (check-within (candidate (list (list 4 4) (list 8 10) (list 2 7) (list 8 9) (list 1 8) (list 1 3) (list 1 9) (list 7 7) (list 3 6) (list 3 5))) 10 0.001)
    (check-within (candidate (list (list 2 6) (list 1 4) (list 3 8) (list 1 9))) 9 0.001)
    (check-within (candidate (list (list 1 2) (list 1 9) (list 2 9) (list 6 10) (list 3 5) (list 1 2))) 10 0.001)
    (check-within (candidate (list (list 6 7) (list 1 10) (list 4 4) (list 5 5) (list 5 10) (list 2 3) (list 2 8) (list 9 10))) 10 0.001)
    (check-within (candidate (list (list 1 1) (list 2 9) (list 3 3) (list 2 2) (list 2 4) (list 8 9) (list 3 9))) 9 0.001)
    (check-within (candidate (list (list 4 6) (list 1 10) (list 4 10) (list 1 10) (list 5 7))) 10 0.001)
    (check-within (candidate (list (list 2 3) (list 9 10) (list 2 9) (list 2 8) (list 8 9) (list 1 2))) 10 0.001)
    (check-within (candidate (list (list 4 9) (list 4 6) (list 2 7) (list 1 9) (list 6 10) (list 7 10) (list 3 9) (list 2 9))) 10 0.001)
    (check-within (candidate (list (list 7 10) (list 4 10) (list 4 10) (list 4 5) (list 3 10) (list 2 4) (list 8 9) (list 3 9) (list 4 5) (list 6 9))) 9 0.001)
    (check-within (candidate (list (list 2 7) (list 2 5) (list 3 3) (list 4 4) (list 5 6) (list 3 4) (list 4 10) (list 5 5) (list 4 5))) 9 0.001)
    (check-within (candidate (list (list 3 7) (list 7 8) (list 2 6) (list 10 10) (list 1 4))) 9 0.001)
    (check-within (candidate (list (list 3 4) (list 3 8) (list 5 8))) 6 0.001)
    (check-within (candidate (list (list 6 9) (list 1 8) (list 7 9))) 9 0.001)
    (check-within (candidate (list (list 7 8) (list 1 5))) 7 0.001)
    (check-within (candidate (list (list 5 10) (list 5 9) (list 5 6) (list 6 8) (list 1 5) (list 7 8) (list 3 5))) 10 0.001)
    (check-within (candidate (list (list 6 8))) 3 0.001)
    (check-within (candidate (list (list 5 5) (list 5 9) (list 2 8) (list 5 9) (list 5 6))) 8 0.001)
    (check-within (candidate (list (list 7 9) (list 3 8) (list 1 8) (list 8 8) (list 5 9) (list 1 3) (list 2 6))) 9 0.001)
    (check-within (candidate (list (list 3 6) (list 4 8) (list 7 9) (list 3 3) (list 9 10) (list 5 8) (list 1 2) (list 7 8) (list 3 10))) 10 0.001)
    (check-within (candidate (list (list 1 8) (list 4 5) (list 1 5) (list 6 7) (list 2 9))) 9 0.001)
    (check-within (candidate (list (list 6 8) (list 2 8) (list 6 9) (list 10 10) (list 2 5) (list 4 6) (list 1 10) (list 8 8) (list 9 10))) 10 0.001)
    (check-within (candidate (list (list 9 10) (list 4 8) (list 9 10) (list 5 7) (list 2 5) (list 2 7) (list 6 10) (list 5 7) (list 9 10))) 9 0.001)
    (check-within (candidate (list (list 1 7) (list 2 7) (list 2 4) (list 6 7))) 7 0.001)
    (check-within (candidate (list (list 2 10) (list 4 5) (list 4 10))) 9 0.001)
    (check-within (candidate (list (list 2 10) (list 3 6) (list 2 10) (list 4 10) (list 4 9) (list 10 10) (list 1 1))) 10 0.001)
    (check-within (candidate (list (list 3 5) (list 6 9) (list 4 7) (list 6 6) (list 4 5) (list 2 4) (list 2 7))) 8 0.001)
    (check-within (candidate (list (list 1 1) (list 1 7))) 7 0.001)
    (check-within (candidate (list (list 1 8) (list 2 8))) 8 0.001)
    (check-within (candidate (list (list 3 7))) 5 0.001)
    (check-within (candidate (list (list 1 6) (list 10 10) (list 5 7) (list 2 9))) 10 0.001)
    (check-within (candidate (list (list 7 8))) 2 0.001)
    (check-within (candidate (list (list 2 10) (list 1 10) (list 5 9) (list 7 7) (list 1 6) (list 3 5) (list 2 9) (list 2 10) (list 7 10))) 10 0.001)
    (check-within (candidate (list (list 3 8) (list 2 9) (list 6 10) (list 4 8) (list 3 4) (list 2 3) (list 5 9) (list 1 5) (list 7 9))) 10 0.001)
    (check-within (candidate (list (list 6 7) (list 1 5) (list 4 6) (list 4 9) (list 6 8) (list 1 7) (list 5 10) (list 3 4))) 10 0.001)
    (check-within (candidate (list (list 1 2) (list 4 10) (list 3 7) (list 2 10) (list 1 2) (list 3 4) (list 9 9) (list 5 9) (list 3 7) (list 3 5))) 10 0.001)
    (check-within (candidate (list (list 1 6) (list 3 4) (list 4 8) (list 8 10) (list 3 8))) 10 0.001)
    (check-within (candidate (list (list 3 6) (list 8 10) (list 2 5) (list 9 10) (list 2 8) (list 5 10) (list 7 10) (list 8 8) (list 8 10) (list 8 9))) 9 0.001)
    (check-within (candidate (list (list 1 8) (list 2 6) (list 2 3) (list 3 6) (list 1 10) (list 5 8))) 10 0.001)
    (check-within (candidate (list (list 3 7) (list 7 10) (list 6 6) (list 4 10) (list 5 10) (list 2 8) (list 1 10) (list 7 8) (list 6 6) (list 4 7))) 10 0.001)
    (check-within (candidate (list (list 6 9))) 4 0.001)
    (check-within (candidate (list (list 7 8) (list 1 1) (list 4 10) (list 1 9) (list 2 6) (list 4 6) (list 8 9) (list 4 5))) 10 0.001)
    (check-within (candidate (list (list 2 7) (list 7 10) (list 7 8) (list 3 5) (list 1 7) (list 1 4))) 10 0.001)
    (check-within (candidate (list (list 2 9))) 8 0.001)
    (check-within (candidate (list (list 7 9) (list 2 2) (list 2 7))) 8 0.001)
    (check-within (candidate (list (list 2 10) (list 8 9) (list 6 8) (list 9 10))) 9 0.001)
    (check-within (candidate (list (list 3 3))) 1 0.001)
))

(test-humaneval)