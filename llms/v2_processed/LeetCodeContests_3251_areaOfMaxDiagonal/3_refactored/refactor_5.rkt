#lang racket

;; You are given a 2D 0-indexed integer array dimensions.
;; For all indices i, 0 <= i < dimensions.length, dimensions[i][0] represents the length and dimensions[i][1] represents the width of the rectangle i.
;; Return the area of the rectangle having the longest diagonal. If there are multiple rectangles with the longest diagonal, return the area of the rectangle having the maximum area.
;; Example 1:
;; Input: dimensions = [[9,3],[8,6]]
;; Output: 48
;; Explanation: 
;; For index = 0, length = 9 and width = 3. Diagonal length = sqrt(9 * 9 + 3 * 3) = sqrt(90) ≈ 9.487.
;; For index = 1, length = 8 and width = 6. Diagonal length = sqrt(8 * 8 + 6 * 6) = sqrt(100) = 10.
;; So, the rectangle at index 1 has a greater diagonal length therefore we return area = 8 * 6 = 48.
;; Example 2:
;; Input: dimensions = [[3,4],[4,3]]
;; Output: 12
;; Explanation: Length of diagonal is the same for both which is 5, so maximum area = 12.
;; Constraints:
;; 1 <= dimensions.length <= 100
;; dimensions[i].length == 2
;; 1 <= dimensions[i][0], dimensions[i][1] <= 100
(define (areaOfMaxDiagonal dimensions)
  (define (diagonal-squared length width)
    (+ (* length length) (* width width)))

  (define (area length width)
    (* length width))

  (define (update-max acc dimension)
    (match-define (list length width) dimension)
    (define diag-sq (diagonal-squared length width))
    (define rect-area (area length width))
    (match-define (list max-diag-sq max-area) acc)
    (if (or (> diag-sq max-diag-sq)
            (and (= diag-sq max-diag-sq) (> rect-area max-area)))
        (list diag-sq rect-area)
        acc))

  (define initial-acc (list 0 0))

  (define final-acc
    (foldl update-max initial-acc dimensions))

  (second final-acc))

;; Example usage:
;; (areaOfMaxDiagonal '((9 3) (8 6))) ; Should return 48
;; (areaOfMaxDiagonal '((3 4) (4 3))) ; Should return 12

(require rackunit)


(define (test-humaneval) 

  (let (( candidate areaOfMaxDiagonal))
    (check-within (candidate (list (list 9 3) (list 8 6))) 48 0.001)
    (check-within (candidate (list (list 3 4) (list 4 3))) 12 0.001)
    (check-within (candidate (list (list 4 10) (list 4 9) (list 9 3) (list 10 8))) 80 0.001)
    (check-within (candidate (list (list 2 6) (list 5 1) (list 3 10) (list 8 4))) 30 0.001)
    (check-within (candidate (list (list 3 7) (list 2 10) (list 3 4) (list 9 9) (list 5 10))) 81 0.001)
    (check-within (candidate (list (list 10 4))) 40 0.001)
    (check-within (candidate (list (list 9 9) (list 1 8) (list 10 5) (list 2 8) (list 6 3) (list 7 1))) 81 0.001)
    (check-within (candidate (list (list 10 3) (list 5 9) (list 8 3))) 30 0.001)
    (check-within (candidate (list (list 2 7) (list 3 2) (list 3 3) (list 10 4) (list 5 3) (list 8 10) (list 8 8) (list 4 7))) 80 0.001)
    (check-within (candidate (list (list 1 10) (list 3 10) (list 4 4) (list 2 6) (list 6 3) (list 6 4) (list 9 1) (list 6 1) (list 2 3))) 30 0.001)
    (check-within (candidate (list (list 4 7) (list 10 10) (list 3 7) (list 9 1) (list 5 7) (list 3 9) (list 10 4) (list 4 8))) 100 0.001)
    (check-within (candidate (list (list 1 1) (list 6 8) (list 6 9) (list 7 2) (list 6 8) (list 1 3) (list 3 1) (list 1 9))) 54 0.001)
    (check-within (candidate (list (list 6 6) (list 1 3) (list 8 10) (list 10 1) (list 3 10) (list 7 7) (list 10 8))) 80 0.001)
    (check-within (candidate (list (list 6 5) (list 8 6) (list 2 10) (list 8 1) (list 9 2) (list 3 5) (list 3 5))) 20 0.001)
    (check-within (candidate (list (list 5 1) (list 4 9) (list 9 1) (list 5 8) (list 2 9) (list 3 2) (list 10 10) (list 5 2))) 100 0.001)
    (check-within (candidate (list (list 8 3) (list 9 10) (list 7 7) (list 6 5) (list 6 9) (list 9 10) (list 5 10))) 90 0.001)
    (check-within (candidate (list (list 6 10) (list 8 6) (list 10 1) (list 7 10) (list 10 10) (list 9 5))) 100 0.001)
    (check-within (candidate (list (list 9 5) (list 9 2) (list 2 2) (list 8 9) (list 5 7) (list 8 10) (list 3 5))) 80 0.001)
    (check-within (candidate (list (list 3 9) (list 9 5))) 45 0.001)
    (check-within (candidate (list (list 10 10) (list 5 5) (list 3 2) (list 2 6) (list 3 1) (list 10 7) (list 4 8) (list 7 9) (list 9 9) (list 1 2))) 100 0.001)
    (check-within (candidate (list (list 2 3) (list 3 5) (list 2 1))) 15 0.001)
    (check-within (candidate (list (list 4 4) (list 7 7))) 49 0.001)
    (check-within (candidate (list (list 7 5) (list 9 6) (list 9 4) (list 5 7) (list 2 6) (list 10 3) (list 9 9) (list 9 4) (list 8 2))) 81 0.001)
    (check-within (candidate (list (list 5 1) (list 9 1) (list 7 1) (list 7 1) (list 3 1) (list 10 7) (list 9 1) (list 7 2) (list 4 6) (list 3 6))) 70 0.001)
    (check-within (candidate (list (list 8 4) (list 7 4) (list 1 5) (list 7 8) (list 5 6) (list 5 2))) 56 0.001)
    (check-within (candidate (list (list 5 10) (list 3 7) (list 8 6) (list 8 6) (list 5 9) (list 10 5) (list 7 8) (list 1 9) (list 2 5) (list 6 6))) 50 0.001)
    (check-within (candidate (list (list 9 4))) 36 0.001)
    (check-within (candidate (list (list 7 6) (list 2 8) (list 9 6) (list 1 10) (list 5 1))) 54 0.001)
    (check-within (candidate (list (list 4 2) (list 1 6) (list 2 1) (list 4 10) (list 10 1) (list 7 5) (list 8 3))) 40 0.001)
    (check-within (candidate (list (list 1 4))) 4 0.001)
    (check-within (candidate (list (list 9 4) (list 6 7))) 36 0.001)
    (check-within (candidate (list (list 7 5))) 35 0.001)
    (check-within (candidate (list (list 1 9) (list 9 7) (list 8 4) (list 6 6) (list 7 8) (list 4 6) (list 7 4) (list 9 9) (list 9 8) (list 8 8))) 81 0.001)
    (check-within (candidate (list (list 3 8) (list 6 3) (list 5 2) (list 3 7) (list 1 3) (list 9 8) (list 4 2) (list 3 8))) 72 0.001)
    (check-within (candidate (list (list 5 4) (list 2 4) (list 8 5) (list 8 4) (list 1 2) (list 6 4))) 40 0.001)
    (check-within (candidate (list (list 7 2) (list 4 6))) 14 0.001)
    (check-within (candidate (list (list 8 10) (list 5 2) (list 4 9))) 80 0.001)
    (check-within (candidate (list (list 9 2) (list 5 6) (list 4 2))) 18 0.001)
    (check-within (candidate (list (list 3 8) (list 2 9) (list 7 7) (list 1 5) (list 1 1))) 49 0.001)
    (check-within (candidate (list (list 6 2) (list 8 2) (list 6 8) (list 7 6) (list 1 2) (list 6 8) (list 10 9) (list 2 8))) 90 0.001)
    (check-within (candidate (list (list 3 8) (list 4 1) (list 5 2) (list 2 6) (list 4 9) (list 10 6) (list 6 10) (list 3 4) (list 6 6) (list 4 3))) 60 0.001)
    (check-within (candidate (list (list 5 5) (list 3 8) (list 2 8))) 24 0.001)
    (check-within (candidate (list (list 8 1) (list 5 8) (list 3 8))) 40 0.001)
    (check-within (candidate (list (list 2 8) (list 8 1) (list 7 10) (list 5 7) (list 2 4) (list 3 10) (list 2 10) (list 7 10) (list 5 6))) 70 0.001)
    (check-within (candidate (list (list 3 10) (list 1 3) (list 10 5) (list 5 9))) 50 0.001)
    (check-within (candidate (list (list 10 6) (list 4 3))) 60 0.001)
    (check-within (candidate (list (list 7 8) (list 8 6) (list 10 10) (list 6 7) (list 7 10))) 100 0.001)
    (check-within (candidate (list (list 7 2) (list 7 3) (list 4 6) (list 4 4) (list 7 8) (list 2 4))) 56 0.001)
    (check-within (candidate (list (list 4 7) (list 3 1) (list 1 10) (list 4 2) (list 4 10) (list 8 8))) 64 0.001)
    (check-within (candidate (list (list 1 8) (list 4 3) (list 7 7) (list 10 6) (list 5 5) (list 1 3) (list 9 1) (list 8 3) (list 3 2) (list 5 8))) 60 0.001)
    (check-within (candidate (list (list 6 7) (list 1 7) (list 5 10) (list 10 1) (list 8 3))) 50 0.001)
    (check-within (candidate (list (list 3 5) (list 2 7) (list 4 4) (list 4 9) (list 7 6) (list 2 4) (list 5 2))) 36 0.001)
    (check-within (candidate (list (list 8 8) (list 6 10) (list 6 6))) 60 0.001)
    (check-within (candidate (list (list 10 2) (list 3 3) (list 5 9) (list 3 7))) 45 0.001)
    (check-within (candidate (list (list 4 3) (list 4 1) (list 8 9) (list 10 1) (list 2 7) (list 7 7) (list 9 3) (list 8 6) (list 1 5) (list 8 3))) 72 0.001)
    (check-within (candidate (list (list 6 8) (list 2 3) (list 4 9) (list 1 1))) 48 0.001)
    (check-within (candidate (list (list 1 6) (list 2 10) (list 1 5) (list 9 3) (list 9 1) (list 2 2))) 20 0.001)
    (check-within (candidate (list (list 6 5) (list 7 10) (list 1 2) (list 10 3) (list 4 2) (list 4 8) (list 5 10) (list 5 9))) 70 0.001)
    (check-within (candidate (list (list 1 2) (list 1 2) (list 2 4) (list 9 9) (list 3 8) (list 3 9) (list 2 3))) 81 0.001)
    (check-within (candidate (list (list 4 4) (list 6 1) (list 1 10) (list 10 7) (list 10 5))) 70 0.001)
    (check-within (candidate (list (list 3 2) (list 2 8) (list 10 9) (list 9 8) (list 2 2) (list 9 1))) 90 0.001)
    (check-within (candidate (list (list 4 10) (list 9 6) (list 4 10) (list 6 7) (list 2 3) (list 7 9) (list 9 2) (list 1 9))) 63 0.001)
    (check-within (candidate (list (list 7 4) (list 10 2) (list 10 8) (list 4 9) (list 4 9) (list 10 3) (list 5 4) (list 4 5) (list 10 6) (list 3 9))) 80 0.001)
    (check-within (candidate (list (list 2 5) (list 7 4) (list 5 3) (list 2 4) (list 3 10) (list 3 5) (list 4 5) (list 4 4) (list 6 5))) 30 0.001)
    (check-within (candidate (list (list 3 2) (list 7 10) (list 8 10) (list 7 4) (list 6 1))) 80 0.001)
    (check-within (candidate (list (list 3 8) (list 4 5) (list 3 8))) 24 0.001)
    (check-within (candidate (list (list 6 8) (list 9 9) (list 1 7))) 81 0.001)
    (check-within (candidate (list (list 8 1) (list 7 5))) 35 0.001)
    (check-within (candidate (list (list 10 6) (list 5 1) (list 9 5) (list 5 7) (list 5 8) (list 6 5) (list 8 3))) 60 0.001)
    (check-within (candidate (list (list 8 6))) 48 0.001)
    (check-within (candidate (list (list 5 2) (list 5 9) (list 9 5) (list 5 5) (list 8 6))) 45 0.001)
    (check-within (candidate (list (list 7 8) (list 9 9) (list 3 5) (list 8 1) (list 1 3) (list 8 2) (list 8 6))) 81 0.001)
    (check-within (candidate (list (list 3 10) (list 6 8) (list 4 5) (list 8 1) (list 7 2) (list 9 8) (list 3 7) (list 3 3) (list 9 10))) 90 0.001)
    (check-within (candidate (list (list 1 1) (list 8 7) (list 4 6) (list 5 2) (list 5 9))) 56 0.001)
    (check-within (candidate (list (list 6 2) (list 8 4) (list 8 6) (list 2 10) (list 6 1) (list 9 8) (list 10 8) (list 10 10) (list 5 9))) 100 0.001)
    (check-within (candidate (list (list 10 2) (list 9 7) (list 4 2) (list 8 6) (list 9 10) (list 10 7) (list 7 5) (list 5 10) (list 5 9))) 90 0.001)
    (check-within (candidate (list (list 1 4) (list 7 2) (list 2 6) (list 7 7))) 49 0.001)
    (check-within (candidate (list (list 2 5) (list 10 10) (list 4 4))) 100 0.001)
    (check-within (candidate (list (list 2 10) (list 10 4) (list 3 9) (list 6 10) (list 2 10) (list 10 1) (list 4 1))) 60 0.001)
    (check-within (candidate (list (list 3 6) (list 5 4) (list 9 5) (list 6 2) (list 4 4) (list 7 2) (list 6 7))) 45 0.001)
    (check-within (candidate (list (list 1 1) (list 1 7))) 7 0.001)
    (check-within (candidate (list (list 1 2) (list 8 8))) 64 0.001)
    (check-within (candidate (list (list 3 7))) 21 0.001)
    (check-within (candidate (list (list 6 7) (list 1 5) (list 10 9) (list 10 2))) 90 0.001)
    (check-within (candidate (list (list 7 8))) 56 0.001)
    (check-within (candidate (list (list 2 6) (list 10 3) (list 10 5) (list 1 9) (list 5 2) (list 9 10) (list 7 2) (list 7 7) (list 1 10))) 90 0.001)
    (check-within (candidate (list (list 3 4) (list 8 2) (list 9 3) (list 2 9) (list 6 5) (list 10 5) (list 4 1) (list 8 7) (list 3 9))) 50 0.001)
    (check-within (candidate (list (list 7 6) (list 6 8) (list 5 7) (list 1 1) (list 4 5) (list 6 10) (list 9 3) (list 4 4))) 60 0.001)
    (check-within (candidate (list (list 1 3) (list 2 4) (list 4 9) (list 10 9) (list 3 9) (list 7 5) (list 2 3) (list 10 7) (list 2 3) (list 1 5))) 90 0.001)
    (check-within (candidate (list (list 1 8) (list 6 10) (list 4 8) (list 3 8) (list 4 3))) 60 0.001)
    (check-within (candidate (list (list 6 5) (list 3 10) (list 8 7) (list 10 10) (list 2 8) (list 5 8) (list 10 8) (list 9 10) (list 2 8) (list 8 9))) 100 0.001)
    (check-within (candidate (list (list 1 6) (list 8 3) (list 6 1) (list 2 10) (list 2 5) (list 3 8))) 20 0.001)
    (check-within (candidate (list (list 7 2) (list 3 8) (list 10 10) (list 7 1) (list 6 8) (list 6 7) (list 10 6) (list 4 6) (list 5 7) (list 10 4))) 100 0.001)
    (check-within (candidate (list (list 9 6))) 54 0.001)
    (check-within (candidate (list (list 8 2) (list 7 6) (list 1 4) (list 1 6) (list 4 8) (list 10 9) (list 9 4) (list 1 5))) 90 0.001)
    (check-within (candidate (list (list 7 3) (list 2 5) (list 7 1) (list 10 7) (list 7 4) (list 8 1))) 70 0.001)
    (check-within (candidate (list (list 9 2))) 18 0.001)
    (check-within (candidate (list (list 9 2) (list 7 2) (list 2 7))) 18 0.001)
    (check-within (candidate (list (list 2 8) (list 10 6) (list 8 10) (list 9 9))) 80 0.001)
    (check-within (candidate (list (list 3 3))) 9 0.001)
))

(test-humaneval)