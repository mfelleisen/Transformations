#lang racket

;; Function to check if cyclic shifts on matrix rows result in the same matrix
;; You are given a 0-indexed m x n integer matrix mat and an integer k. You have to cyclically right shift odd indexed rows k times and cyclically left shift even indexed rows k times.
;; Return true if the initial and final matrix are exactly the same and false otherwise.
;; Example 1:
;; Input: mat = [[1,2,1,2],[5,5,5,5],[6,3,6,3]], k = 2
;; Output: true
;; Explanation:
;; Initially, the matrix looks like the first figure. 
;; Second figure represents the state of the matrix after one right and left cyclic shifts to even and odd indexed rows.
;; Third figure is the final state of the matrix after two cyclic shifts which is similar to the initial matrix.
;; Therefore, return true.
;; Example 2:
;; Input: mat = [[2,2],[2,2]], k = 3
;; Output: true
;; Explanation: As all the values are equal in the matrix, even after performing cyclic shifts the matrix will remain the same. Therefeore, we return true.
;; Example 3:
;; Input: mat = [[1,2]], k = 1
;; Output: false
;; Explanation: After one cyclic shift, mat = [[2,1]] which is not equal to the initial matrix. Therefore we return false.
;; Constraints:
;; 1 <= mat.length <= 25
;; 1 <= mat[i].length <= 25
;; 1 <= mat[i][j] <= 25
;; 1 <= k <= 50
(define (areSimilar mat k)
  ;; Helper function to perform cyclic shift
  (define (cyclic-shift row shifts)
    (define len (length row))
    (define mod-shifts (modulo shifts len))
    (append (drop-right row mod-shifts) (take-right row mod-shifts)))

  ;; Function to apply appropriate shift depending on the row index
  (define (process-row idx row)
    (if (even? idx)
        (cyclic-shift row k)
        (cyclic-shift row (- (length row) k))))

  ;; Transform the matrix by applying shifts
  (define transformed-mat
    (map-indexed process-row mat))
  
  ;; Compare initial and final matrices
  (equal? mat transformed-mat))

;; The function `map-indexed` is not built-in in Racket, so we define it using `map` and `in-indexed`
(define (map-indexed f lst)
  (map (lambda (idx item) (f idx item))
       (in-range (length lst))
       lst))

(require rackunit)


(define (test-humaneval) 

  (let (( candidate areSimilar))
    (check-within (candidate (list (list 1 2)) 1) #f 0.001)
    (check-within (candidate (list (list 1 2 1 2) (list 5 5 5 5) (list 6 3 6 3)) 2) #t 0.001)
    (check-within (candidate (list (list 4 9 10 10) (list 9 3 8 4) (list 2 5 3 8) (list 6 1 10 4)) 5) #f 0.001)
    (check-within (candidate (list (list 5 8 8 4 7 2 3 4 3 10) (list 8 7 9 1 3 4 2 6 6 9) (list 6 2 10 10 4 6 3 4 1 1)) 3) #f 0.001)
    (check-within (candidate (list (list 4 7 9 1 10 5 2 6 1 7) (list 8 9 9 2 3 2 3 2 3 5) (list 1 2 4 7 4 7 9 7 9 9)) 5) #f 0.001)
    (check-within (candidate (list (list 10 6 3 6) (list 4 8 1 2)) 6) #f 0.001)
    (check-within (candidate (list (list 7 10 6 7 7 4 4 7 2 2) (list 3 6 4 8 4 6 4 3 1 4) (list 4 8 7 1 10 2 10 8 10 1) (list 4 7 10 5 1 9 8 3 5 8) (list 3 7 6 5 3 1 3 2 8 5) (list 6 1 5 10 8 7 7 10 1 3)) 7) #f 0.001)
    (check-within (candidate (list (list 6 5 3) (list 4 6 2) (list 4 1 8) (list 3 9 1) (list 6 1 2) (list 1 9 9) (list 2 6 10)) 5) #f 0.001)
    (check-within (candidate (list (list 2 4) (list 9 8)) 9) #f 0.001)
    (check-within (candidate (list (list 2 2) (list 2 2)) 3) #t 0.001)
    (check-within (candidate (list (list 9 1 10 6 10 7 3) (list 9 2 9 10 7 10 10)) 4) #f 0.001)
    (check-within (candidate (list (list 7 7) (list 10 10) (list 4 4)) 2) #t 0.001)
    (check-within (candidate (list (list 6 6 6 6 6 6 6 6 6 6) (list 9 9 9 9 9 9 9 9 9 9) (list 1 1 1 1 1 1 1 1 1 1) (list 10 10 10 10 10 10 10 10 10 10) (list 2 2 2 2 2 2 2 2 2 2) (list 6 6 6 6 6 6 6 6 6 6) (list 7 7 7 7 7 7 7 7 7 7) (list 9 9 9 9 9 9 9 9 9 9) (list 8 8 8 8 8 8 8 8 8 8)) 1) #t 0.001)
    (check-within (candidate (list (list 6 9 1) (list 8 9 7) (list 2 8 7) (list 1 5 7) (list 10 5 9) (list 5 5 6) (list 8 6 1) (list 5 7 8)) 5) #f 0.001)
    (check-within (candidate (list (list 3 10 3 10 3 10 3 10) (list 5 8 5 8 5 8 5 8) (list 3 9 3 9 3 9 3 9) (list 3 8 3 8 3 8 3 8) (list 2 3 2 3 2 3 2 3)) 2) #t 0.001)
    (check-within (candidate (list (list 9 5 3 10) (list 4 7 10 7) (list 1 7 9 4) (list 8 8 1 6) (list 6 7 6 1) (list 3 1 1 8) (list 9 2 8 3) (list 1 9 7 6)) 4) #t 0.001)
    (check-within (candidate (list (list 4 6) (list 10 1) (list 8 8) (list 10 9) (list 9 10)) 9) #f 0.001)
    (check-within (candidate (list (list 1 9 6 7 1 4 7 6 7) (list 7 10 6 6 4 9 6 8 2) (list 3 9 8 10 9 9 3 9 5) (list 8 5 2 3 4 7 3 3 1) (list 1 5 9 9 6 1 9 7 5) (list 8 3 10 2 4 8 7 9 9) (list 5 9 6 8 4 3 4 6 4) (list 7 2 6 9 2 4 5 4 9) (list 4 8 7 5 3 6 3 9 5)) 1) #f 0.001)
    (check-within (candidate (list (list 9 3 3 7 7 5 3 3) (list 10 9 9 3 6 8 7 5) (list 8 9 3 10 10 10 2 1) (list 9 7 8 2 3 4 8 4) (list 5 9 5 2 2 6 5 7) (list 1 5 9 7 8 1 1 1)) 10) #f 0.001)
    (check-within (candidate (list (list 10 6 10 6 10 6 10 6)) 4) #t 0.001)
    (check-within (candidate (list (list 2 4) (list 6 1) (list 1 2) (list 2 10) (list 6 5) (list 4 9)) 7) #f 0.001)
    (check-within (candidate (list (list 8 8 8 8 8 8 8 8 8 8) (list 7 7 7 7 7 7 7 7 7 7) (list 6 6 6 6 6 6 6 6 6 6)) 2) #t 0.001)
    (check-within (candidate (list (list 8 10 1 7 1 3 9 6 8) (list 9 10 4 8 8 9 3 10 10) (list 4 3 2 2 3 6 4 6 1) (list 9 4 1 4 5 2 5 1 8) (list 3 10 6 3 8 4 8 3 10)) 7) #f 0.001)
    (check-within (candidate (list (list 8 9) (list 3 3) (list 5 6) (list 10 1) (list 2 5) (list 5 8) (list 5 4) (list 9 5)) 2) #t 0.001)
    (check-within (candidate (list (list 9 1 8 9 2 9 1 8 9 2) (list 10 2 7 8 9 10 2 7 8 9) (list 7 6 6 9 5 7 6 6 9 5)) 5) #t 0.001)
    (check-within (candidate (list (list 4 4 4 2 7 9 1 8 9 8) (list 3 3 6 3 8 8 7 7 4 5) (list 10 1 3 7 6 5 7 10 3 10)) 5) #f 0.001)
    (check-within (candidate (list (list 9 10 10 6 6 8 10 7 10 9) (list 10 6 1 10 10 5 7 9 9 2) (list 8 5 8 3 5 2 2 9 7 10)) 20) #t 0.001)
    (check-within (candidate (list (list 4 5 1 3 10) (list 10 5 9 10 2) (list 8 10 2 8 1) (list 5 8 9 3 4) (list 6 6 10 10 10) (list 6 1 7 9 4) (list 6 7 6 2 10)) 8) #f 0.001)
    (check-within (candidate (list (list 2 7 1 10 5 3) (list 10 7 8 2 2 2) (list 9 6 1 4 10 6) (list 6 1 1 9 2 5) (list 6 4 7 3 6 4) (list 10 10 5 4 2 1) (list 7 3 3 7 1 5) (list 5 8 2 10 5 1) (list 3 1 5 1 5 7)) 2) #f 0.001)
    (check-within (candidate (list (list 7 7 4) (list 8 9 9) (list 9 7 5) (list 6 3 6) (list 4 9 5) (list 1 10 3) (list 4 4 7) (list 4 7 6)) 1) #f 0.001)
    (check-within (candidate (list (list 10 10) (list 10 10) (list 5 5) (list 3 3) (list 2 2)) 2) #t 0.001)
    (check-within (candidate (list (list 6 4 7 6 3 9 4 2 10 5) (list 9 7 7 3 10 9 7 4 3 1)) 20) #t 0.001)
    (check-within (candidate (list (list 7)) 1) #t 0.001)
    (check-within (candidate (list (list 6 3 2)) 4) #f 0.001)
    (check-within (candidate (list (list 6 8)) 4) #t 0.001)
    (check-within (candidate (list (list 6 6 7 7 1) (list 10 3 3 2 2) (list 7 9 8 10 7) (list 10 8 2 7 1) (list 2 2 1 2 3) (list 6 2 8 10 10) (list 6 2 6 3 3) (list 2 2 2 4 7)) 4) #f 0.001)
    (check-within (candidate (list (list 8 8 5 3 7 8) (list 8 9 1 7 3 10) (list 4 3 9 8 4 7) (list 2 2 5 8 2 2) (list 6 1 2 7 4 8) (list 10 9 6 3 1 4) (list 7 1 6 7 4 6)) 2) #f 0.001)
    (check-within (candidate (list (list 7) (list 5) (list 5) (list 4) (list 4) (list 5) (list 8)) 6) #t 0.001)
    (check-within (candidate (list (list 8 8 5 10 7 8 8 5 10 7) (list 1 2 6 10 7 1 2 6 10 7)) 5) #t 0.001)
    (check-within (candidate (list (list 10 2 6 7 6 6 5) (list 6 3 3 4 6 5 7) (list 6 8 5 10 8 4 1)) 8) #f 0.001)
    (check-within (candidate (list (list 4 10 9 7 9 9 2) (list 3 9 2 1 8 9 10) (list 7 10 9 7 2 3 8)) 1) #f 0.001)
    (check-within (candidate (list (list 1 7 10 10 9 2 1) (list 6 4 5 2 3 3 10) (list 2 6 8 3 6 1 4)) 9) #f 0.001)
    (check-within (candidate (list (list 2 9 2 2 6 10 4 8 3) (list 10 8 4 5 10 3 3 8 5) (list 2 6 4 5 4 8 5 5 4) (list 1 3 2 10 5 3 10 9 4) (list 2 4 2 4 7 7 1 4 9)) 2) #f 0.001)
    (check-within (candidate (list (list 9 5 6 9 5 6) (list 1 9 4 1 9 4) (list 5 7 2 5 7 2) (list 9 1 5 9 1 5) (list 6 8 6 6 8 6) (list 10 1 7 10 1 7)) 6) #t 0.001)
    (check-within (candidate (list (list 2 7 6) (list 10 6 5) (list 10 2 4) (list 10 7 9) (list 5 8 6) (list 10 6 3) (list 10 9 6) (list 5 2 8) (list 10 1 2)) 7) #f 0.001)
    (check-within (candidate (list (list 5 4 5 10 5)) 9) #f 0.001)
    (check-within (candidate (list (list 10 10 9) (list 5 6 7) (list 1 4 7) (list 5 1 1) (list 5 1 5) (list 5 10 3)) 2) #f 0.001)
    (check-within (candidate (list (list 9 4 5) (list 8 5 4) (list 2 9 9)) 10) #f 0.001)
    (check-within (candidate (list (list 4 2 2 7 9 1 1 2) (list 1 8 7 5 7 5 9 6) (list 2 9 4 10 1 8 5 4)) 3) #f 0.001)
    (check-within (candidate (list (list 10) (list 7) (list 8) (list 2)) 1) #t 0.001)
    (check-within (candidate (list (list 10) (list 1) (list 5) (list 3) (list 1) (list 1)) 4) #t 0.001)
    (check-within (candidate (list (list 7 1 7 7 1 7 7 1 7) (list 5 10 1 5 10 1 5 10 1)) 3) #t 0.001)
    (check-within (candidate (list (list 4 7 9 9 4 7 9 9) (list 8 9 7 4 8 9 7 4) (list 6 8 6 4 6 8 6 4) (list 9 8 8 8 9 8 8 8) (list 3 6 5 3 3 6 5 3) (list 1 9 4 3 1 9 4 3) (list 8 3 2 7 8 3 2 7) (list 3 8 2 8 3 8 2 8) (list 6 5 2 8 6 5 2 8)) 8) #t 0.001)
    (check-within (candidate (list (list 4) (list 5) (list 4) (list 2) (list 4) (list 2) (list 7) (list 4)) 1) #t 0.001)
    (check-within (candidate (list (list 3 8 5 4 10 2) (list 9 3 9 5 4 2)) 6) #t 0.001)
    (check-within (candidate (list (list 8 9 10 6 5 7) (list 8 9 9 3 3 9) (list 4 5 4 4 4 10) (list 2 6 3 9 7 1) (list 10 10 4 4 6 10)) 9) #f 0.001)
    (check-within (candidate (list (list 7 7 7 7 7) (list 1 1 1 1 1)) 1) #t 0.001)
    (check-within (candidate (list (list 5 7 5 5 1 9 1 8 6 7) (list 8 1 9 10 10 5 4 9 1 8) (list 10 6 8 10 2 10 9 4 9 6) (list 4 7 10 2 7 4 2 10 3 5) (list 2 2 4 9 10 1 6 2 8 3) (list 1 3 5 9 9 8 10 8 9 10) (list 7 8 7 7 6 9 2 5 8 4) (list 6 9 4 2 4 10 10 8 10 7)) 8) #f 0.001)
    (check-within (candidate (list (list 2 9 10) (list 7 3 3) (list 7 6 2)) 1) #f 0.001)
    (check-within (candidate (list (list 2 5 8 9 6 8) (list 3 6 4 10 10 6) (list 9 6 10 9 6 5)) 1) #f 0.001)
    (check-within (candidate (list (list 2 2) (list 4 5) (list 3 2) (list 4 6) (list 1 9) (list 5 3) (list 3 5) (list 2 4) (list 3 9)) 7) #f 0.001)
    (check-within (candidate (list (list 1 8 6 8 6 7 1 6)) 16) #t 0.001)
    (check-within (candidate (list (list 7 9 9 2 7) (list 8 5 8 6 7) (list 2 9 8 5 2) (list 9 9 2 6 8) (list 7 4 10 10 8)) 6) #f 0.001)
    (check-within (candidate (list (list 8 8) (list 6 6) (list 2 2) (list 8 8) (list 9 9) (list 8 8) (list 10 10) (list 3 3) (list 4 4) (list 5 5)) 1) #t 0.001)
    (check-within (candidate (list (list 3 3 3 3 3 3) (list 5 3 5 3 5 3) (list 2 5 2 5 2 5) (list 8 8 8 8 8 8) (list 3 8 3 8 3 8) (list 5 3 5 3 5 3) (list 1 8 1 8 1 8) (list 8 9 8 9 8 9) (list 2 8 2 8 2 8)) 4) #t 0.001)
    (check-within (candidate (list (list 2 2 2 2 2) (list 7 7 7 7 7) (list 5 5 5 5 5) (list 8 8 8 8 8) (list 1 1 1 1 1) (list 10 10 10 10 10) (list 7 7 7 7 7)) 1) #t 0.001)
    (check-within (candidate (list (list 3 1 10 5 10 3 1 10 5 10) (list 3 5 9 2 10 3 5 9 2 10) (list 4 6 3 5 7 4 6 3 5 7) (list 8 10 6 7 8 8 10 6 7 8)) 5) #t 0.001)
    (check-within (candidate (list (list 10 7 1 7) (list 3 5 9 5) (list 2 8 3 10) (list 8 7 1 9) (list 3 8 6 3) (list 6 5 8 9) (list 8 7 5 1) (list 10 4 9 9) (list 4 6 1 9) (list 6 10 1 7)) 3) #f 0.001)
    (check-within (candidate (list (list 1 10 3 9 6) (list 7 1 3 4 10)) 3) #f 0.001)
    (check-within (candidate (list (list 7 7) (list 2 2) (list 5 5)) 1) #t 0.001)
    (check-within (candidate (list (list 4 4 4 4 4 4 4) (list 2 2 2 2 2 2 2) (list 3 3 3 3 3 3 3) (list 8 8 8 8 8 8 8) (list 6 6 6 6 6 6 6)) 2) #t 0.001)
    (check-within (candidate (list (list 10 3 5 3 10 3 5 3) (list 2 3 9 7 2 3 9 7) (list 10 4 4 8 10 4 4 8) (list 10 2 7 9 10 2 7 9) (list 8 1 8 3 8 1 8 3) (list 1 9 1 7 1 9 1 7)) 4) #t 0.001)
    (check-within (candidate (list (list 6) (list 7) (list 1)) 2) #t 0.001)
    (check-within (candidate (list (list 7 6 4 5)) 5) #f 0.001)
    (check-within (candidate (list (list 5 5 5 5) (list 5 5 5 5) (list 10 10 10 10) (list 2 2 2 2) (list 3 3 3 3) (list 2 2 2 2) (list 8 8 8 8) (list 10 10 10 10) (list 9 9 9 9) (list 7 7 7 7)) 2) #t 0.001)
    (check-within (candidate (list (list 5 1 1 9 4 1 7 6) (list 8 7 7 6 2 2 1 5) (list 10 2 5 3 10 7 7 5) (list 10 6 1 6 8 4 6 3) (list 10 10 9 8 2 10 8 7) (list 7 4 2 10 2 3 8 7) (list 4 7 5 9 10 4 3 2) (list 10 9 7 7 6 3 9 7) (list 1 4 8 4 6 5 5 1)) 1) #f 0.001)
    (check-within (candidate (list (list 3)) 1) #t 0.001)
    (check-within (candidate (list (list 1 1 1 1 1) (list 10 10 10 10 10) (list 10 10 10 10 10)) 2) #t 0.001)
    (check-within (candidate (list (list 6) (list 3) (list 2) (list 10)) 2) #t 0.001)
    (check-within (candidate (list (list 9 7 5 6) (list 5 2 1 8) (list 9 4 3 6) (list 5 7 4 1) (list 8 1 8 9) (list 4 3 6 5) (list 6 2 7 3) (list 1 3 6 4) (list 4 9 5 5)) 7) #f 0.001)
    (check-within (candidate (list (list 10 7 2 10 5 2 7) (list 10 10 3 8 3 3 8) (list 4 3 10 10 10 4 10)) 4) #f 0.001)
    (check-within (candidate (list (list 8 5) (list 8 10) (list 8 10) (list 1 1) (list 2 1)) 7) #f 0.001)
    (check-within (candidate (list (list 9 9) (list 8 8) (list 2 2) (list 1 1) (list 8 8) (list 4 4) (list 9 9) (list 4 4) (list 6 6)) 2) #t 0.001)
    (check-within (candidate (list (list 10 1 1) (list 7 10 6) (list 9 6 6) (list 9 8 10) (list 8 2 1) (list 6 8 3) (list 8 6 6)) 5) #f 0.001)
    (check-within (candidate (list (list 2 10 5 6 5 5) (list 6 3 1 5 4 7) (list 5 6 3 2 4 10) (list 9 2 6 8 6 2) (list 3 6 8 4 9 1)) 8) #f 0.001)
    (check-within (candidate (list (list 10 3 4 2 8 10 3 4 2 8) (list 9 9 3 4 5 9 9 3 4 5) (list 6 9 9 2 7 6 9 9 2 7) (list 5 2 3 3 4 5 2 3 3 4)) 5) #t 0.001)
    (check-within (candidate (list (list 3 4 10 3 4 10) (list 5 5 4 5 5 4) (list 5 5 3 5 5 3) (list 7 8 7 7 8 7)) 3) #t 0.001)
    (check-within (candidate (list (list 7 1 9 3 6) (list 5 6 5 5 6) (list 2 3 5 10 8) (list 5 10 2 5 4) (list 7 9 1 7 10) (list 8 2 3 4 2) (list 1 6 9 2 1)) 7) #f 0.001)
    (check-within (candidate (list (list 3 3) (list 3 3) (list 4 4) (list 3 3) (list 8 8) (list 5 5)) 1) #t 0.001)
    (check-within (candidate (list (list 2 10 2 6 3 6) (list 4 5 10 7 7 9) (list 1 7 4 1 9 4) (list 3 7 6 3 1 4) (list 4 10 4 6 3 5) (list 1 5 5 9 5 1) (list 10 2 5 4 7 10) (list 2 9 7 4 5 3) (list 5 5 1 2 8 3)) 2) #f 0.001)
    (check-within (candidate (list (list 5) (list 5) (list 5)) 2) #t 0.001)
    (check-within (candidate (list (list 9 5 5 6 7) (list 7 9 3 8 1) (list 8 8 8 9 5) (list 1 3 2 6 9) (list 3 6 4 8 7) (list 9 3 3 9 10) (list 8 5 1 2 8) (list 7 3 10 5 1) (list 8 4 5 5 1)) 5) #t 0.001)
    (check-within (candidate (list (list 5 8 5 2 8 5 9) (list 7 8 2 2 8 2 2) (list 4 5 6 7 3 9 9) (list 5 7 4 8 2 9 2) (list 9 5 3 3 5 7 3) (list 3 8 9 6 3 10 7) (list 6 7 3 7 3 6 6)) 8) #f 0.001)
    (check-within (candidate (list (list 8 8 8 8 8 8) (list 8 8 8 8 8 8) (list 2 2 2 2 2 2) (list 6 6 6 6 6 6) (list 9 9 9 9 9 9) (list 10 10 10 10 10 10) (list 10 10 10 10 10 10)) 2) #t 0.001)
    (check-within (candidate (list (list 7 7 10 2) (list 3 5 7 6) (list 2 10 1 8) (list 8 3 1 10) (list 5 1 3 3) (list 6 3 4 9) (list 8 9 1 1)) 7) #f 0.001)
    (check-within (candidate (list (list 5 2 7 2 6 10 7 5) (list 10 9 4 1 7 2 7 4) (list 2 6 7 3 2 10 4 5) (list 10 4 7 2 10 3 6 2)) 16) #t 0.001)
    (check-within (candidate (list (list 9 10 10 1) (list 1 7 3 5) (list 9 6 4 7) (list 6 6 4 5) (list 2 4 2 7) (list 2 1 1 1) (list 7 2 1 8) (list 2 8 1 3) (list 7 4 6 1) (list 10 10 7 5)) 4) #t 0.001)
    (check-within (candidate (list (list 7 3 10 2 3 1 10) (list 7 6 10 1 3 2 1) (list 9 1 5 7 1 8 3) (list 4 10 10 7 7 9 7) (list 7 9 1 5 3 8 4) (list 4 9 5 10 2 8 10) (list 2 5 10 3 6 2 9) (list 6 7 2 3 4 2 2)) 1) #f 0.001)
    (check-within (candidate (list (list 8 8) (list 9 9) (list 2 2) (list 10 10) (list 10 10) (list 1 1) (list 5 5) (list 9 9) (list 7 7)) 2) #t 0.001)
    (check-within (candidate (list (list 2 1 7 3 7 6 7 9 9 3) (list 3 9 10 4 4 6 8 10 5 6) (list 9 8 6 2 3 4 9 1 9 10) (list 7 10 8 8 3 9 9 5 8 9) (list 9 5 6 9 9 6 4 3 2 3) (list 3 10 6 2 7 6 10 6 2 6) (list 7 9 7 4 5 7 2 4 9 5) (list 4 7 9 6 7 4 6 4 10 4)) 6) #f 0.001)
))

(test-humaneval)