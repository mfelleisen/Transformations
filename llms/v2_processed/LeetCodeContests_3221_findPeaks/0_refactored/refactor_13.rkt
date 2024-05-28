#lang racket

;; You are given a 0-indexed array mountain. Your task is to find all the peaks in the mountain array.
;; Return an array that consists of indices of peaks in the given array in any order.
;; Notes:
;; A peak is defined as an element that is strictly greater than its neighboring elements.
;; The first and last elements of the array are not a peak.
;; Example 1:
;; Input: mountain = [2,4,4]
;; Output: []
;; Explanation: mountain[0] and mountain[2] can not be a peak because they are first and last elements of the array.
;; mountain[1] also can not be a peak because it is not strictly greater than mountain[2].
;; So the answer is [].
;; Example 2:
;; Input: mountain = [1,4,3,8,5]
;; Output: [1,3]
;; Explanation: mountain[0] and mountain[4] can not be a peak because they are first and last elements of the array.
;; mountain[2] also can not be a peak because it is not strictly greater than mountain[3] and mountain[1].
;; But mountain [1] and mountain[3] are strictly greater than their neighboring elements.
;; So the answer is [1,3].
;; Constraints:
;; 3 <= mountain.length <= 100
;; 1 <= mountain[i] <= 100
(define (findPeaks mountain)
  (define (is-peak? idx)
    (let ([current (list-ref mountain idx)]
          [prev (list-ref mountain (sub1 idx))]
          [next (list-ref mountain (add1 idx))])
      (and (> current prev) (> current next))))

  (define (range start end)
    (if (>= start end)
        '()
        (cons start (range (add1 start) end))))
  
  (filter (lambda (idx) (is-peak? idx))
          (range 1 (sub1 (length mountain)))))

;; Example usage:
;; (findPeaks '(1 4 3 8 5))  ;; Should return '(1 3)
;; (findPeaks '(2 4 4))      ;; Should return '()

(require rackunit)


(define (test-humaneval) 

  (let (( candidate findPeaks))
    (check-within (candidate (list 2 4 4)) (list ) 0.001)
    (check-within (candidate (list 1 4 3 8 5)) (list 1 3) 0.001)
    (check-within (candidate (list 1 1 1)) (list ) 0.001)
    (check-within (candidate (list 1 1 3)) (list ) 0.001)
    (check-within (candidate (list 1 1 5)) (list ) 0.001)
    (check-within (candidate (list 1 2 5)) (list ) 0.001)
    (check-within (candidate (list 1 4 1)) (list 1) 0.001)
    (check-within (candidate (list 1 4 3)) (list 1) 0.001)
    (check-within (candidate (list 1 5 5)) (list ) 0.001)
    (check-within (candidate (list 1 6 4)) (list 1) 0.001)
    (check-within (candidate (list 2 1 1)) (list ) 0.001)
    (check-within (candidate (list 2 1 2)) (list ) 0.001)
    (check-within (candidate (list 2 2 3)) (list ) 0.001)
    (check-within (candidate (list 2 2 5)) (list ) 0.001)
    (check-within (candidate (list 2 3 2)) (list 1) 0.001)
    (check-within (candidate (list 2 3 6)) (list ) 0.001)
    (check-within (candidate (list 2 4 3)) (list 1) 0.001)
    (check-within (candidate (list 2 4 5)) (list ) 0.001)
    (check-within (candidate (list 2 6 4)) (list 1) 0.001)
    (check-within (candidate (list 3 3 3)) (list ) 0.001)
    (check-within (candidate (list 3 3 5)) (list ) 0.001)
    (check-within (candidate (list 3 4 6)) (list ) 0.001)
    (check-within (candidate (list 3 5 1)) (list 1) 0.001)
    (check-within (candidate (list 3 5 3)) (list 1) 0.001)
    (check-within (candidate (list 3 5 4)) (list 1) 0.001)
    (check-within (candidate (list 3 5 6)) (list ) 0.001)
    (check-within (candidate (list 4 2 1)) (list ) 0.001)
    (check-within (candidate (list 4 2 2)) (list ) 0.001)
    (check-within (candidate (list 4 2 4)) (list ) 0.001)
    (check-within (candidate (list 4 2 6)) (list ) 0.001)
    (check-within (candidate (list 4 4 1)) (list ) 0.001)
    (check-within (candidate (list 4 4 2)) (list ) 0.001)
    (check-within (candidate (list 4 4 5)) (list ) 0.001)
    (check-within (candidate (list 4 5 4)) (list 1) 0.001)
    (check-within (candidate (list 4 6 1)) (list 1) 0.001)
    (check-within (candidate (list 4 6 6)) (list ) 0.001)
    (check-within (candidate (list 5 1 2)) (list ) 0.001)
    (check-within (candidate (list 5 2 1)) (list ) 0.001)
    (check-within (candidate (list 5 2 2)) (list ) 0.001)
    (check-within (candidate (list 5 2 4)) (list ) 0.001)
    (check-within (candidate (list 5 3 1)) (list ) 0.001)
    (check-within (candidate (list 5 5 1)) (list ) 0.001)
    (check-within (candidate (list 5 5 2)) (list ) 0.001)
    (check-within (candidate (list 5 5 6)) (list ) 0.001)
    (check-within (candidate (list 5 6 1)) (list 1) 0.001)
    (check-within (candidate (list 5 6 4)) (list 1) 0.001)
    (check-within (candidate (list 6 1 1)) (list ) 0.001)
    (check-within (candidate (list 6 1 2)) (list ) 0.001)
    (check-within (candidate (list 6 1 5)) (list ) 0.001)
    (check-within (candidate (list 6 2 2)) (list ) 0.001)
    (check-within (candidate (list 6 2 5)) (list ) 0.001)
    (check-within (candidate (list 6 3 2)) (list ) 0.001)
    (check-within (candidate (list 6 3 3)) (list ) 0.001)
    (check-within (candidate (list 6 3 6)) (list ) 0.001)
    (check-within (candidate (list 6 4 3)) (list ) 0.001)
    (check-within (candidate (list 6 5 2)) (list ) 0.001)
    (check-within (candidate (list 6 5 4)) (list ) 0.001)
    (check-within (candidate (list 6 6 4)) (list ) 0.001)
    (check-within (candidate (list 1 1 1 4)) (list ) 0.001)
    (check-within (candidate (list 1 1 7 7)) (list ) 0.001)
    (check-within (candidate (list 1 3 6 5)) (list 2) 0.001)
    (check-within (candidate (list 1 4 7 8)) (list ) 0.001)
    (check-within (candidate (list 1 6 6 6)) (list ) 0.001)
    (check-within (candidate (list 1 8 1 8)) (list 1) 0.001)
    (check-within (candidate (list 2 2 1 2)) (list ) 0.001)
    (check-within (candidate (list 2 3 7 6)) (list 2) 0.001)
    (check-within (candidate (list 2 5 4 5)) (list 1) 0.001)
    (check-within (candidate (list 2 7 1 2)) (list 1) 0.001)
    (check-within (candidate (list 2 7 2 6)) (list 1) 0.001)
    (check-within (candidate (list 2 7 5 3)) (list 1) 0.001)
    (check-within (candidate (list 2 7 7 6)) (list ) 0.001)
    (check-within (candidate (list 3 1 2 5)) (list ) 0.001)
    (check-within (candidate (list 3 3 4 2)) (list 2) 0.001)
    (check-within (candidate (list 3 3 7 8)) (list ) 0.001)
    (check-within (candidate (list 3 4 2 4)) (list 1) 0.001)
    (check-within (candidate (list 3 4 5 4)) (list 2) 0.001)
    (check-within (candidate (list 3 4 7 6)) (list 2) 0.001)
    (check-within (candidate (list 3 5 5 3)) (list ) 0.001)
    (check-within (candidate (list 3 6 4 7)) (list 1) 0.001)
    (check-within (candidate (list 3 8 5 5)) (list 1) 0.001)
    (check-within (candidate (list 4 2 4 3)) (list 2) 0.001)
    (check-within (candidate (list 4 2 6 8)) (list ) 0.001)
    (check-within (candidate (list 4 3 3 8)) (list ) 0.001)
    (check-within (candidate (list 4 4 8 7)) (list 2) 0.001)
    (check-within (candidate (list 4 5 1 1)) (list 1) 0.001)
    (check-within (candidate (list 4 6 1 7)) (list 1) 0.001)
    (check-within (candidate (list 4 6 2 1)) (list 1) 0.001)
    (check-within (candidate (list 4 6 2 2)) (list 1) 0.001)
    (check-within (candidate (list 5 1 7 6)) (list 2) 0.001)
    (check-within (candidate (list 5 3 2 2)) (list ) 0.001)
    (check-within (candidate (list 5 3 6 3)) (list 2) 0.001)
    (check-within (candidate (list 5 3 8 3)) (list 2) 0.001)
    (check-within (candidate (list 5 4 4 6)) (list ) 0.001)
    (check-within (candidate (list 5 4 4 8)) (list ) 0.001)
    (check-within (candidate (list 5 5 1 7)) (list ) 0.001)
    (check-within (candidate (list 5 5 8 2)) (list 2) 0.001)
    (check-within (candidate (list 5 6 7 4)) (list 2) 0.001)
    (check-within (candidate (list 5 7 4 3)) (list 1) 0.001)
    (check-within (candidate (list 5 8 7 8)) (list 1) 0.001)
    (check-within (candidate (list 6 2 8 6)) (list 2) 0.001)
))

(test-humaneval)