#lang racket

;; You are given an integer list nums. The value of this list is defined as the sum of |nums[i] - nums[i + 1]| for all 0 <= i < nums.length - 1.
;; You are allowed to select any sublist of the given list and reverse it. You can perform this operation only once.
;; Find maximum possible value of the final list.
;; Example 1:
;; Input: nums = [2,3,1,5,4]
;; Output: 10
;; Explanation: By reversing the sublist [3,1,5] the list becomes [2,5,1,3,4] whose value is 10.
;; Example 2:
;; Input: nums = [2,4,9,24,2,1,10]
;; Output: 68
;; Constraints:
;; 1 <= nums.length <= 3 * 104
;; -105 <= nums[i] <= 105
(define (maxValueAfterReverse nums)
  ;; Initialize the variables to track the total sum, the maximum difference achievable,
  ;; and the minimum and maximum values when considering the reversal effect.
  (let ([total 0]
        [diff 0]
        [min2 +inf.0]
        [max2 -inf.0])
    ;; Loop through the list of numbers to calculate the initial total sum and identify
    ;; opportunities for optimization by reversing sublists.
    (for ([i (range (sub1 (length nums)))])
      ;; Calculate the total sum of absolute differences.
      (set! total (+ total (abs (- (list-ref nums i) (list-ref nums (+ i 1))))))
      ;; Update min2 and max2 based on the current and next element considering the reversal effect.
      (set! min2 (min min2 (max (list-ref nums i) (list-ref nums (+ i 1)))))
      (set! max2 (max max2 (min (list-ref nums i) (list-ref nums (+ i 1)))))
      ;; Update diff based on the potential improvement by reversing from the start or to the end.
      (set! diff (max diff (- (abs (- (list-ref nums (+ i 1)) (first nums)))
                             (abs (- (list-ref nums (+ i 1)) (list-ref nums i))))))
      (set! diff (max diff (- (abs (- (list-ref nums i) (last nums)))
                             (abs (- (list-ref nums i) (list-ref nums (+ i 1))))))))
    ;; Return the maximum possible value by adding the best improvement found to the initial total.
    (+ total (max diff (* 2 (- max2 min2))))))

;; Examples
(maxValueAfterReverse '(2 3 1 5 4))  ; Output: 10
(maxValueAfterReverse '(2 4 9 24 2 1 10))  ; Output: 68
(require rackunit)

(define (test-humaneval) 

  (let (( candidate maxValueAfterReverse))
    (check-within (candidate (list 2 4 9 24 2 1 10)) 68 0.001)
    (check-within (candidate (list 5 5 5 5 5)) 0 0.001)
    (check-within (candidate (list 2 4 9 24 2 1 10)) 68 0.001)
    (check-within (candidate (list 10 20 30 40 50)) 80 0.001)
    (check-within (candidate (list 1 1 1 1 1)) 0 0.001)
    (check-within (candidate (list 2 3 1 5 4)) 10 0.001)
))

(test-humaneval)