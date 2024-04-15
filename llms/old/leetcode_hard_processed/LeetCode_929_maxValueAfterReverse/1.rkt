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
  ;; Calculate the initial total value of the array based on the given formula.
  (define total (for/sum ([i (in-range (sub1 (length nums)))])
                         (abs (- (list-ref nums i) (list-ref nums (+ i 1))))))
  ;; Initialize variables to store the minimum and maximum values encountered in a specific way.
  (define min2 +inf.0)
  (define max2 -inf.0)
  ;; Initialize a variable to store the maximum difference found.
  (define diff 0)
  ;; Loop through the list to calculate min2, max2, and the maximum difference (diff).
  (for ([i (in-range (sub1 (length nums)))])
    (let* ((current (list-ref nums i))
           (next (list-ref nums (+ i 1)))
           (current-max (max current next))
           (current-min (min current next)))
      ;; Update min2 and max2 based on the encountered values.
      (set! min2 (min min2 current-max))
      (set! max2 (max max2 current-min))
      ;; Update diff based on the potential increase in value from reversing a sublist starting from the first element or ending at the last element.
      (set! diff (max diff (- (abs (- next (first nums))) (abs (- next current)))))
      (set! diff (max diff (- (abs (- current (last nums))) (abs (- current next)))))))
  ;; Calculate the final maximum possible value by adding the maximum of diff and 2 times (max2 - min2) to the initial total.
  (+ total (max diff (* 2 (- max2 min2)))))

;; Example usage
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