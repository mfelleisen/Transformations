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
  ;; Initialize total sum of absolute differences, the maximum potential difference,
  ;; and variables to track the second smallest and second largest elements after reversing any sublist.
  (let ([total 0] [diff 0] [min2 +inf.0] [max2 -inf.0])
    ;; Iterate through nums to calculate the total initial sum of absolute differences,
    ;; and update min2, max2, and diff for potential improvement by reversing sublists.
    (for ([i (in-range 0 (- (length nums) 1))])
      (set! total (+ total (abs (- (list-ref nums i) (list-ref nums (+ i 1))))))
      (set! min2 (min min2 (max (list-ref nums i) (list-ref nums (+ i 1)))))
      (set! max2 (max max2 (min (list-ref nums i) (list-ref nums (+ i 1)))))
      (set! diff (max diff (- (abs (- (list-ref nums (+ i 1)) (first nums)))
                              (abs (- (list-ref nums (+ i 1)) (list-ref nums i))))))
      (set! diff (max diff (- (abs (- (list-ref nums i) (last nums)))
                              (abs (- (list-ref nums i) (list-ref nums (+ i 1))))))))
    ;; Return the total initial sum plus the maximum of diff and twice the difference between max2 and min2,
    ;; which represents the maximum improvement possible by reversing a sublist.
    (+ total (max diff (* 2 (- max2 min2))))))

;; The function `maxValueAfterReverse` calculates the maximum possible value of the list after potentially reversing a sublist,
;; by initially calculating the sum of absolute differences, then finding the maximum improvement achievable through reversal.
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