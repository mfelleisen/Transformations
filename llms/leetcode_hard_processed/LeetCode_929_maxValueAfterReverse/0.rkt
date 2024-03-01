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
  (let ([total 0] ; Initialize total sum of absolute differences
        [diff 0]  ; Maximum difference that can be achieved by reversing
        [min2 +inf.0] ; Second smallest value in `nums` after potential reverse
        [max2 -inf.0]) ; Second largest value in `nums` after potential reverse
    (for ([i (range (sub1 (length nums)))]) ; Iterate over nums except the last element
      (let* ([a (list-ref nums i)] ; Current element
             [b (list-ref nums (+ i 1))] ; Next element
             [ab-diff (abs (- a b))]) ; Absolute difference between them
        (set! total (+ total ab-diff)) ; Add it to total
        (set! min2 (min min2 (max a b))) ; Update min2
        (set! max2 (max max2 (min a b))) ; Update max2
        ; Update diff with maximum of current diff and new potential diffs
        (set! diff (max diff (- (abs (- b (first nums))) ab-diff)))
        (set! diff (max diff (- (abs (- a (last nums))) ab-diff)))))
    ; Return total plus the maximum of diff and twice the difference between max2 and min2
    (+ total (max diff (* 2 (- max2 min2))))))

;; The function takes a list of integers `nums` as input and returns an integer.
;; It calculates the maximum possible value of the list after reversing any sublist exactly once.
;; This is achieved by initially computing the total sum of absolute differences between adjacent elements,
;; and then finding the maximum improvement (`diff`) that can be achieved by reversing any sublist.
;; The function returns the sum of the initial total and the maximum of `diff` and twice the difference between the second largest and second smallest values (`max2` and `min2`) after a potential reverse.
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