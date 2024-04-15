#lang racket

;; Given an integer list nums, return the maximum difference between two successive elements in its sorted form. If the list contains less than two elements, return 0.
;; You must write an algorithm that runs in linear time and uses linear extra space.
;; Example 1:
;; Input: nums = [3,6,9,1]
;; Output: 3
;; Explanation: The sorted form of the list is [1,3,6,9], either (3,6) or (6,9) has the maximum difference 3.
;; Example 2:
;; Input: nums = [10]
;; Output: 0
;; Explanation: The list contains less than 2 elements, therefore return 0.
;; Constraints:
;; 1 <= nums.length <= 105
;; 0 <= nums[i] <= 109
(define (maximumGap nums)
  ;; If there are less than two elements, return 0 as per the problem statement.
  (if (< (length nums) 2)
      0
      (let* ((min-val (apply min nums))  ;; Find the minimum value in the list.
             (max-val (apply max nums))  ;; Find the maximum value in the list.
             (length (length nums))  ;; Calculate the length of the input list.
             ;; Calculate the size of each bucket. Avoid division by zero by ensuring bucket size is at least 1.
             (bucket-size (max 1 (quotient (- max-val min-val) (- length 1))))
             ;; Calculate the number of buckets needed.
             (bucket-num (+ 1 (quotient (- max-val min-val) bucket-size)))
             ;; Initialize buckets with lists of [min, max] values, starting with [inf, -inf].
             (buckets (make-vector bucket-num (list +inf.0 -inf.0))))
        ;; Fill the buckets with the minimum and maximum values that fall into each bucket.
        (for-each (lambda (num)
                    (let* ((idx (quotient (- num min-val) bucket-size))
                           (bucket (vector-ref buckets idx))
                           (new-min (min (first bucket) num))
                           (new-max (max (second bucket) num)))
                      (vector-set! buckets idx (list new-min new-max))))
                  nums)
        ;; Compute the maximum gap by iterating through the buckets.
        (let loop ((i 0)
                   (prev-max min-val)
                   (max-gap 0))
          (if (< i bucket-num)
              (let ((bucket (vector-ref buckets i)))
                (if (= (first bucket) +inf.0)  ;; Skip empty buckets.
                    (loop (+ i 1) prev-max max-gap)
                    (let* ((current-min (first bucket))
                           (current-max (second bucket))
                           (current-gap (- current-min prev-max)))
                      (loop (+ i 1) current-max (max max-gap current-gap)))))
              max-gap)))))

;; Example usage:
(maximumGap '(3 6 9 1))  ;; Expected output: 3
(maximumGap '(10))       ;; Expected output: 0
(require rackunit)

(define (test-humaneval) 

  (let (( candidate maximumGap))
    (check-within (candidate (list 1 0 1 0 1 0 1 0 1 0)) 1 0.001)
    (check-within (candidate (list 1 2 4 8 16 32 64 128 256 512)) 256 0.001)
    (check-within (candidate (list 0 0 0 0 0 0 0 0 0 1)) 1 0.001)
    (check-within (candidate (list 0)) 0 0.001)
    (check-within (candidate (list 1 1 1 1 1 1 1 1 1 1)) 0 0.001)
    (check-within (candidate (list 1 3 6 9 12 15 18 21 24 27)) 3 0.001)
    (check-within (candidate (list 9 8 7 6 5 4 3 2 1 0)) 1 0.001)
    (check-within (candidate (list 1 2 3 4 5 6 7 8 9 10)) 1 0.001)
    (check-within (candidate (list 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000)) 1000 0.001)
    (check-within (candidate (list 10 20 30 40 50 60 70 80 90 100)) 10 0.001)
    (check-within (candidate (list 0 0 0 1 1 1 1 1 1 1)) 1 0.001)
    (check-within (candidate (list 10 100 1000 10000 100000 1000000 10000000)) 9000000 0.001)
    (check-within (candidate (list 100 200 300 400 500 600 700 800 900 1000)) 100 0.001)
    (check-within (candidate (list 3 6 9 1)) 3 0.001)
    (check-within (candidate (list 1 10 100 1000 10000 100000 1000000)) 900000 0.001)
    (check-within (candidate (list 0 0 0 0 0 0 0 0 0 0)) 0 0.001)
    (check-within (candidate (list )) 0 0.001)
    (check-within (candidate (list 1 2 3 5 8 13 21 34 55 89)) 34 0.001)
    (check-within (candidate (list 5 10 15 20 25 30 35 40 45 50)) 5 0.001)
    (check-within (candidate (list 1 1 1 1 1 2 2 2 2 2)) 1 0.001)
    (check-within (candidate (list 10)) 0 0.001)
))

(test-humaneval)