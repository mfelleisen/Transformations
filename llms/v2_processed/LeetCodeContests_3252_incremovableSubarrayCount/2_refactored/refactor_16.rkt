#lang racket

;; You are given a 0-indexed array of positive integers nums.
;; A subarray of nums is called incremovable if nums becomes strictly increasing on removing the subarray. For example, the subarray [3, 4] is an incremovable subarray of [5, 3, 4, 6, 7] because removing this subarray changes the array [5, 3, 4, 6, 7] to [5, 6, 7] which is strictly increasing.
;; Return the total number of incremovable subarrays of nums.
;; Note that an empty array is considered strictly increasing.
;; A subarray is a contiguous non-empty sequence of elements within an array.
;; Example 1:
;; Input: nums = [1,2,3,4]
;; Output: 10
;; Explanation: The 10 incremovable subarrays are: [1], [2], [3], [4], [1,2], [2,3], [3,4], [1,2,3], [2,3,4], and [1,2,3,4], because on removing any one of these subarrays nums becomes strictly increasing. Note that you cannot select an empty subarray.
;; Example 2:
;; Input: nums = [6,5,7,8]
;; Output: 7
;; Explanation: The 7 incremovable subarrays are: [5], [6], [5,7], [6,5], [5,7,8], [6,5,7] and [6,5,7,8].
;; It can be shown that there are only 7 incremovable subarrays in nums.
;; Example 3:
;; Input: nums = [8,7,6,6]
;; Output: 3
;; Explanation: The 3 incremovable subarrays are: [8,7,6], [7,6,6], and [8,7,6,6]. Note that [8,7] is not an incremovable subarray because after removing [8,7] nums becomes [6,6], which is sorted in ascending order but not strictly increasing.
;; Constraints:
;; 1 <= nums.length <= 50
;; 1 <= nums[i] <= 50
(define (incremovableSubarrayCount nums)
  ;; Function to check if the list is strictly increasing
  (define (is-strictly-increasing? lst)
    (match lst
      [(list) #true]
      [(list _) #true]
      [(cons x (cons y rest))
       (and (< x y) (is-strictly-increasing? (cons y rest)))]))

  ;; Function to generate all subarrays
  (define (subarrays lst)
    (for*/list ([start (in-range (length lst))]
                [end (in-range (add1 start) (add1 (length lst)))])
      (sublist lst start end)))

  ;; Function to remove subarray from list
  (define (remove-subarray lst sub)
    (let loop ([lst lst] [sub sub] [result '()])
      (cond
        [(empty? sub) (reverse (append result lst))]
        [(equal? (first lst) (first sub))
         (loop (rest lst) (rest sub) result)]
        [else
         (loop (rest lst) sub (cons (first lst) result))])))

  ;; Function to count all incremovable subarrays
  (define (count-incremovables)
    (for/fold ([count 0]) ([sub (in-list (subarrays nums))])
      (let ([modified-nums (remove-subarray nums sub)])
        (if (is-strictly-increasing? modified-nums)
            (add1 count)
            count))))

  (count-incremovables))

;; Example usage:
(incremovableSubarrayCount '(1 2 3 4))  ; Output: 10
(incremovableSubarrayCount '(6 5 7 8))  ; Output: 7
(incremovableSubarrayCount '(8 7 6 6))  ; Output: 3

(require rackunit)


(define (test-humaneval) 

  (let (( candidate incremovableSubarrayCount))
    (check-within (candidate (list 1 2 3 4)) 10 0.001)
    (check-within (candidate (list 6 5 7 8)) 7 0.001)
    (check-within (candidate (list 8 7 6 6)) 3 0.001)
    (check-within (candidate (list 1)) 1 0.001)
    (check-within (candidate (list 2)) 1 0.001)
    (check-within (candidate (list 3)) 1 0.001)
    (check-within (candidate (list 4)) 1 0.001)
    (check-within (candidate (list 5)) 1 0.001)
    (check-within (candidate (list 6)) 1 0.001)
    (check-within (candidate (list 7)) 1 0.001)
    (check-within (candidate (list 8)) 1 0.001)
    (check-within (candidate (list 9)) 1 0.001)
    (check-within (candidate (list 10)) 1 0.001)
    (check-within (candidate (list 1 2)) 3 0.001)
    (check-within (candidate (list 1 4)) 3 0.001)
    (check-within (candidate (list 1 8)) 3 0.001)
    (check-within (candidate (list 2 10)) 3 0.001)
    (check-within (candidate (list 3 4)) 3 0.001)
    (check-within (candidate (list 3 8)) 3 0.001)
    (check-within (candidate (list 3 10)) 3 0.001)
    (check-within (candidate (list 4 2)) 3 0.001)
    (check-within (candidate (list 4 3)) 3 0.001)
    (check-within (candidate (list 4 5)) 3 0.001)
    (check-within (candidate (list 5 1)) 3 0.001)
    (check-within (candidate (list 5 4)) 3 0.001)
    (check-within (candidate (list 5 6)) 3 0.001)
    (check-within (candidate (list 5 7)) 3 0.001)
    (check-within (candidate (list 5 9)) 3 0.001)
    (check-within (candidate (list 6 4)) 3 0.001)
    (check-within (candidate (list 6 5)) 3 0.001)
    (check-within (candidate (list 6 9)) 3 0.001)
    (check-within (candidate (list 9 3)) 3 0.001)
    (check-within (candidate (list 9 4)) 3 0.001)
    (check-within (candidate (list 9 7)) 3 0.001)
    (check-within (candidate (list 9 8)) 3 0.001)
    (check-within (candidate (list 10 5)) 3 0.001)
    (check-within (candidate (list 10 10)) 3 0.001)
    (check-within (candidate (list 1 2 3)) 6 0.001)
    (check-within (candidate (list 1 2 6)) 6 0.001)
    (check-within (candidate (list 2 1 6)) 5 0.001)
    (check-within (candidate (list 2 4 5)) 6 0.001)
    (check-within (candidate (list 2 6 8)) 6 0.001)
    (check-within (candidate (list 2 6 9)) 6 0.001)
    (check-within (candidate (list 2 10 9)) 5 0.001)
    (check-within (candidate (list 3 1 9)) 5 0.001)
    (check-within (candidate (list 3 5 9)) 6 0.001)
    (check-within (candidate (list 3 7 2)) 4 0.001)
    (check-within (candidate (list 3 8 8)) 5 0.001)
    (check-within (candidate (list 3 10 10)) 5 0.001)
    (check-within (candidate (list 4 5 2)) 4 0.001)
    (check-within (candidate (list 5 8 4)) 4 0.001)
    (check-within (candidate (list 5 9 3)) 4 0.001)
    (check-within (candidate (list 5 9 7)) 5 0.001)
    (check-within (candidate (list 6 7 4)) 4 0.001)
    (check-within (candidate (list 8 7 4)) 3 0.001)
    (check-within (candidate (list 8 7 5)) 3 0.001)
    (check-within (candidate (list 8 9 5)) 4 0.001)
    (check-within (candidate (list 9 2 5)) 4 0.001)
    (check-within (candidate (list 9 5 2)) 3 0.001)
    (check-within (candidate (list 9 6 9)) 4 0.001)
    (check-within (candidate (list 9 9 4)) 3 0.001)
    (check-within (candidate (list 10 7 4)) 3 0.001)
    (check-within (candidate (list 10 10 6)) 3 0.001)
    (check-within (candidate (list 2 1 1 4)) 5 0.001)
    (check-within (candidate (list 2 5 7 9)) 10 0.001)
    (check-within (candidate (list 3 5 3 5)) 6 0.001)
    (check-within (candidate (list 3 7 10 6)) 6 0.001)
    (check-within (candidate (list 3 8 3 8)) 6 0.001)
    (check-within (candidate (list 4 1 3 7)) 6 0.001)
    (check-within (candidate (list 4 3 5 1)) 3 0.001)
    (check-within (candidate (list 4 3 7 5)) 4 0.001)
    (check-within (candidate (list 4 8 7 6)) 5 0.001)
    (check-within (candidate (list 4 9 10 6)) 6 0.001)
    (check-within (candidate (list 5 4 3 8)) 5 0.001)
    (check-within (candidate (list 5 5 9 5)) 3 0.001)
    (check-within (candidate (list 5 10 10 9)) 5 0.001)
    (check-within (candidate (list 6 4 4 9)) 5 0.001)
    (check-within (candidate (list 6 5 2 10)) 5 0.001)
    (check-within (candidate (list 7 3 2 3)) 4 0.001)
    (check-within (candidate (list 7 5 1 10)) 5 0.001)
    (check-within (candidate (list 7 9 7 2)) 4 0.001)
    (check-within (candidate (list 7 9 8 8)) 5 0.001)
    (check-within (candidate (list 7 10 4 3)) 4 0.001)
    (check-within (candidate (list 8 8 1 7)) 4 0.001)
    (check-within (candidate (list 9 2 8 7)) 3 0.001)
    (check-within (candidate (list 10 5 9 5)) 3 0.001)
    (check-within (candidate (list 10 7 2 8)) 4 0.001)
    (check-within (candidate (list 10 9 1 3)) 4 0.001)
    (check-within (candidate (list 1 1 4 4 7)) 6 0.001)
    (check-within (candidate (list 1 2 3 4 5)) 15 0.001)
    (check-within (candidate (list 1 2 8 9 2)) 7 0.001)
    (check-within (candidate (list 1 7 4 9 2)) 5 0.001)
    (check-within (candidate (list 2 2 4 6 3)) 4 0.001)
    (check-within (candidate (list 2 7 1 3 9)) 9 0.001)
    (check-within (candidate (list 3 1 9 6 5)) 4 0.001)
    (check-within (candidate (list 3 3 3 6 1)) 3 0.001)
    (check-within (candidate (list 3 7 1 8 1)) 4 0.001)
    (check-within (candidate (list 4 1 6 10 2)) 3 0.001)
    (check-within (candidate (list 5 4 8 4 2)) 3 0.001)
    (check-within (candidate (list 6 1 1 8 10)) 7 0.001)
))

(test-humaneval)