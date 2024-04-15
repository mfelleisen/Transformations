#lang racket

;; Given an unsorted integer list nums, return the smallest missing positive integer.
;; You must implement an algorithm that runs in O(n) time and uses O(1) auxiliary space.
;; Example 1:
;; Input: nums = [1,2,0]
;; Output: 3
;; Explanation: The numbers in the range [1,2] are all in the list.
;; Example 2:
;; Input: nums = [3,4,-1,1]
;; Output: 2
;; Explanation: 1 is in the list but 2 is missing.
;; Example 3:
;; Input: nums = [7,8,9,11,12]
;; Output: 1
;; Explanation: The smallest positive integer 1 is missing.
;; Constraints:
;; 1 <= nums.length <= 105
;; -231 <= nums[i] <= 231 - 1
(define (firstMissingPositive nums)
  ;; Function to swap elements in the list, given their indices.
  (define (swap lst i j)
    (for/list ([k (length lst)])
      (cond [(= k i) (list-ref lst j)]
            [(= k j) (list-ref lst i)]
            [else (list-ref lst k)])))
  ;; Function to place each positive integer in its corresponding index (i.e., value 1 at index 0, value 2 at index 1, etc.).
  (define (place-integers lst)
    (let loop ([i 0] [lst lst])
      (if (< i (length lst))
          (let* ([val (list-ref lst i)]
                 [pos (- val 1)])
            (if (and (<= 1 val) (<= val (length lst)) (not (= val (list-ref lst pos))))
                (loop i (swap lst i pos))  ; Swap and continue without incrementing `i` if the condition is met.
                (loop (+ i 1) lst)))  ; Otherwise, move to the next index.
          lst)))  ; Return the modified list when done.
  ;; Function to find the first missing positive.
  (define (find-first-missing lst)
    (for/or ([i (length lst)])
      (if (not (= (+ i 1) (list-ref lst i)))
          (+ i 1)
          #f)))
  ;; Main logic starts here.
  (let* ([n (length nums)]
         [placed-ints (place-integers nums)]
         [missing (find-first-missing placed-ints)])
    (if missing
        missing
        (+ n 1))))  ; If all positions are correct, the missing positive is `n + 1`.

;; Example usage
(firstMissingPositive '(1 2 0))  ; Output: 3
(firstMissingPositive '(3 4 -1 1))  ; Output: 2
(firstMissingPositive '(7 8 9 11 12))  ; Output: 1
(require rackunit)

(define (test-humaneval) 

  (let (( candidate firstMissingPositive))
    (check-within (candidate (list 3 4 -1 1)) 2 0.001)
    (check-within (candidate (list 5 4 3 2 1)) 6 0.001)
    (check-within (candidate (list 1 1 1 1)) 2 0.001)
    (check-within (candidate (list 1)) 2 0.001)
    (check-within (candidate (list 2)) 1 0.001)
    (check-within (candidate (list 1 2 5 7 9 11)) 3 0.001)
    (check-within (candidate (list 7 8 9 11 12)) 1 0.001)
    (check-within (candidate (list 1 2 3 4 5)) 6 0.001)
    (check-within (candidate (list 1 2 3 5 5 6 6 7 8 9 10)) 4 0.001)
    (check-within (candidate (list 0 2 1 4 3 6 5 -2 7 9 8 11 10 12 13 14 15 16 17 18)) 19 0.001)
    (check-within (candidate (list 3 4 -1 1)) 2 0.001)
    (check-within (candidate (list 1 1 1 1 1 2 2 2 2 2 3 3 3 3 3)) 4 0.001)
    (check-within (candidate (list 1 1 1 1)) 2 0.001)
    (check-within (candidate (list -1 -2 -3 -4)) 1 0.001)
    (check-within (candidate (list 7 8 9 11 12)) 1 0.001)
    (check-within (candidate (list -1)) 1 0.001)
    (check-within (candidate (list -2 -3 -5 -1 -7 -10 -11 -12 -13 -15 -16 -17 -18 -19 -20 2 1 6 7 3)) 4 0.001)
    (check-within (candidate (list 0)) 1 0.001)
    (check-within (candidate (list 1 2 3 4 5 7 8 9 10)) 6 0.001)
    (check-within (candidate (list 1 2 0)) 3 0.001)
    (check-within (candidate (list 2 3)) 1 0.001)
    (check-within (candidate (list 2 2 2 2 3 3 3 3)) 1 0.001)
    (check-within (candidate (list 1 1 1 1 1 2 2 2 2 2 3 3 3 3 3)) 4 0.001)
    (check-within (candidate (list 1 2 0)) 3 0.001)
    (check-within (candidate (list 0)) 1 0.001)
    (check-within (candidate (list 100 101 102 103 104)) 1 0.001)
    (check-within (candidate (list 2 5 6 8 11 15)) 1 0.001)
    (check-within (candidate (list 2)) 1 0.001)
    (check-within (candidate (list 0 0 0 0)) 1 0.001)
    (check-within (candidate (list 1 1 2 2 3 3)) 4 0.001)
    (check-within (candidate (list 1 2 3 5 5 6 6 7 8 9 10)) 4 0.001)
    (check-within (candidate (list -1)) 1 0.001)
    (check-within (candidate (list 1 2 5 7 9 11)) 3 0.001)
    (check-within (candidate (list 5 4 3 2 1)) 6 0.001)
))

(test-humaneval)