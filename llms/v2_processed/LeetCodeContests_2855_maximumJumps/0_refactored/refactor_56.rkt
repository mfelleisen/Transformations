#lang racket

(provide (rename-out [maximumJumps max-jump]))

;; You are given a 0-indexed array nums of n integers and an integer target.
;; You are initially positioned at index 0. In one step, you can jump from index i to any index j such that:
;;  * 0 <= i < j < n
;;  * -target <= nums[j] - nums[i] <= target
;; Return the maximum number of jumps you can make to reach index n - 1.
;; If there is no way to reach index n - 1, return -1.

(define (maximumJumps nums target)
  (define n (length nums))
  (define dp (make-vector n -inf.0))
  (vector-set! dp 0 0)

  (define (update-dp i)
    (let ([current (vector-ref dp i)])
      (unless (= current -inf.0)
        (for ([j (in-range (add1 i) n)])
          (when (<= (abs (- (list-ref nums j) (list-ref nums i))) target)
            (vector-set! dp j (max (vector-ref dp j) (add1 current))))))))
  
  (for ([i (in-range n)])
    (update-dp i))
  
  (let ([result (vector-ref dp (sub1 n))])
    (if (= result -inf.0) -1 result)))

;; Examples
(maximumJumps '(1 3 6 4 1 2) 2)  ;; Output: 3
(maximumJumps '(1 3 6 4 1 2) 3)  ;; Output: 5
(maximumJumps '(1 3 6 4 1 2) 0)  ;; Output: -1

(require rackunit)


(define (test-humaneval) 

  (let (( candidate maximumJumps))
    (check-within (candidate (list 1 3 6 4 1 2) 2) 3 0.001)
    (check-within (candidate (list 1 3 6 4 1 2) 3) 5 0.001)
    (check-within (candidate (list 1 3 6 4 1 2) 0) -1 0.001)
    (check-within (candidate (list 0 1) 0) -1 0.001)
    (check-within (candidate (list 0 1) 1) 1 0.001)
    (check-within (candidate (list 0 1) 2) 1 0.001)
    (check-within (candidate (list 1 0) 0) -1 0.001)
    (check-within (candidate (list 1 0) 1) 1 0.001)
    (check-within (candidate (list 1 0) 2) 1 0.001)
    (check-within (candidate (list 0 1 2) 0) -1 0.001)
    (check-within (candidate (list 0 1 2) 1) 2 0.001)
    (check-within (candidate (list 0 1 2) 2) 2 0.001)
    (check-within (candidate (list 0 1 2) 3) 2 0.001)
    (check-within (candidate (list 0 2 1) 0) -1 0.001)
    (check-within (candidate (list 0 2 1) 1) 1 0.001)
    (check-within (candidate (list 0 2 1) 2) 2 0.001)
    (check-within (candidate (list 0 2 1) 3) 2 0.001)
    (check-within (candidate (list 1 0 2) 0) -1 0.001)
    (check-within (candidate (list 1 0 2) 1) 1 0.001)
    (check-within (candidate (list 1 0 2) 2) 2 0.001)
    (check-within (candidate (list 1 0 2) 3) 2 0.001)
    (check-within (candidate (list 1 2 0) 0) -1 0.001)
    (check-within (candidate (list 1 2 0) 1) 1 0.001)
    (check-within (candidate (list 1 2 0) 2) 2 0.001)
    (check-within (candidate (list 1 2 0) 3) 2 0.001)
    (check-within (candidate (list 2 0 1) 0) -1 0.001)
    (check-within (candidate (list 2 0 1) 1) 1 0.001)
    (check-within (candidate (list 2 0 1) 2) 2 0.001)
    (check-within (candidate (list 2 0 1) 3) 2 0.001)
    (check-within (candidate (list 2 1 0) 0) -1 0.001)
    (check-within (candidate (list 2 1 0) 1) 2 0.001)
    (check-within (candidate (list 2 1 0) 2) 2 0.001)
    (check-within (candidate (list 2 1 0) 3) 2 0.001)
    (check-within (candidate (list 0 1 2 3) 0) -1 0.001)
    (check-within (candidate (list 0 1 2 3) 1) 3 0.001)
    (check-within (candidate (list 0 1 2 3) 2) 3 0.001)
    (check-within (candidate (list 0 1 2 3) 3) 3 0.001)
    (check-within (candidate (list 0 1 2 3) 4) 3 0.001)
    (check-within (candidate (list 0 1 3 2) 0) -1 0.001)
    (check-within (candidate (list 0 1 3 2) 1) 2 0.001)
    (check-within (candidate (list 0 1 3 2) 2) 3 0.001)
    (check-within (candidate (list 0 1 3 2) 3) 3 0.001)
    (check-within (candidate (list 0 1 3 2) 4) 3 0.001)
    (check-within (candidate (list 0 2 1 3) 0) -1 0.001)
    (check-within (candidate (list 0 2 1 3) 1) -1 0.001)
    (check-within (candidate (list 0 2 1 3) 2) 3 0.001)
    (check-within (candidate (list 0 2 1 3) 3) 3 0.001)
    (check-within (candidate (list 0 2 1 3) 4) 3 0.001)
    (check-within (candidate (list 0 2 3 1) 0) -1 0.001)
    (check-within (candidate (list 0 2 3 1) 1) 1 0.001)
    (check-within (candidate (list 0 2 3 1) 2) 3 0.001)
    (check-within (candidate (list 0 2 3 1) 3) 3 0.001)
    (check-within (candidate (list 0 2 3 1) 4) 3 0.001)
    (check-within (candidate (list 0 3 1 2) 0) -1 0.001)
    (check-within (candidate (list 0 3 1 2) 1) 2 0.001)
    (check-within (candidate (list 0 3 1 2) 2) 2 0.001)
    (check-within (candidate (list 0 3 1 2) 3) 3 0.001)
    (check-within (candidate (list 0 3 1 2) 4) 3 0.001)
    (check-within (candidate (list 0 3 2 1) 0) -1 0.001)
    (check-within (candidate (list 0 3 2 1) 1) 1 0.001)
    (check-within (candidate (list 0 3 2 1) 2) 2 0.001)
    (check-within (candidate (list 0 3 2 1) 3) 3 0.001)
    (check-within (candidate (list 0 3 2 1) 4) 3 0.001)
    (check-within (candidate (list 1 0 2 3) 0) -1 0.001)
    (check-within (candidate (list 1 0 2 3) 1) 2 0.001)
    (check-within (candidate (list 1 0 2 3) 2) 3 0.001)
    (check-within (candidate (list 1 0 2 3) 3) 3 0.001)
    (check-within (candidate (list 1 0 2 3) 4) 3 0.001)
    (check-within (candidate (list 1 0 3 2) 0) -1 0.001)
    (check-within (candidate (list 1 0 3 2) 1) 1 0.001)
    (check-within (candidate (list 1 0 3 2) 2) 2 0.001)
    (check-within (candidate (list 1 0 3 2) 3) 3 0.001)
    (check-within (candidate (list 1 0 3 2) 4) 3 0.001)
    (check-within (candidate (list 1 2 0 3) 0) -1 0.001)
    (check-within (candidate (list 1 2 0 3) 1) 2 0.001)
    (check-within (candidate (list 1 2 0 3) 2) 2 0.001)
    (check-within (candidate (list 1 2 0 3) 3) 3 0.001)
    (check-within (candidate (list 1 2 0 3) 4) 3 0.001)
    (check-within (candidate (list 1 2 3 0) 0) -1 0.001)
    (check-within (candidate (list 1 2 3 0) 1) 1 0.001)
    (check-within (candidate (list 1 2 3 0) 2) 2 0.001)
    (check-within (candidate (list 1 2 3 0) 3) 3 0.001)
    (check-within (candidate (list 1 2 3 0) 4) 3 0.001)
    (check-within (candidate (list 1 3 0 2) 0) -1 0.001)
    (check-within (candidate (list 1 3 0 2) 1) 1 0.001)
    (check-within (candidate (list 1 3 0 2) 2) 2 0.001)
    (check-within (candidate (list 1 3 0 2) 3) 3 0.001)
    (check-within (candidate (list 1 3 0 2) 4) 3 0.001)
    (check-within (candidate (list 1 3 2 0) 0) -1 0.001)
    (check-within (candidate (list 1 3 2 0) 1) 1 0.001)
    (check-within (candidate (list 1 3 2 0) 2) 3 0.001)
    (check-within (candidate (list 1 3 2 0) 3) 3 0.001)
    (check-within (candidate (list 1 3 2 0) 4) 3 0.001)
    (check-within (candidate (list 2 0 1 3) 0) -1 0.001)
    (check-within (candidate (list 2 0 1 3) 1) 1 0.001)
    (check-within (candidate (list 2 0 1 3) 2) 3 0.001)
    (check-within (candidate (list 2 0 1 3) 3) 3 0.001)
    (check-within (candidate (list 2 0 1 3) 4) 3 0.001)
    (check-within (candidate (list 2 0 3 1) 0) -1 0.001)
    (check-within (candidate (list 2 0 3 1) 1) 1 0.001)
))

(test-humaneval)