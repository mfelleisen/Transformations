#lang racket

(require racket/list) ; Required for list manipulation functions

;; There are n workers. You are given two integer lists quality and wage where quality[i] is the quality of the ith worker and wage[i] is the minimum wage expectation for the ith worker.
;; We want to hire exactly k workers to form a paid group. To hire a group of k workers, we must pay them according to the following rules:
;; Every worker in the paid group should be paid in the ratio of their quality compared to other workers in the paid group.
;; Every worker in the paid group must be paid at least their minimum wage expectation.
;; Given the integer k, return the least amount of money needed to form a paid group satisfying the above conditions. Answers within 10-5 of the actual answer will be accepted.
;; Example 1:
;; Input: quality = [10,20,5], wage = [70,50,30], k = 2
;; Output: 105.00000
;; Explanation: We pay 70 to 0th worker and 35 to 2nd worker.
;; Example 2:
;; Input: quality = [3,1,10,10,1], wage = [4,8,2,2,7], k = 3
;; Output: 30.66667
;; Explanation: We pay 4 to 0th worker, 13.33333 to 2nd and 3rd workers separately.
;; Constraints:
;; n == quality.length == wage.length
;; 1 <= k <= n <= 104
;; 1 <= quality[i], wage[i] <= 104
(define (mincost_to_hire_workers quality wage k)
  ;; Create a list of workers with their wage/quality ratio and quality, and sort it by the ratio
  (define workers (sort (map (lambda (w q) (list (/ w q) q)) wage quality)
                        < #:key car))
  ;; Initialize variables to keep track of total quality and the minimum cost
  (define total-quality 0)
  ;; Use a max heap represented as a sorted list in reverse order to keep track of the highest qualities
  (define max-heap '())
  ;; Result starts as infinity, representing the minimum cost found so far
  (define result +inf.0)

  ;; Iterate through each worker
  (for ([worker workers])
    (let* ((ratio (car worker)) ; Extract ratio
           (q (cadr worker)))   ; Extract quality
      ;; Update the total quality and max-heap
      (set! total-quality (+ total-quality q))
      (set! max-heap (sort (cons q max-heap) >))
      ;; If we have more than k workers, remove the one with the highest quality
      (when (> (length max-heap) k)
        (set! total-quality (- total-quality (car max-heap)))
        (set! max-heap (cdr max-heap)))
      ;; If we have exactly k workers, calculate the current cost and update the result if it's lower
      (when (= (length max-heap) k)
        (set! result (min result (* total-quality ratio))))))
  result)

;; Example usage:
(mincost_to_hire_workers '(10 20 5) '(70 50 30) 2) ; => 105.0
(mincost_to_hire_workers '(3 1 10 10 1) '(4 8 2 2 7) 3) ; => 30.666666666666668
(require rackunit)

(define (test-humaneval) 

  (let (( candidate mincost_to_hire_workers))
    (check-within (candidate (list 4) (list 8) 1) 8.0 0.001)
    (check-within (candidate (list 3 1 10 10 1) (list 4 8 2 2 7) 3) 30.666666666666664 0.001)
    (check-within (candidate (list 100000000000000000000 100000000000000000000 100000000000000000000) (list 100000000000000000000 100000000000000000000 100000000000000000000) 1) 100000000000000000000 0.001)
    (check-within (candidate (list 10 20 5) (list 70 50 30) 2) 105.0 0.001)
))

(test-humaneval)