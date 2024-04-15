#lang racket

;; You are given two 0-indexed lists nums and cost consisting each of n positive integers.
;; You can do the following operation any number of times:
;; Increase or decrease any element of the list nums by 1.
;; The cost of doing one operation on the ith element is cost[i].
;; Return the minimum total cost such that all the elements of the list nums become equal.
;; Example 1:
;; Input: nums = [1,3,5,2], cost = [2,3,1,14]
;; Output: 8
;; Explanation: We can make all the elements equal to 2 in the following way:
;; - Increase the 0th element one time. The cost is 2.
;; - Decrease the 1st element one time. The cost is 3.
;; - Decrease the 2nd element three times. The cost is 1 + 1 + 1 = 3.
;; The total cost is 2 + 3 + 3 = 8.
;; It can be shown that we cannot make the list equal with a smaller cost.
;; Example 2:
;; Input: nums = [2,2,2,2,2], cost = [4,2,8,1,3]
;; Output: 0
;; Explanation: All the elements are already equal, so no operations are needed.
;; Constraints:
;; n == nums.length == cost.length
;; 1 <= n <= 105
;; 1 <= nums[i], cost[i] <= 106
;; Test cases are generated in a way that the output doesn't exceed 253-1
(define (minCost nums cost)
  ;; First, find the minimum value in the `nums` list to establish a base value for comparison.
  (let ([base (apply min nums)])
    ;; Initialize `ans` to a large number to ensure it's higher than any possible result.
    (let loop ([ans +inf.0] [target base])
      ;; If the target exceeds base + 1, it means we've checked all necessary target values (base and base + 1),
      ;; so we return the minimum cost found.
      (if (> target (+ base 1))
          ans
          ;; Otherwise, calculate the cost for making all elements equal to the current target value.
          (let ([local-res (for/fold ([res 0]) ([num (in-list nums)] [c (in-list cost)])
                             (+ res (* (abs (- num target)) c)))])
            ;; Compare the calculated cost with the current minimum and update if it's lower.
            ;; Then, recursively call `loop` with the updated minimum cost and the next target value.
            (loop (min ans local-res) (+ target 1)))))))

;; Example usage:
(minCost '(1 3 5 2) '(2 3 1 14)) ; Expected output: 8
(minCost '(2 2 2 2 2) '(4 2 8 1 3)) ; Expected output: 0
(require rackunit)

(define (test-humaneval) 

  (let (( candidate minCost))
    (check-within (candidate (list 1 3 5 2) (list 2 3 1 14)) 8 0.001)
    (check-within (candidate (list 1 1 1 1 1) (list 1 2 3 4 5)) 0 0.001)
    (check-within (candidate (list 2 1 1 1 1) (list 1 2 3 4 5)) 1 0.001)
    (check-within (candidate (list 1 1 1 1 100) (list 1 1 1 1 1)) 99 0.001)
    (check-within (candidate (list 2 2 2 2 2) (list 4 2 8 1 3)) 0 0.001)
    (check-within (candidate (list 1 3 5 2) (list 2 3 1 14)) 8 0.001)
    (check-within (candidate (list 1 1 1 1 100) (list 1 1 1 1 1)) 99 0.001)
    (check-within (candidate (list 3 3 3 3 3) (list 1 2 3 4 5)) 0 0.001)
    (check-within (candidate (list 2 2 2 2 2) (list 4 2 8 1 3)) 0 0.001)
    (check-within (candidate (list 3 3 3 3 3) (list 1 2 3 4 5)) 0 0.001)
    (check-within (candidate (list 1 1 1 1 1) (list 1 2 3 4 5)) 0 0.001)
    (check-within (candidate (list 2 1 1 1 1) (list 1 2 3 4 5)) 1 0.001)
))

(test-humaneval)