#lang racket

;; Function: minCost
;; Description: Calculates the minimum total cost to make all elements of nums equal,
;; each operation's cost on an element is determined by the corresponding element in cost.
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
  ;; Find the minimum value in nums as a starting point for comparison
  (let ([base (apply min nums)])
    ;; Initialize ans with a very large number to ensure any practical result will be smaller
    (let loop ([ans +inf.0] [target base])
      ;; If target exceeds base + 1, return the current ans as the minimum total cost found
      (if (> target (+ base 1))
          ans
          ;; Otherwise, calculate the total cost to make all elements in nums equal to the target
          (let ([local-res (for/fold ([res 0]) ([num (in-list nums)] [c (in-list cost)])
                              ;; Add the cost of making num equal to target to the accumulator
                              (+ res (* (abs (- num target)) c)))])
            ;; Recur with the smaller of ans and local-res, and increment the target
            (loop (min ans local-res) (+ target 1)))))))

;; The function minCost is defined to iterate over potential target values (starting from the minimum value in nums)
;; and calculate the cost required to make every element in nums equal to this target value.
;; It iterates over a small range (from the minimum value in nums to minimum value + 1) as the optimal solution
;; is guaranteed to be within this range due to the linearity of cost increase/decrease.
;; The total cost for each target value is calculated by summing up the individual costs
;; for each element in nums to reach the target value, considering the specific cost for each operation
;; as defined in the cost list. The minimum of these total costs is returned as the result.
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