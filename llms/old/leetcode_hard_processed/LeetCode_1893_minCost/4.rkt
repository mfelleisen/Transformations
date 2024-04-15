#lang racket

;; Function to calculate the minimum total cost to make all elements in nums equal.
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
  ;; Find the minimum value in nums as a starting point.
  (let* ((base (apply min nums))
         ;; Initialize ans with the highest possible value.
         (ans (foldl (lambda (num acc) (min acc num))
                     +inf.0
                     ;; Create a list of target values to check (base and base + 1).
                     (map (lambda (target)
                            ;; Calculate the cost for each target and choose the minimum.
                            (foldl (lambda (i local-res)
                                     ;; Calculate the difference times cost for each element.
                                     (+ local-res (* (abs (- (list-ref nums i) target))
                                                     (list-ref cost i))))
                                   0
                                   (range (length nums))))
                          (list base (+ base 1))))))
    ans))

;; The function takes two lists, nums and cost, and computes the minimum cost required
;; to make all elements in nums equal. It does so by checking the cost for making all
;; elements equal to either the minimum element in nums or the minimum element plus one,
;; since these are the only candidates that need to be checked for the optimal solution.
;; The cost for each element is calculated by multiplying the difference from the target
;; by the operation cost, and the total cost is the sum of these individual costs.
;; The minimum of these total costs is returned as the answer.
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