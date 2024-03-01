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
  ;; Calculate the minimum total cost to make all elements of nums equal,
  ;; where each operation's cost on the ith element is given by cost[i].
  
  ;; Find the minimum value in nums as a starting point
  (define base (apply min nums))
  
  ;; Define a helper function to calculate the total cost for making all
  ;; elements equal to a target value.
  (define (calc-total-cost target)
    (foldl (lambda (num-cost acc)
             (let ((num (car num-cost))
                   (cst (cdr num-cost)))
               (+ acc (* (abs (- num target)) cst))))
           0
           (map cons nums cost)))
  
  ;; Iterate through the possible target values (base and base + 1) and find
  ;; the minimum total cost.
  (define ans
    (foldl (lambda (target acc)
             (min acc (calc-total-cost target)))
           +inf.0
           (list base (+ base 1))))
  
  ans)

;; Examples of calling the function
;; (minCost '(1 3 5 2) '(2 3 1 14)) should return 8
;; (minCost '(2 2 2 2 2) '(4 2 8 1 3)) should return 0
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