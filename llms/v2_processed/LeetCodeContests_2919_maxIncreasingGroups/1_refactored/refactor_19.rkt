#lang racket

;; Function: maxIncreasingGroups
;; Input: usageLimits - a list of integers representing the maximum usage limits for each index
;; Output: The maximum number of strictly increasing size groups that can be formed
;; You are given a 0-indexed array usageLimits of length n.
;; Your task is to create groups using numbers from 0 to n - 1, ensuring that each number, i, is used no more than usageLimits[i] times in total across all groups. You must also satisfy the following conditions:
;;  * Each group must consist of distinct numbers, meaning that no duplicate numbers are allowed within a single group.
;;  * Each group (except the first one) must have a length strictly greater than the previous group.
;; Return an integer denoting the maximum number of groups you can create while satisfying these conditions.
;; Example 1:
;; Input: usageLimits = [1,2,5]
;; Output: 3
;; Explanation: In this example, we can use 0 at most once, 1 at most twice, and 2 at most five times.
;; One way of creating the maximum number of groups while satisfying the conditions is:
;; Group 1 contains the number [2].
;; Group 2 contains the numbers [1,2].
;; Group 3 contains the numbers [0,1,2].
;; It can be shown that the maximum number of groups is 3.
;; So, the output is 3.
;; Example 2:
;; Input: usageLimits = [2,1,2]
;; Output: 2
;; Explanation: In this example, we can use 0 at most twice, 1 at most once, and 2 at most twice.
;; One way of creating the maximum number of groups while satisfying the conditions is:
;; Group 1 contains the number [0].
;; Group 2 contains the numbers [1,2].
;; It can be shown that the maximum number of groups is 2.
;; So, the output is 2.
;; Example 3:
;; Input: usageLimits = [1,1]
;; Output: 1
;; Explanation: In this example, we can use both 0 and 1 at most once.
;; One way of creating the maximum number of groups while satisfying the conditions is:
;; Group 1 contains the number [0].
;; It can be shown that the maximum number of groups is 1.
;; So, the output is 1.
;; Constraints:
;;  * 1 <= usageLimits.length <= 105
;;  * 1 <= usageLimits[i] <= 109
(define (maxIncreasingGroups usageLimits)
  ;; Sort the usageLimits list to facilitate group creation
  (define sorted-limits (sort usageLimits <))

  ;; Helper function to iterate over the sorted limits and count groups
  (define (count-groups limits current-min-size total num-groups)
    (match limits
      ;; Base case: If we have no more limits left, return the number of groups
      ['() num-groups]
      ;; Recursive case: Process the current limit
      [(cons current-limit rest-limits)
       (define new-total (+ total current-limit))
       ;; Check if we can form a group of at least `current-min-size`
       (if (>= new-total current-min-size)
           ;; If yes, form a new group and recurse with updated parameters
           (count-groups rest-limits (+ current-min-size 1) (- new-total current-min-size) (+ num-groups 1))
           ;; If no, just move to the next limit without forming a group
           (count-groups rest-limits current-min-size new-total num-groups))]))

  ;; Start the recursive counting with initial parameters
  (count-groups sorted-limits 1 0 0))

;; Example usage
(maxIncreasingGroups '(1 2 5))  ; Output: 3
(maxIncreasingGroups '(2 1 2))  ; Output: 2
(maxIncreasingGroups '(1 1))    ; Output: 1

(require rackunit)


(define (test-humaneval) 

  (let (( candidate maxIncreasingGroups))
    (check-within (candidate (list 1 2 5)) 3 0.001)
    (check-within (candidate (list 2 1 2)) 2 0.001)
    (check-within (candidate (list 1 1)) 1 0.001)
    (check-within (candidate (list 1 4)) 2 0.001)
    (check-within (candidate (list 1 5)) 2 0.001)
    (check-within (candidate (list 1 7)) 2 0.001)
    (check-within (candidate (list 1 8)) 2 0.001)
    (check-within (candidate (list 2 1)) 2 0.001)
    (check-within (candidate (list 2 2)) 2 0.001)
    (check-within (candidate (list 2 3)) 2 0.001)
    (check-within (candidate (list 2 4)) 2 0.001)
    (check-within (candidate (list 2 5)) 2 0.001)
    (check-within (candidate (list 2 7)) 2 0.001)
    (check-within (candidate (list 2 8)) 2 0.001)
    (check-within (candidate (list 2 9)) 2 0.001)
    (check-within (candidate (list 3 1)) 2 0.001)
    (check-within (candidate (list 3 4)) 2 0.001)
    (check-within (candidate (list 3 7)) 2 0.001)
    (check-within (candidate (list 3 10)) 2 0.001)
    (check-within (candidate (list 4 1)) 2 0.001)
    (check-within (candidate (list 4 2)) 2 0.001)
    (check-within (candidate (list 4 4)) 2 0.001)
    (check-within (candidate (list 4 5)) 2 0.001)
    (check-within (candidate (list 4 7)) 2 0.001)
    (check-within (candidate (list 4 10)) 2 0.001)
    (check-within (candidate (list 5 8)) 2 0.001)
    (check-within (candidate (list 5 10)) 2 0.001)
    (check-within (candidate (list 6 2)) 2 0.001)
    (check-within (candidate (list 6 3)) 2 0.001)
    (check-within (candidate (list 6 4)) 2 0.001)
    (check-within (candidate (list 6 5)) 2 0.001)
    (check-within (candidate (list 6 6)) 2 0.001)
    (check-within (candidate (list 6 7)) 2 0.001)
    (check-within (candidate (list 6 9)) 2 0.001)
    (check-within (candidate (list 6 19)) 2 0.001)
    (check-within (candidate (list 7 1)) 2 0.001)
    (check-within (candidate (list 7 2)) 2 0.001)
    (check-within (candidate (list 7 3)) 2 0.001)
    (check-within (candidate (list 7 4)) 2 0.001)
    (check-within (candidate (list 7 7)) 2 0.001)
    (check-within (candidate (list 7 13)) 2 0.001)
    (check-within (candidate (list 8 3)) 2 0.001)
    (check-within (candidate (list 8 6)) 2 0.001)
    (check-within (candidate (list 9 1)) 2 0.001)
    (check-within (candidate (list 9 3)) 2 0.001)
    (check-within (candidate (list 9 4)) 2 0.001)
    (check-within (candidate (list 9 6)) 2 0.001)
    (check-within (candidate (list 9 8)) 2 0.001)
    (check-within (candidate (list 9 10)) 2 0.001)
    (check-within (candidate (list 10 2)) 2 0.001)
    (check-within (candidate (list 10 3)) 2 0.001)
    (check-within (candidate (list 10 8)) 2 0.001)
    (check-within (candidate (list 10 11)) 2 0.001)
    (check-within (candidate (list 13 11)) 2 0.001)
    (check-within (candidate (list 13 13)) 2 0.001)
    (check-within (candidate (list 16 9)) 2 0.001)
    (check-within (candidate (list 18 6)) 2 0.001)
    (check-within (candidate (list 32 42)) 2 0.001)
    (check-within (candidate (list 1 1 5)) 2 0.001)
    (check-within (candidate (list 1 1 10)) 2 0.001)
    (check-within (candidate (list 1 4 3)) 3 0.001)
    (check-within (candidate (list 1 4 5)) 3 0.001)
    (check-within (candidate (list 1 6 4)) 3 0.001)
    (check-within (candidate (list 1 6 8)) 3 0.001)
    (check-within (candidate (list 1 7 19)) 3 0.001)
    (check-within (candidate (list 1 8 6)) 3 0.001)
    (check-within (candidate (list 1 9 5)) 3 0.001)
    (check-within (candidate (list 1 9 6)) 3 0.001)
    (check-within (candidate (list 1 10 6)) 3 0.001)
    (check-within (candidate (list 2 2 2)) 3 0.001)
    (check-within (candidate (list 2 3 8)) 3 0.001)
    (check-within (candidate (list 2 6 10)) 3 0.001)
    (check-within (candidate (list 2 7 2)) 3 0.001)
    (check-within (candidate (list 2 7 7)) 3 0.001)
    (check-within (candidate (list 2 8 7)) 3 0.001)
    (check-within (candidate (list 2 9 9)) 3 0.001)
    (check-within (candidate (list 3 1 1)) 2 0.001)
    (check-within (candidate (list 3 5 5)) 3 0.001)
    (check-within (candidate (list 3 5 8)) 3 0.001)
    (check-within (candidate (list 3 6 5)) 3 0.001)
    (check-within (candidate (list 3 7 4)) 3 0.001)
    (check-within (candidate (list 3 7 10)) 3 0.001)
    (check-within (candidate (list 3 8 1)) 3 0.001)
    (check-within (candidate (list 3 9 9)) 3 0.001)
    (check-within (candidate (list 3 10 9)) 3 0.001)
    (check-within (candidate (list 4 2 5)) 3 0.001)
    (check-within (candidate (list 4 2 15)) 3 0.001)
    (check-within (candidate (list 4 5 5)) 3 0.001)
    (check-within (candidate (list 4 7 9)) 3 0.001)
    (check-within (candidate (list 4 8 2)) 3 0.001)
    (check-within (candidate (list 4 8 4)) 3 0.001)
    (check-within (candidate (list 4 10 3)) 3 0.001)
    (check-within (candidate (list 4 10 4)) 3 0.001)
    (check-within (candidate (list 5 1 5)) 3 0.001)
    (check-within (candidate (list 5 2 9)) 3 0.001)
    (check-within (candidate (list 5 2 10)) 3 0.001)
    (check-within (candidate (list 5 6 1)) 3 0.001)
    (check-within (candidate (list 5 6 5)) 3 0.001)
    (check-within (candidate (list 5 7 4)) 3 0.001)
    (check-within (candidate (list 5 10 3)) 3 0.001)
))

(test-humaneval)