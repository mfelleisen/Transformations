#lang racket

;; A chef has collected data on the satisfaction level of his n dishes. Chef can cook any dish in 1 unit of time.
;; Like-time coefficient of a dish is defined as the time taken to cook that dish including previous dishes multiplied by its satisfaction level i.e. time[i] * satisfaction[i].
;; Return the maximum sum of like-time coefficient that the chef can obtain after dishes preparation.
;; Dishes can be prepared in any order and the chef can discard some dishes to get this maximum value.
;; Example 1:
;; Input: satisfaction = [-1,-8,0,5,-9]
;; Output: 14
;; Explanation: After Removing the second and last dish, the maximum total like-time coefficient will be equal to (-1*1 + 0*2 + 5*3 = 14).
;; Each dish is prepared in one unit of time.
;; Example 2:
;; Input: satisfaction = [4,3,2]
;; Output: 20
;; Explanation: Dishes can be prepared in any order, (2*1 + 3*2 + 4*3 = 20)
;; Example 3:
;; Input: satisfaction = [-1,-4,-5]
;; Output: 0
;; Explanation: People do not like the dishes. No dish is prepared.
;; Constraints:
;; n == satisfaction.length
;; 1 <= n <= 500
;; -1000 <= satisfaction[i] <= 1000
(define (max_satisfaction satisfaction)
  ;; First, sort the list of satisfaction in descending order
  (define sorted-satisfaction (sort satisfaction >))
  ;; Initialize the variables to keep track of the total satisfaction, cumulative satisfaction, and the answer
  (define total 0)
  (define sum 0)
  (define ans 0)
  ;; Iterate through the sorted satisfaction list
  (for ([i sorted-satisfaction])
    ;; Update the total satisfaction after adding each dish
    (set! total (+ total i))
    ;; Only if the total is positive, we consider adding the dish
    (when (> total 0)
      (set! sum (+ sum total))
      ;; Update the answer with the maximum sum obtained so far
      (set! ans (max ans sum))))
  ans)

;; The function iterates through the sorted satisfaction list in reverse order.
;; It calculates the cumulative satisfaction for adding each dish.
;; If the cumulative satisfaction is positive, it adds it to the sum and updates the answer with the maximum sum obtained.
;; This way, it ensures to achieve the maximum like-time coefficient.
(require rackunit)

(define (test-humaneval) 

  (let (( candidate max_satisfaction))
    (check-within (candidate (list 1 1 1 1 1 1 1 1 1 1)) 55 0.001)
    (check-within (candidate (list -1 -4 -5)) 0 0.001)
    (check-within (candidate (list -1 -8 0 5 -9)) 14 0.001)
    (check-within (candidate (list 0 0 0 0 0 0 0 0 0 0)) 0 0.001)
    (check-within (candidate (list -1 -2 -3 -4 -5 -6 -7 -8 -9 -10)) 0 0.001)
    (check-within (candidate (list )) 0 0.001)
    (check-within (candidate (list -10 -20 -30 -40 -50 -60 -70 -80 -90 -100)) 0 0.001)
    (check-within (candidate (list -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)) 0 0.001)
    (check-within (candidate (list -1000 -1000 -1000)) 0 0.001)
    (check-within (candidate (list 4 3 2)) 20 0.001)
))

(test-humaneval)