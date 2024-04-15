#lang racket

;; You are given a list tasks where tasks[i] = [actuali, minimumi]:
;; actuali is the actual amount of energy you spend to finish the ith task.
;; minimumi is the minimum amount of energy you require to begin the ith task.
;; For example, if the task is [10, 12] and your current energy is 11, you cannot start this task. However, if your current energy is 13, you can complete this task, and your energy will be 3 after finishing it.
;; You can finish the tasks in any order you like.
;; Return the minimum initial amount of energy you will need to finish all the tasks.
;; Example 1:
;; Input: tasks = [[1,2],[2,4],[4,8]]
;; Output: 8
;; Explanation:
;; Starting with 8 energy, we finish the tasks in the following order:
;;     - 3rd task. Now energy = 8 - 4 = 4.
;;     - 2nd task. Now energy = 4 - 2 = 2.
;;     - 1st task. Now energy = 2 - 1 = 1.
;; Notice that even though we have leftover energy, starting with 7 energy does not work because we cannot do the 3rd task.
;; Example 2:
;; Input: tasks = [[1,3],[2,4],[10,11],[10,12],[8,9]]
;; Output: 32
;; Explanation:
;; Starting with 32 energy, we finish the tasks in the following order:
;;     - 1st task. Now energy = 32 - 1 = 31.
;;     - 2nd task. Now energy = 31 - 2 = 29.
;;     - 3rd task. Now energy = 29 - 10 = 19.
;;     - 4th task. Now energy = 19 - 10 = 9.
;;     - 5th task. Now energy = 9 - 8 = 1.
;; Example 3:
;; Input: tasks = [[1,7],[2,8],[3,9],[4,10],[5,11],[6,12]]
;; Output: 27
;; Explanation:
;; Starting with 27 energy, we finish the tasks in the following order:
;;     - 5th task. Now energy = 27 - 5 = 22.
;;     - 2nd task. Now energy = 22 - 2 = 20.
;;     - 3rd task. Now energy = 20 - 3 = 17.
;;     - 1st task. Now energy = 17 - 1 = 16.
;;     - 4th task. Now energy = 16 - 4 = 12.
;;     - 6th task. Now energy = 12 - 6 = 6.
;; Constraints:
;; 1 <= tasks.length <= 105
;; 1 <= actual​i <= minimumi <= 104
(define (minimumEffort tasks)
  ; First, sort tasks by the difference between minimum energy required and actual energy spent, in descending order.
  (define sorted-tasks
    (sort tasks
          (lambda (task1 task2)
            (> (- (second task1) (first task1))
               (- (second task2) (first task2))))))
  
  ; Initialize the accumulator for the sum of actual energies spent and the answer variable.
  (define sum-energy 0)
  (define ans 0)
  
  ; Iterate over the sorted tasks to determine the minimum initial energy required.
  (for-each
   (lambda (task)
     (set! ans (max ans (+ sum-energy (second task))))
     (set! sum-energy (+ sum-energy (first task))))
   sorted-tasks)
  
  ; Return the calculated minimum initial energy required.
  ans)

;; Example usage:
(minimumEffort '((1 2) (2 4) (4 8)))
(minimumEffort '((1 3) (2 4) (10 11) (10 12) (8 9)))
(minimumEffort '((1 7) (2 8) (3 9) (4 10) (5 11) (6 12)))
(require rackunit)

(define (test-humaneval) 

  (let (( candidate minimumEffort))
    (check-within (candidate (list (list 1 7) (list 2 8) (list 3 9) (list 4 10) (list 5 11) (list 6 12))) 27 0.001)
    (check-within (candidate (list (list 1 3) (list 2 4) (list 10 11) (list 10 12) (list 8 9))) 32 0.001)
    (check-within (candidate (list (list 5 10) (list 10 20) (list 20 40))) 40 0.001)
    (check-within (candidate (list (list 1 2) (list 2 4) (list 4 8))) 8 0.001)
    (check-within (candidate (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5) (list 6 6) (list 7 7) (list 8 8) (list 9 9) (list 10 10))) 55 0.001)
    (check-within (candidate (list (list 1 3) (list 5 8))) 8 0.001)
    (check-within (candidate (list (list 1 1))) 1 0.001)
    (check-within (candidate (list (list 1 2) (list 1 2) (list 1 2) (list 1 2) (list 1 2) (list 1 2) (list 1 2) (list 1 2) (list 1 2) (list 1 2) (list 1 2))) 12 0.001)
    (check-within (candidate (list (list 2 2) (list 3 3) (list 4 4) (list 5 5))) 14 0.001)
))

(test-humaneval)