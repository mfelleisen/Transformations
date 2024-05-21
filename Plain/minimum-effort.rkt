#lang racket

(require "../testing-2.rkt")

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai2
;;
(define (me-ai2 tasks)
  ;; First, sort the tasks based on the difference between minimumi and actuali in descending order.
  ;; This allows us to prioritize tasks that require a higher initial energy, which is crucial for minimizing the total initial energy required.
  (define sorted-tasks (sort tasks
                             (lambda (task1 task2)
                               (> (- (second task1) (first task1))
                                  (- (second task2) (first task2))))))
  ;; Initialize ans to 0 and sum-energy to 0. 
  ;; ans will keep track of the maximum energy required at any point, and sum-energy will keep track of the total energy spent.
  (define ans 0)
  (define sum-energy 0)
  ;; Iterate over the sorted tasks to calculate the minimum initial energy required.
  (for-each (lambda (task)
              (set! ans (max ans (+ sum-energy (second task))))
              (set! sum-energy (+ sum-energy (first task))))
            sorted-tasks)
  ans)



;; ---------------------------------------------------------------------------------------------------
;; MODULE ai1
;;
(define (me-ai1 tasks)
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



;; ---------------------------------------------------------------------------------------------------
;; MODULE ai0
;;
(define (me-ai0 tasks)
  ;; First, sort the tasks based on the difference between the minimum energy required and the actual energy spent, in descending order.
  (define sorted-tasks
    (sort tasks
          (lambda (task1 task2)
            (> (- (second task1) (first task1))
               (- (second task2) (first task2))))))
  
  ;; Initialize the answer and sum of energies spent to 0.
  (define ans 0)
  (define sum-energy 0)
  
  ;; Iterate through each task in the sorted list.
  (for ([task sorted-tasks])
    ;; Update the answer to be the maximum of the current answer and
    ;; the sum of energies spent plus the minimum energy required for the current task.
    (set! ans (max ans (+ sum-energy (second task))))
    ;; Add the actual energy spent on the current task to the sum of energies.
    (set! sum-energy (+ sum-energy (first task))))
  
  ;; Return the final answer.
  ans)

;; ---------------------------------------------------------------------------------------------------
;; MODULE HIGH
;;
(define (me-HIGH tasks)

   (define sorted (sort tasks >= #:key (λ (e) (- (second e) (first e)))))

  (define-values (credit total)
    (for/fold ([credit 0]
               [total 0])
              ([task sorted])
      (define threshold (second task))
      (define cost (first task))
      (define re-up
        (if (< credit threshold)
            (- threshold credit)
            0))
      (values (- (+ credit re-up) cost) (+ total re-up))))

  total)

;; ---------------------------------------------------------------------------------------------------

(test me
      in
      ai0
      ai1
      ai2
      HIGH
      [#:show-graph #true]
      with

      ;; llm test
      (check-equal?
       (me
        (list (list 1 7) (list 2 8) (list 3 9) (list 4 10) (list 5 11) (list 6 12)))
       27)

      (check-equal?
       (me
        (list (list 1 3) (list 2 4) (list 10 11) (list 10 12) (list 8 9)))
       32)

      (check-equal? (me (list (list 5 10) (list 10 20) (list 20 40))) 40)

      (check-equal? (me (list (list 1 2) (list 2 4) (list 4 8))) 8)

      (check-equal? 
       (me (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5) (list 6 6) (list 7 7) (list 8 8) (list 9 9) (list 10 10)))
       55)

      (check-equal? (me (list (list 1 3) (list 5 8))) 8)

      (check-equal? (me (list (list 1 1))) 1)

      (check-equal? 
       (me
        (list (list 1 2) (list 1 2) (list 1 2) (list 1 2) (list 1 2) (list 1 2) (list 1 2) (list 1 2) (list 1 2) (list 1 2) (list 1 2)))
       12)

      (check-equal? (me (list (list 2 2) (list 3 3) (list 4 4) (list 5 5)))
                    14))


