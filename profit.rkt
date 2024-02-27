#lang racket

(require "testing.rkt")

(def-module module% jobScheduling)

;; ---------------------------------------------------------------------------------------------------
;; Given n jobs: scheduled to be done startTime[i] to endTime[i], obtaining a profit of profit[i]
;; return the maximum profit such that there are no two jobs in the subset with overlapping time range
;; subj. to. a job that ends at time X means it is possible to start another job that starts at time X

;; this is of course a HORRIBLE data representation; an F I student should flunk 

#; {[Listof N] [Listpf N] [Listof N] -> N}

;; ---------------------------------------------------------------------------------------------------
(module% plain-functional
  (define (jobScheduling startTime endTime profit)

    ;; try all remaining jobs >= endtime
    ;; for each, filter jobs by endtime -> report profit
    ;;     accumulate: jobs chosen to compute profit when no jobs left

    #; {type Task = [List N N N]} ;; should use struct but it's internal to a function
    #; {type ProfitTree = N || [Listof ProfitTree]}
  
    #; {[Listof Task] N -> ProfitTree}
    ;; compute the profit starting from the still-executable tasks, given 
    ;; ACCUMULATOR the profit accrued by the tasks run between `tasks0` and `tasks1`
    (define (jobScheduling/accu tasks1 profit-on-path-to-here)
      (cond
        [(empty? tasks1) profit-on-path-to-here]
        [else
         ;; if two tasks have identical representations, this avoids re-computations (just choose one)
         (for*/list ([t tasks1] [all-other-tasks (in-value (set-remove tasks1 t))])
           (match-define [list _ end profit] t)
           (define remaining-tasks (filter (λ (t) (>= (first t) end)) all-other-tasks))
           (jobScheduling/accu remaining-tasks (+ profit profit-on-path-to-here)))]))

    #; {[Listof Task]}
    (define tasks0 (map list startTime endTime profit))

    ;; in:
    (apply max (flatten (jobScheduling/accu tasks0 0)))))

;; ---------------------------------------------------------------------------------------------------
(module% eliminate-flatten
  (define (jobScheduling startTime endTime profit)

    ;; try all remaining jobs >= endtime
    ;; for each, filter jobs by endtime -> report profit
    ;;     accumulate: jobs chosen to compute profit when no jobs left

    #; {type Task = [List N N N]} ;; should use struct but it's internal to a function
    #; {type ProfitTree = N || [Listof ProfitTree]}
  
    #; {[Listof Task] N -> N}
    ;; compute the profit starting from the still-executable tasks, given 
    ;; ACCUMULATOR the profit accrued by the tasks run between `tasks0` and `tasks1`
    (define (jobScheduling/accu tasks1 profit-on-path-to-here)
      (cond
        [(empty? tasks1) profit-on-path-to-here]
        [else
         ;; if two tasks have identical representations, this avoids re-computations (just choose one)
         (for*/fold ([max-so-far 0]) ([t tasks1] [all-other-tasks (in-value (set-remove tasks1 t))])
           (match-define [list _ end profit] t)
           (define remaining-tasks (filter (λ (t) (>= (first t) end)) all-other-tasks))
           (max max-so-far
                (jobScheduling/accu remaining-tasks (+ profit profit-on-path-to-here))))]))

    #; {[Listof Task]}
    (define tasks0 (map list startTime endTime profit))

    ;; in:
    (jobScheduling/accu tasks0 0)))

;; ---------------------------------------------------------------------------------------------------
(module% with-global-accu
  (define (jobScheduling startTime endTime profit)

    ;; try all remaining jobs >= endtime
    ;; for each, filter jobs by endtime -> report profit
    ;;     accumulate: jobs chosen to compute profit when no jobs left

    #; {type Task = [List N N N]} ;; should use struct but it's internal to a function
    #; {type ProfitTree = N || [Listof ProfitTree]}
  
    #; {[Listof Task] N N -> N}
    ;; compute the profit starting from the still-executable tasks, given 
    ;; ACCUMULATOR the profit accrued by the tasks run between `tasks0` and `tasks1`
    (define (jobScheduling/accu tasks1 profit-on-path-to-here max-so-far)
      (cond
        [(empty? tasks1) (max profit-on-path-to-here max-so-far)]
        [else
         ;; if two tasks have identical representations, this avoids re-computations (just choose one)
         (for*/fold ([max-so-far max-so-far]) ([t tasks1] [others (in-value (set-remove tasks1 t))])
           (match-define [list _ end profit] t)
           (define remaining-tasks (filter (λ (t) (>= (first t) end)) others))
           (jobScheduling/accu remaining-tasks (+ profit profit-on-path-to-here) max-so-far))]))

    #; {[Listof Task]}
    (define tasks0 (map list startTime endTime profit))

    ;; in:
    (jobScheduling/accu tasks0 0 0)))

;; ---------------------------------------------------------------------------------------------------
(module% imperative
  (define (jobScheduling startTime endTime profit)

    (define max-so-far 0)

    ;; try all remaining jobs >= endtime
    ;; for each, filter jobs by endtime -> report profit
    ;;     accumulate: jobs chosen to compute profit when no jobs left

    #; {type Task = [List N N N]} ;; should use struct but it's internal to a function
    #; {type ProfitTree = N || [Listof ProfitTree]}
  
    #; {[Listof Task] N -> Void}
    ;; compute the profit starting from the still-executable tasks, given 
    ;; ACCUMULATOR the profit accrued by the tasks run between `tasks0` and `tasks1`
    (define (jobScheduling/accu tasks1 accu)
      (cond
        [(empty? tasks1) (set! max-so-far (max accu max-so-far))]
        [else
         ;; if two tasks have identical representations, this avoids re-computations (just choose one)
         (for*/list ([t tasks1] [all-other-tasks (in-value (set-remove tasks1 t))])
           (match-define [list _ end profit] t)
           (define remaining-tasks (filter (λ (t) (>= (first t) end)) all-other-tasks))
           (jobScheduling/accu remaining-tasks (+ profit accu)))]))

    #; {[Listof Task]}
    (define tasks0 (map list startTime endTime profit))

    ;; in:
    (jobScheduling/accu tasks0 0)

    max-so-far))

;; -----------------------------------------------------------------------------
;; test them all 
(test jobScheduling
      in
      plain-functional eliminate-flatten with-global-accu imperative
      with
      (check-equal? (jobScheduling (list 1 2 3 3) (list 3 4 5 6) (list 50 10 40 70))
                    ;; Time range [1-3]+[3-6] , we get profit of 120 = 50 + 70.
                    120 "1 -> 4") 

      (check-equal? (jobScheduling (list 1 2 3 4 6) (list 3 5 10 6 9) (list 20 20 100 70 60))
                    150
                    "1 -> 4 -> 5")

      (check-equal? (jobScheduling (list 1 1 1) (list 2 3 4) (list 5 6 4))
                    6
                    "2"))

