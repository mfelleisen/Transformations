#lang racket

;; Function to calculate the maximum profit from non-overlapping jobs
;; We have n jobs, where every job is scheduled to be done from startTime[i] to endTime[i], obtaining a profit of profit[i].
;; You're given the startTime, endTime and profit lists, return the maximum profit you can take such that there are no two jobs in the subset with overlapping time range.
;; If you choose a job that ends at time X you will be able to start another job that starts at time X.
;; Example 1:
;; Input: startTime = [1,2,3,3], endTime = [3,4,5,6], profit = [50,10,40,70]
;; Output: 120
;; Explanation: The subset chosen is the first and fourth job. 
;; Time range [1-3]+[3-6] , we get profit of 120 = 50 + 70.
;; Example 2:
;; Input: startTime = [1,2,3,4,6], endTime = [3,5,10,6,9], profit = [20,20,100,70,60]
;; Output: 150
;; Explanation: The subset chosen is the first, fourth and fifth job. 
;; Profit obtained 150 = 20 + 70 + 60.
;; Example 3:
;; Input: startTime = [1,1,1], endTime = [2,3,4], profit = [5,6,4]
;; Output: 6
;; Constraints:
;; 1 <= startTime.length == endTime.length == profit.length <= 5 * 104
;; 1 <= startTime[i] < endTime[i] <= 109
;; 1 <= profit[i] <= 104
(define (jobScheduling startTime endTime profit)
  ;; Zipping start times, end times, and profits together and sorting by end time
  (define jobs (sort (map list endTime startTime profit) < #:key car))
  ;; Initialize DP array with the profit of the first job
  (define dp (list (third (car jobs))))
  
  ;; Helper function to find the latest job that doesn't conflict with the current job
  ;; It uses binary search to find such a job efficiently.
  (define (latestNonConflict index)
    (let* ((startTimeToFind (second (list-ref jobs index)))
           ;; Extracting only the end times to perform binary search
           (endTimes (map car jobs))
           (i (bisect-left endTimes startTimeToFind)))
      (if (> i 0) (- i 1) -1)))
  
  ;; Function to perform a binary search on a sorted list
  ;; Returns the index of the first element greater than or equal to the target
  (define (bisect-left sorted-list target)
    (let loop ([lo 0] [hi (length sorted-list)])
      (if (< lo hi)
          (let* ((mid (+ lo (quotient (- hi lo) 2)))
                 (mid-val (list-ref sorted-list mid)))
            (if (< mid-val target)
                (loop (+ mid 1) hi)
                (loop lo mid)))
          lo)))
  
  ;; Iterating through all jobs to fill the DP array with maximum profits
  (for ([i (in-range 1 (length jobs))])
    (let* ((L (latestNonConflict i))
           ;; If there's no conflict, add the current profit to the profit at L
           ;; Otherwise, just take the current job's profit
           (include-profit (+ (if (= L -1) 0 (list-ref dp L))
                              (third (list-ref jobs i))))
           ;; The maximum profit at this point is the max of including the current job
           ;; or not including it
           (max-profit (max (last dp) include-profit)))
      (set! dp (append dp (list max-profit)))))
  
  ;; The last element in DP array is the maximum profit achievable
  (last dp))

;; Example Usage
(jobScheduling '(1 2 3 3) '(3 4 5 6) '(50 10 40 70)) ;; => 120
(jobScheduling '(1 2 3 4 6) '(3 5 10 6 9) '(20 20 100 70 60)) ;; => 150
(jobScheduling '(1 1 1) '(2 3 4) '(5 6 4)) ;; => 6
(require rackunit)

(define (test-humaneval) 

  (let (( candidate jobScheduling))
    (check-within (candidate (list 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) (list 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2) (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) 15 0.001)
    (check-within (candidate (list 1 1 1 1 1) (list 2 2 2 2 2) (list 1 2 3 4 5)) 5 0.001)
    (check-within (candidate (list 1 1 1) (list 2 3 4) (list 5 6 4)) 6 0.001)
    (check-within (candidate (list 1 3 5 7 9) (list 2 4 6 8 10) (list 100 200 300 400 500)) 1500 0.001)
    (check-within (candidate (list 1 3 0 5 3) (list 2 4 1 6 5) (list 50 10 40 70 60)) 130 0.001)
    (check-within (candidate (list 1 1 1 1 1) (list 3 3 3 3 3) (list 5 6 7 8 9)) 9 0.001)
))

(test-humaneval)