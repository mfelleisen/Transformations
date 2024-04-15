#lang racket

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
  ;; Combines startTime, endTime, and profit into a list of jobs and sorts them by endTime.
  (define jobs (sort (map list endTime startTime profit) < #:key car))
  ;; Initializes the dynamic programming table with the profit of the first job.
  (define dp (list (third (first jobs))))
  
  ;; A helper function to find the latest job that does not conflict with the current job.
  ;; It performs a binary search on the jobs' end times to find the right position.
  (define (latestNonConflict index)
    (define startTimeToFind (second (list-ref jobs index)))
    (let loop ((i 0) (j (sub1 index)))
      (cond
        ((> i j) -1) ;; Base case: if i exceeds j, there's no non-conflicting job.
        (else
         (define mid (quotient (+ i j) 2))
         (if (< (first (list-ref jobs mid)) startTimeToFind)
             (if (< (first (list-ref jobs (add1 mid))) startTimeToFind)
                 (loop (add1 mid) j)
                 mid)
             (loop i (sub1 mid)))))))
  
  ;; Iterates through the list of jobs, computing the maximum profit for each job
  ;; considering the non-conflicting jobs that come before it.
  (for/list ((i (in-range 1 (length jobs))))
    (define L (latestNonConflict i))
    ;; Updates the DP table with the maximum profit including or excluding the current job.
    (set! dp (append dp (list (max (last dp)
                                   (+ (if (= L -1) 0 (list-ref dp L))
                                      (third (list-ref jobs i))))))))
  
  ;; Returns the last element of the DP table, which contains the maximum profit.
  (last dp))

;; Example Usage:
(jobScheduling '(1 2 3 3) '(3 4 5 6) '(50 10 40 70))
(jobScheduling '(1 2 3 4 6) '(3 5 10 6 9) '(20 20 100 70 60))
(jobScheduling '(1 1 1) '(2 3 4) '(5 6 4))
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