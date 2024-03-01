#lang racket

;; Function to find the maximum number of achievable requests
;; We have n buildings numbered from 0 to n - 1. Each building has a number of employees. It's transfer season, and some employees want to change the building they reside in.
;; You are given a list requests where requests[i] = [fromi, toi] represents an employee's request to transfer from building fromi to building toi.
;; All buildings are full, so a list of requests is achievable only if for each building, the net change in employee transfers is zero. This means the number of employees leaving is equal to the number of employees moving in. For example if n = 3 and two employees are leaving building 0, one is leaving building 1, and one is leaving building 2, there should be two employees moving to building 0, one employee moving to building 1, and one employee moving to building 2.
;; Return the maximum number of achievable requests.
;; Example 1:
;; Input: n = 5, requests = [[0,1],[1,0],[0,1],[1,2],[2,0],[3,4]]
;; Output: 5
;; Explantion: Let's see the requests:
;; From building 0 we have employees x and y and both want to move to building 1.
;; From building 1 we have employees a and b and they want to move to buildings 2 and 0 respectively.
;; From building 2 we have employee z and they want to move to building 0.
;; From building 3 we have employee c and they want to move to building 4.
;; From building 4 we don't have any requests.
;; We can achieve the requests of users x and b by swapping their places.
;; We can achieve the requests of users y, a and z by swapping the places in the 3 buildings.
;; Example 2:
;; Input: n = 3, requests = [[0,0],[1,2],[2,1]]
;; Output: 3
;; Explantion: Let's see the requests:
;; From building 0 we have employee x and they want to stay in the same building 0.
;; From building 1 we have employee y and they want to move to building 2.
;; From building 2 we have employee z and they want to move to building 1.
;; We can achieve all the requests. 
;; Example 3:
;; Input: n = 4, requests = [[0,3],[3,1],[1,2],[2,0]]
;; Output: 4
;; Constraints:
;; 1 <= n <= 20
;; 1 <= requests.length <= 16
;; requests[i].length == 2
;; 0 <= fromi, toi < n
(define (maximumRequests n requests)
  ;; Helper function to calculate the net change in employee transfers for a subset of requests
  (define (net-changes subset)
    ;; Initialize net changes for each building
    (let ([changes (make-vector n 0)])
      ;; Iterate over each request
      (for ([req (in-list subset)])
        (vector-set! changes (first req) (- (vector-ref changes (first req)) 1))
        (vector-set! changes (second req) (+ (vector-ref changes (second req)) 1)))
      changes))
  
  ;; Helper function to check if the subset of requests is achievable
  (define (valid-subset? subset)
    ;; Calculate net changes for the subset
    (let ([changes (net-changes subset)])
      ;; Check if all buildings have net zero changes
      (for/and ([change (in-vector changes)])
        (= change 0))))
  
  ;; Initialize maximum number of achievable requests
  (define max-requests 0)
  
  ;; Generate all possible subsets of requests
  (for ([i (in-range (expt 2 (length requests)))])
    ;; Generate subset for the current number
    (let ([subset (for/list ([j (in-range (length requests))]
                             #:when (bitwise-bit-set? i j))
                    (list-ref requests j))])
      ;; Check if the subset is valid and update max-requests if needed
      (when (valid-subset? subset)
        (set! max-requests (max max-requests (length subset))))))
  
  ;; Return the maximum number of achievable requests
  max-requests)

;; Test the function with example inputs
(maximumRequests 5 '((0 1) (1 0) (0 1) (1 2) (2 0) (3 4))) ; Should output 5
(require rackunit)

(define (test-humaneval) 

  (let (( candidate maximumRequests))
    (check-within (candidate 4 (list (list 0 1) (list 1 2) (list 2 3) (list 3 0) (list 0 2) (list 2 0) (list 1 3) (list 3 1))) 8 0.001)
    (check-within (candidate 5 (list (list 0 1) (list 1 0) (list 0 1) (list 1 2) (list 2 0) (list 3 4))) 5 0.001)
    (check-within (candidate 3 (list (list 0 1) (list 1 2) (list 2 0) (list 1 0) (list 2 0))) 3 0.001)
    (check-within (candidate 1 (list (list 0 0))) 1 0.001)
    (check-within (candidate 2 (list (list 0 1) (list 1 1))) 1 0.001)
    (check-within (candidate 5 (list (list 0 3) (list 3 1) (list 1 2) (list 2 0))) 4 0.001)
    (check-within (candidate 3 (list (list 0 1) (list 1 2) (list 2 0) (list 1 0))) 3 0.001)
    (check-within (candidate 5 (list (list 0 1) (list 1 2) (list 2 3) (list 3 4) (list 4 0))) 5 0.001)
    (check-within (candidate 6 (list (list 0 1) (list 1 2) (list 2 3) (list 3 4) (list 4 5) (list 5 0))) 6 0.001)
    (check-within (candidate 3 (list (list 0 2) (list 2 1) (list 1 0))) 3 0.001)
    (check-within (candidate 7 (list (list 0 1) (list 1 2) (list 2 3) (list 3 4) (list 4 5) (list 5 6) (list 6 0))) 7 0.001)
    (check-within (candidate 4 (list (list 3 2) (list 3 3) (list 0 3) (list 2 3) (list 1 2))) 3 0.001)
    (check-within (candidate 4 (list (list 0 3) (list 3 1) (list 1 2) (list 2 0))) 4 0.001)
    (check-within (candidate 3 (list (list 0 0) (list 1 2) (list 2 1))) 3 0.001)
    (check-within (candidate 5 (list (list 0 1) (list 1 2) (list 2 3) (list 3 4) (list 4 0) (list 0 2) (list 1 3) (list 2 4) (list 3 0) (list 4 1) (list 3 1) (list 2 3) (list 1 4))) 12 0.001)
    (check-within (candidate 2 (list (list 0 1) (list 1 0) (list 0 1) (list 1 0))) 4 0.001)
    (check-within (candidate 3 (list (list 1 0) (list 2 1) (list 0 2) (list 2 0) (list 1 0))) 3 0.001)
    (check-within (candidate 2 (list (list 0 1) (list 1 0) (list 0 1) (list 1 0) (list 0 1) (list 1 0) (list 0 1) (list 1 0))) 8 0.001)
    (check-within (candidate 1 (list )) 0 0.001)
    (check-within (candidate 3 (list (list 0 1) (list 1 2) (list 2 0) (list 0 1) (list 1 2) (list 2 0))) 6 0.001)
    (check-within (candidate 5 (list (list 0 1) (list 1 0) (list 2 3) (list 3 2) (list 4 0) (list 0 4) (list 2 4) (list 4 2))) 8 0.001)
    (check-within (candidate 6 (list (list 0 1) (list 1 0) (list 0 1) (list 1 0) (list 0 1) (list 1 0))) 6 0.001)
))

(test-humaneval)