#lang racket

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
  ;; Function to calculate the net changes in employee transfers for each building
  (define (net-transfers subset)
    (foldl (lambda (req acc)
             (let ([from (car req)]
                   [to (cadr req)])
               (update acc from sub1)
               (update acc to add1)))
           (make-vector n 0)
           subset))
  ;; Helper function to update a vector based on a key and an update function (e.g., add1, sub1)
  (define (update vec key func)
    (vector-set! vec key (func (vector-ref vec key)))
    vec)
  ;; Check if all buildings have net zero transfers
  (define (valid-transfers? transfers)
    (for/and ([i (in-vector transfers)])
      (= i 0)))
  ;; Recursive function to generate all possible subsets of requests and evaluate them
  (define (generate-subsets lst)
    (if (null? lst)
        '(())
        (let* ([x (car lst)]
               [xs (cdr lst)]
               [without-x (generate-subsets xs)])
          (append without-x (map (lambda (subset) (cons x subset)) without-x)))))
  ;; Main logic to find the maximum number of achievable requests
  (let* ([subsets (generate-subsets requests)]
         [valid-subsets (filter (lambda (subset)
                                  (valid-transfers? (net-transfers subset)))
                                subsets)])
    (apply max (map length valid-subsets))))

;; Example usage
(define requests1 '((0 1) (1 0) (0 1) (1 2) (2 0) (3 4)))
(define requests2 '((0 0) (1 2) (2 1)))
(define requests3 '((0 3) (3 1) (1 2) (2 0)))

(maximumRequests 5 requests1) ;; Output: 5
(maximumRequests 3 requests2) ;; Output: 3
(maximumRequests 4 requests3) ;; Output: 4
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