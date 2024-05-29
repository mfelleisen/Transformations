#lang racket

;; You are given an integer n representing the number of houses on a number line, numbered from 0 to n - 1.
;; Additionally, you are given a 2D integer array offers where offers[i] = [starti, endi, goldi], indicating that ith buyer wants to buy all the houses from starti to endi for goldi amount of gold.
;; As a salesman, your goal is to maximize your earnings by strategically selecting and selling houses to buyers.
;; Return the maximum amount of gold you can earn.
;; Note that different buyers can't buy the same house, and some houses may remain unsold.
;; Example 1:
;; Input: n = 5, offers = [[0,0,1],[0,2,2],[1,3,2]]
;; Output: 3
;; Explanation: There are 5 houses numbered from 0 to 4 and there are 3 purchase offers.
;; We sell houses in the range [0,0] to 1st buyer for 1 gold and houses in the range [1,3] to 3rd buyer for 2 golds.
;; It can be proven that 3 is the maximum amount of gold we can achieve.
;; Example 2:
;; Input: n = 5, offers = [[0,0,1],[0,2,10],[1,3,2]]
;; Output: 10
;; Explanation: There are 5 houses numbered from 0 to 4 and there are 3 purchase offers.
;; We sell houses in the range [0,2] to 2nd buyer for 10 golds.
;; It can be proven that 10 is the maximum amount of gold we can achieve.
;; Constraints:
;;  * 1 <= n <= 105
;;  * 1 <= offers.length <= 105
;;  * offers[i].length == 3
;;  * 0 <= starti <= endi <= n - 1
;;  * 1 <= goldi <= 103
(define (maximizeTheProfit n offers)
  ;; Sort offers by their ending time
  (define sorted-offers (sort offers < #:key second))

  ;; Helper function to find the last non-conflicting offer using binary search
  (define (find-last-non-conflicting offers i)
    (let loop ((low 0) (high (- i 1)))
      (if (> low high)
          -1
          (let* ((mid (quotient (+ low high) 2))
                 (mid-offer (list-ref offers mid)))
            (if (< (second mid-offer) (first (list-ref offers i)))
                (let ((next-offer (list-ref offers (+ mid 1))))
                  (if (< (second next-offer) (first (list-ref offers i)))
                      (loop (+ mid 1) high)
                      mid))
                (loop low (- mid 1)))))))

  ;; Recursive function to calculate maximum profit using memoization
  (define (calculate-max-profit dp sorted-offers i)
    (if (= i 0)
        (vector-ref dp 0)
        (let* ((current-offer (list-ref sorted-offers i))
               (current-profit (third current-offer))
               (last-non-conflict (find-last-non-conflicting sorted-offers i))
               (profit-including-current (if (>= last-non-conflict 0)
                                             (+ current-profit (vector-ref dp last-non-conflict))
                                             current-profit)))
          (vector-set! dp i (max (vector-ref dp (- i 1)) profit-including-current))
          (vector-ref dp i))))

  ;; Initialize dp vector
  (define dp (make-vector (length sorted-offers) 0))
  (vector-set! dp 0 (third (first sorted-offers))) ;; Base case

  ;; Fill dp vector using recursion
  (for ([i (in-range 1 (length sorted-offers))])
    (calculate-max-profit dp sorted-offers i))

  ;; Return the last element in dp vector as the result
  (vector-ref dp (- (length sorted-offers) 1)))

;; Example usage:
(maximizeTheProfit 5 (list (list 0 0 1) (list 0 2 2) (list 1 3 2)))  ;; Output: 3
(maximizeTheProfit 5 (list (list 0 0 1) (list 0 2 10) (list 1 3 2)))  ;; Output: 10

(require rackunit)


(define (test-humaneval) 

  (let (( candidate maximizeTheProfit))
    (check-within (candidate 5 (list (list 0 0 1) (list 0 2 2) (list 1 3 2))) 3 0.001)
    (check-within (candidate 5 (list (list 0 0 1) (list 0 2 10) (list 1 3 2))) 10 0.001)
    (check-within (candidate 4 (list (list 1 3 10) (list 1 3 3) (list 0 0 1) (list 0 0 7))) 17 0.001)
    (check-within (candidate 4 (list (list 0 0 6) (list 1 2 8) (list 0 3 7) (list 2 2 5) (list 0 1 5) (list 2 3 2) (list 0 2 8) (list 2 3 10) (list 0 3 2))) 16 0.001)
    (check-within (candidate 15 (list (list 5 5 10) (list 2 6 6) (list 8 11 5) (list 7 11 9) (list 2 4 1) (list 3 8 5) (list 0 6 9) (list 0 10 5) (list 5 10 8) (list 4 5 1))) 20 0.001)
    (check-within (candidate 10 (list (list 1 6 1) (list 0 1 10) (list 3 6 2) (list 0 5 10) (list 0 0 3) (list 0 0 4) (list 1 1 4) (list 0 6 7) (list 4 4 1))) 12 0.001)
    (check-within (candidate 11 (list (list 7 8 6) (list 6 6 4) (list 4 6 9) (list 6 7 4) (list 5 5 8) (list 1 5 9) (list 7 7 8) (list 1 2 5) (list 0 2 9) (list 1 3 8) (list 0 2 7) (list 2 2 8))) 29 0.001)
    (check-within (candidate 3 (list (list 0 0 6) (list 0 1 8) (list 1 2 1) (list 0 1 4) (list 0 1 2) (list 0 0 7) (list 0 0 6) (list 0 0 5))) 8 0.001)
    (check-within (candidate 4 (list (list 0 1 9) (list 1 1 4))) 9 0.001)
    (check-within (candidate 11 (list (list 1 10 6) (list 1 10 5) (list 0 2 7) (list 0 0 8) (list 8 10 7))) 15 0.001)
    (check-within (candidate 3 (list (list 0 1 8) (list 1 1 6) (list 2 2 7) (list 0 2 6) (list 0 2 2) (list 0 0 6) (list 0 0 9) (list 0 1 4))) 22 0.001)
    (check-within (candidate 6 (list (list 0 2 4))) 4 0.001)
    (check-within (candidate 10 (list (list 5 9 3) (list 1 5 8) (list 0 0 6) (list 5 8 10))) 16 0.001)
    (check-within (candidate 5 (list (list 1 1 3) (list 1 1 3) (list 0 0 8) (list 1 3 8) (list 0 2 1) (list 3 3 9) (list 0 0 7) (list 0 2 3) (list 0 0 5) (list 0 3 10) (list 1 3 10) (list 4 4 6) (list 0 1 1) (list 2 4 10))) 26 0.001)
    (check-within (candidate 13 (list (list 2 2 5) (list 1 8 10) (list 2 3 3))) 10 0.001)
    (check-within (candidate 2 (list (list 1 1 8) (list 1 1 8) (list 1 1 10) (list 1 1 7) (list 0 0 7) (list 0 0 3) (list 0 1 8) (list 0 0 4) (list 0 0 4) (list 0 0 7) (list 0 0 10) (list 0 1 4) (list 1 1 1) (list 0 1 5))) 20 0.001)
    (check-within (candidate 3 (list (list 0 1 7) (list 1 1 3) (list 0 0 2) (list 1 1 6) (list 0 0 10) (list 1 1 7) (list 0 2 3) (list 0 1 2) (list 0 0 7))) 17 0.001)
    (check-within (candidate 5 (list (list 0 0 5) (list 1 3 9) (list 0 2 2) (list 1 1 6) (list 1 2 10) (list 0 2 10) (list 1 1 3))) 15 0.001)
    (check-within (candidate 10 (list (list 0 1 9) (list 5 6 10) (list 1 3 8) (list 1 9 7) (list 7 8 1) (list 2 7 1) (list 0 8 7) (list 1 6 6) (list 1 4 4) (list 0 5 4) (list 0 0 3) (list 0 8 6))) 22 0.001)
    (check-within (candidate 4 (list (list 0 0 1) (list 0 0 10) (list 0 2 1) (list 0 0 6) (list 0 3 10) (list 0 1 5) (list 1 2 10) (list 0 0 2) (list 3 3 1) (list 0 0 9) (list 0 1 2) (list 0 0 4) (list 1 3 5) (list 1 1 1))) 21 0.001)
    (check-within (candidate 9 (list (list 0 3 10) (list 5 6 5) (list 1 5 2) (list 1 8 9) (list 1 1 9) (list 1 7 1) (list 3 7 9) (list 2 3 2) (list 4 6 1) (list 4 5 7) (list 2 2 2) (list 6 8 10) (list 1 3 10) (list 1 4 10))) 28 0.001)
    (check-within (candidate 10 (list (list 0 2 2))) 2 0.001)
    (check-within (candidate 10 (list (list 2 7 4) (list 2 4 9) (list 1 8 7) (list 0 4 3))) 9 0.001)
    (check-within (candidate 6 (list (list 0 1 4) (list 1 2 4) (list 0 1 10) (list 1 2 4) (list 2 2 5) (list 1 1 8) (list 2 3 2) (list 4 4 4) (list 0 0 3))) 20 0.001)
    (check-within (candidate 1 (list (list 0 0 8) (list 0 0 3) (list 0 0 8) (list 0 0 8) (list 0 0 5) (list 0 0 9) (list 0 0 6) (list 0 0 1) (list 0 0 8) (list 0 0 1) (list 0 0 5) (list 0 0 9) (list 0 0 2))) 9 0.001)
    (check-within (candidate 15 (list (list 8 10 5) (list 4 12 6) (list 6 11 7) (list 8 11 3) (list 7 13 1) (list 7 7 8) (list 8 10 5) (list 0 11 3) (list 1 1 9) (list 2 11 6) (list 3 11 8))) 22 0.001)
    (check-within (candidate 10 (list (list 5 6 9) (list 0 2 9))) 18 0.001)
    (check-within (candidate 11 (list (list 7 9 5) (list 0 0 8) (list 6 6 3) (list 4 9 1) (list 3 7 5) (list 0 4 7))) 16 0.001)
    (check-within (candidate 7 (list (list 0 2 9) (list 2 4 8) (list 0 3 6) (list 4 4 10) (list 2 2 2) (list 1 1 10) (list 0 0 8) (list 4 4 9) (list 4 4 4) (list 3 3 5) (list 2 5 2) (list 0 3 6) (list 3 4 5))) 35 0.001)
    (check-within (candidate 9 (list (list 3 8 1) (list 0 6 7) (list 0 3 6) (list 1 6 2) (list 2 3 10) (list 3 3 2) (list 1 2 2) (list 1 3 9) (list 0 0 7) (list 1 2 9) (list 5 5 4) (list 5 6 6) (list 1 5 5) (list 0 1 2) (list 0 6 1))) 24 0.001)
    (check-within (candidate 8 (list (list 0 0 7) (list 0 1 8) (list 1 1 1) (list 2 2 7) (list 2 3 1))) 15 0.001)
    (check-within (candidate 8 (list (list 6 6 5) (list 0 1 7) (list 1 7 10))) 12 0.001)
    (check-within (candidate 13 (list (list 0 9 5) (list 6 8 7) (list 0 0 3) (list 4 4 2) (list 1 9 7) (list 9 12 9) (list 1 2 9) (list 1 1 10) (list 3 3 3) (list 0 3 3) (list 4 8 5) (list 0 0 9) (list 7 10 7))) 40 0.001)
    (check-within (candidate 11 (list (list 2 5 1))) 1 0.001)
    (check-within (candidate 3 (list (list 0 0 9) (list 0 2 6) (list 1 1 1) (list 1 2 10) (list 0 0 10) (list 0 0 4) (list 0 2 7) (list 0 0 1) (list 0 0 9) (list 2 2 5))) 20 0.001)
    (check-within (candidate 5 (list (list 1 1 3) (list 1 2 1) (list 0 2 3) (list 1 1 10) (list 3 3 3) (list 2 4 3) (list 0 3 5) (list 4 4 2) (list 2 3 10) (list 3 3 8) (list 3 3 9) (list 0 2 8) (list 0 2 2) (list 1 1 3) (list 0 0 8))) 30 0.001)
    (check-within (candidate 13 (list (list 6 9 3) (list 6 9 6) (list 5 12 10) (list 11 12 4) (list 4 4 2) (list 0 7 8) (list 2 6 6) (list 6 6 4))) 12 0.001)
    (check-within (candidate 3 (list (list 0 2 9) (list 1 1 8) (list 0 1 1) (list 2 2 4) (list 2 2 1) (list 0 0 4) (list 1 1 9) (list 0 0 6) (list 0 1 7))) 19 0.001)
    (check-within (candidate 3 (list (list 1 2 8) (list 0 0 1) (list 0 1 1) (list 0 0 3) (list 1 2 2) (list 0 0 7) (list 0 0 10) (list 1 1 6))) 18 0.001)
    (check-within (candidate 2 (list (list 0 0 3) (list 1 1 10) (list 0 1 6))) 13 0.001)
    (check-within (candidate 3 (list (list 0 0 9) (list 1 1 1) (list 0 2 7) (list 1 1 7) (list 1 2 6) (list 0 0 8) (list 0 2 3) (list 1 2 10) (list 2 2 3) (list 2 2 5))) 21 0.001)
    (check-within (candidate 5 (list (list 2 3 2) (list 0 1 7) (list 0 1 1) (list 0 0 9) (list 2 4 1) (list 3 4 5) (list 1 3 10) (list 0 0 8))) 19 0.001)
    (check-within (candidate 15 (list (list 4 6 9) (list 4 10 9) (list 3 5 4) (list 0 2 6) (list 3 13 7) (list 1 11 6) (list 1 8 4) (list 4 12 4) (list 3 8 8) (list 13 13 7) (list 4 12 3))) 22 0.001)
    (check-within (candidate 8 (list (list 1 5 9) (list 0 4 9) (list 0 0 3) (list 1 2 9) (list 0 0 10) (list 4 7 9) (list 7 7 2) (list 0 2 6) (list 1 1 5) (list 1 4 3) (list 2 4 8) (list 0 1 1) (list 2 3 1))) 28 0.001)
    (check-within (candidate 4 (list (list 0 2 7) (list 2 3 9) (list 2 3 2) (list 1 2 1) (list 1 2 9) (list 0 3 7) (list 0 2 9) (list 1 2 8) (list 0 3 10) (list 0 3 8) (list 0 0 5) (list 2 2 6))) 14 0.001)
    (check-within (candidate 12 (list (list 0 0 4) (list 5 8 2) (list 2 2 10) (list 3 5 7) (list 1 2 1) (list 5 7 8) (list 8 11 3))) 25 0.001)
    (check-within (candidate 2 (list (list 0 0 7) (list 0 1 3) (list 0 0 8))) 8 0.001)
    (check-within (candidate 4 (list (list 2 3 8) (list 0 1 1) (list 3 3 2))) 9 0.001)
    (check-within (candidate 14 (list (list 2 12 4) (list 7 11 4) (list 4 4 5) (list 0 1 6) (list 3 4 1) (list 4 11 9) (list 10 12 7) (list 7 12 1) (list 11 11 1) (list 0 0 5) (list 12 12 8) (list 6 7 6))) 26 0.001)
    (check-within (candidate 10 (list (list 1 4 6) (list 7 9 9) (list 1 4 5) (list 8 8 2) (list 4 7 1) (list 6 8 8) (list 2 3 1) (list 0 1 4))) 15 0.001)
    (check-within (candidate 7 (list (list 2 5 5) (list 1 2 9) (list 1 3 7) (list 2 4 3) (list 0 0 6) (list 0 0 1) (list 4 4 9) (list 1 5 7) (list 2 2 10))) 25 0.001)
    (check-within (candidate 11 (list (list 0 4 10))) 10 0.001)
    (check-within (candidate 3 (list (list 0 1 10) (list 1 2 2) (list 0 2 6) (list 0 0 1) (list 0 0 3) (list 0 1 8) (list 0 0 2) (list 2 2 8) (list 0 0 3) (list 2 2 3) (list 1 2 6) (list 0 0 4) (list 1 2 5))) 18 0.001)
    (check-within (candidate 14 (list (list 11 11 4) (list 1 11 10) (list 11 12 2) (list 7 8 2))) 10 0.001)
    (check-within (candidate 2 (list (list 0 0 1) (list 0 0 1) (list 1 1 9) (list 0 0 1) (list 1 1 2) (list 0 1 10))) 10 0.001)
    (check-within (candidate 6 (list (list 0 5 6) (list 1 2 10) (list 0 2 4) (list 2 4 5) (list 4 4 6) (list 2 2 2) (list 0 0 7) (list 2 5 9) (list 2 2 3))) 23 0.001)
    (check-within (candidate 6 (list (list 0 0 7) (list 2 5 5))) 12 0.001)
    (check-within (candidate 10 (list (list 2 3 2) (list 0 1 6) (list 0 0 2) (list 1 1 5) (list 3 3 8) (list 2 8 7) (list 1 7 8) (list 0 1 4) (list 7 7 8) (list 1 3 7) (list 5 5 10) (list 2 6 6) (list 0 0 4) (list 5 7 4) (list 1 9 4))) 35 0.001)
    (check-within (candidate 10 (list (list 0 2 4) (list 1 4 7) (list 0 1 10) (list 0 5 1))) 10 0.001)
    (check-within (candidate 12 (list (list 0 5 6) (list 4 10 9) (list 7 11 10) (list 10 11 1) (list 6 10 1) (list 2 2 6))) 16 0.001)
    (check-within (candidate 11 (list (list 3 7 8) (list 2 7 10) (list 3 9 3))) 10 0.001)
    (check-within (candidate 4 (list (list 0 0 3) (list 0 2 6) (list 0 0 1) (list 1 1 2) (list 0 2 8) (list 1 1 3) (list 1 3 8) (list 1 1 10) (list 1 2 7) (list 1 1 8) (list 0 0 9))) 19 0.001)
    (check-within (candidate 1 (list (list 0 0 9))) 9 0.001)
    (check-within (candidate 3 (list (list 0 1 5) (list 0 0 5) (list 0 0 6) (list 0 1 6) (list 0 2 10) (list 1 2 6) (list 0 0 9) (list 1 2 9))) 18 0.001)
    (check-within (candidate 4 (list (list 0 0 2) (list 2 3 9) (list 0 1 8) (list 0 0 9) (list 0 0 1) (list 3 3 9) (list 1 2 1) (list 1 3 5) (list 0 1 4) (list 0 1 4))) 19 0.001)
    (check-within (candidate 3 (list (list 0 0 7) (list 2 2 1) (list 1 1 3) (list 0 0 3) (list 1 1 7) (list 0 1 5) (list 0 2 3) (list 1 1 5) (list 0 1 10) (list 1 1 5) (list 1 1 6) (list 0 1 3) (list 0 0 8) (list 1 2 7) (list 1 1 4))) 16 0.001)
    (check-within (candidate 14 (list (list 5 7 2) (list 1 5 3) (list 11 13 2) (list 12 12 5) (list 4 5 6) (list 5 10 2) (list 4 10 8) (list 1 1 4) (list 4 4 2) (list 3 7 9) (list 5 10 1) (list 0 3 2))) 18 0.001)
    (check-within (candidate 11 (list (list 1 1 5) (list 4 4 9) (list 0 0 1) (list 1 3 3) (list 3 7 4) (list 3 9 6) (list 7 10 2) (list 3 7 5) (list 4 4 8) (list 7 8 10) (list 1 3 7) (list 1 4 5) (list 0 0 10))) 36 0.001)
    (check-within (candidate 13 (list (list 4 9 9) (list 1 9 8) (list 1 9 8) (list 0 0 8) (list 8 11 3) (list 2 3 6) (list 9 9 10) (list 5 12 1) (list 4 6 4))) 28 0.001)
    (check-within (candidate 5 (list (list 2 2 7) (list 0 2 10) (list 2 3 10))) 10 0.001)
    (check-within (candidate 10 (list (list 0 4 6) (list 1 1 1) (list 0 5 1) (list 1 6 3) (list 8 9 1) (list 2 3 7) (list 2 3 10) (list 1 2 1) (list 0 0 8) (list 3 5 5) (list 0 0 10))) 22 0.001)
    (check-within (candidate 4 (list (list 0 1 1) (list 0 0 9) (list 1 1 8) (list 3 3 1) (list 1 1 5) (list 0 0 9) (list 0 1 9) (list 0 0 7) (list 2 2 2) (list 2 3 5) (list 1 1 10) (list 1 2 8))) 24 0.001)
    (check-within (candidate 7 (list (list 0 1 9) (list 0 1 4) (list 0 0 3) (list 0 0 1) (list 1 6 5) (list 4 6 9) (list 4 5 7) (list 0 0 3) (list 1 5 9) (list 0 2 2))) 18 0.001)
    (check-within (candidate 12 (list (list 8 8 6) (list 8 8 6) (list 1 10 7) (list 0 0 3) (list 9 10 7) (list 1 7 2) (list 1 1 1) (list 2 3 6) (list 0 11 1) (list 1 8 5) (list 1 5 7) (list 1 2 4) (list 9 9 5) (list 0 3 1))) 23 0.001)
    (check-within (candidate 15 (list (list 5 6 3) (list 2 2 7) (list 0 0 5) (list 1 7 10) (list 11 14 5) (list 13 14 1) (list 2 12 1) (list 0 4 5) (list 0 6 2) (list 6 9 10) (list 3 5 2) (list 0 1 1) (list 1 14 1) (list 1 6 1))) 29 0.001)
    (check-within (candidate 7 (list (list 1 1 5) (list 1 1 4) (list 0 0 9) (list 1 1 6) (list 0 6 4) (list 2 6 3) (list 2 5 9) (list 0 6 3) (list 0 2 1) (list 1 1 6) (list 4 5 5))) 24 0.001)
    (check-within (candidate 1 (list (list 0 0 5) (list 0 0 3) (list 0 0 4) (list 0 0 8) (list 0 0 10) (list 0 0 6) (list 0 0 7) (list 0 0 7) (list 0 0 7) (list 0 0 3) (list 0 0 4) (list 0 0 5))) 10 0.001)
    (check-within (candidate 7 (list (list 2 2 3) (list 2 6 4) (list 4 6 5) (list 0 0 4) (list 1 1 4) (list 2 3 1) (list 2 4 3) (list 0 2 8) (list 1 3 10) (list 1 3 2) (list 1 6 7) (list 0 6 9) (list 2 2 2) (list 1 1 9) (list 4 4 2))) 21 0.001)
    (check-within (candidate 12 (list (list 0 0 7) (list 0 2 3) (list 0 7 2) (list 2 3 1) (list 2 11 6) (list 2 10 2) (list 1 3 6) (list 4 7 9) (list 7 9 3) (list 4 6 1) (list 5 6 8) (list 0 2 4) (list 0 0 3) (list 5 5 9) (list 2 5 3))) 25 0.001)
    (check-within (candidate 9 (list (list 1 8 4) (list 5 6 5) (list 0 2 6) (list 4 5 4))) 11 0.001)
    (check-within (candidate 8 (list (list 0 4 6) (list 2 3 6) (list 2 5 9) (list 2 6 7) (list 6 6 5) (list 4 4 4) (list 1 1 5) (list 2 5 7))) 20 0.001)
    (check-within (candidate 13 (list (list 0 6 10))) 10 0.001)
    (check-within (candidate 6 (list (list 0 1 2) (list 0 0 9) (list 3 3 10) (list 0 3 7) (list 0 0 2) (list 0 0 3) (list 2 2 2) (list 2 3 2) (list 5 5 6) (list 0 1 2) (list 0 5 2))) 27 0.001)
    (check-within (candidate 14 (list (list 3 12 7) (list 1 3 2) (list 4 11 3) (list 0 1 7) (list 1 5 2) (list 1 1 4))) 14 0.001)
    (check-within (candidate 14 (list (list 0 0 3) (list 0 1 3) (list 1 11 3) (list 6 7 6) (list 7 7 5) (list 1 2 8) (list 7 10 9))) 20 0.001)
    (check-within (candidate 13 (list (list 0 12 7) (list 2 2 4) (list 2 2 8) (list 3 3 2) (list 1 11 5) (list 1 7 2))) 10 0.001)
    (check-within (candidate 1 (list (list 0 0 2) (list 0 0 8) (list 0 0 1))) 8 0.001)
    (check-within (candidate 1 (list (list 0 0 1) (list 0 0 4) (list 0 0 7) (list 0 0 2) (list 0 0 5) (list 0 0 1) (list 0 0 4) (list 0 0 2) (list 0 0 6) (list 0 0 6) (list 0 0 3) (list 0 0 3))) 7 0.001)
    (check-within (candidate 1 (list (list 0 0 6) (list 0 0 6) (list 0 0 3) (list 0 0 6) (list 0 0 6) (list 0 0 10) (list 0 0 1) (list 0 0 2))) 10 0.001)
    (check-within (candidate 9 (list (list 4 6 7) (list 1 3 10))) 17 0.001)
    (check-within (candidate 13 (list (list 2 6 3) (list 1 12 6) (list 2 11 3) (list 7 7 2) (list 5 12 4) (list 0 1 2) (list 0 1 8) (list 1 1 3) (list 6 6 4) (list 8 9 7) (list 8 8 2) (list 2 2 2) (list 0 0 9) (list 9 11 7) (list 8 9 7))) 29 0.001)
    (check-within (candidate 8 (list (list 0 1 8) (list 0 0 6) (list 5 5 9))) 17 0.001)
    (check-within (candidate 1 (list (list 0 0 10) (list 0 0 3) (list 0 0 8) (list 0 0 9) (list 0 0 1) (list 0 0 8) (list 0 0 2) (list 0 0 7) (list 0 0 10) (list 0 0 8) (list 0 0 5) (list 0 0 3) (list 0 0 2) (list 0 0 4))) 10 0.001)
    (check-within (candidate 9 (list (list 0 2 6) (list 1 3 5) (list 1 1 5) (list 2 3 10) (list 4 8 4) (list 5 8 5) (list 6 6 10))) 25 0.001)
    (check-within (candidate 6 (list (list 0 0 7))) 7 0.001)
    (check-within (candidate 8 (list (list 1 1 5) (list 1 2 9) (list 1 2 6) (list 0 3 6) (list 1 1 10) (list 3 4 1) (list 3 5 3) (list 1 5 8) (list 0 2 6) (list 5 7 9))) 20 0.001)
    (check-within (candidate 14 (list (list 3 4 4) (list 6 8 1) (list 0 4 1))) 5 0.001)
    (check-within (candidate 11 (list (list 4 4 2) (list 1 2 7) (list 2 8 10) (list 1 1 3) (list 8 10 4) (list 1 2 1) (list 4 6 10))) 21 0.001)
    (check-within (candidate 11 (list (list 1 8 1) (list 1 5 5) (list 0 1 3) (list 10 10 10) (list 1 1 8) (list 1 2 1) (list 2 3 10) (list 2 10 10) (list 2 2 9) (list 0 9 4))) 28 0.001)
    (check-within (candidate 6 (list (list 2 2 6) (list 0 1 2) (list 2 2 2))) 8 0.001)
))

(test-humaneval)