#lang racket

;; There is a one-dimensional garden on the x-axis. The garden starts at the point 0 and ends at the point n. (i.e., the length of the garden is n).
;; There are n + 1 taps located at points [0, 1, ..., n] in the garden.
;; Given an integer n and an integer list ranges of length n + 1 where ranges[i] (0-indexed) means the i-th tap can water the area [i - ranges[i], i + ranges[i]] if it was open.
;; Return the minimum number of taps that should be open to water the whole garden, If the garden cannot be watered return -1.
;; Example 1:
;; Input: n = 5, ranges = [3,4,1,1,0,0]
;; Output: 1
;; Explanation: The tap at point 0 can cover the interval [-3,3]
;; The tap at point 1 can cover the interval [-3,5]
;; The tap at point 2 can cover the interval [1,3]
;; The tap at point 3 can cover the interval [2,4]
;; The tap at point 4 can cover the interval [4,4]
;; The tap at point 5 can cover the interval [5,5]
;; Opening Only the second tap will water the whole garden [0,5]
;; Example 2:
;; Input: n = 3, ranges = [0,0,0,0]
;; Output: -1
;; Explanation: Even if you activate all the four taps you cannot water the whole garden.
;; Constraints:
;; 1 <= n <= 104
;; ranges.length == n + 1
;; 0 <= ranges[i] <= 100
(define (min_taps n ranges)
  ;; Create a list to store the maximum right extent of watering for each starting point
  (define intervals (make-vector (add1 n) 0))
  ;; Populate intervals with the maximum right extent of watering from each tap
  (for ([i (in-range (add1 n))])
    (let* ([left (max 0 (- i (list-ref ranges i)))]
           [right (min n (+ i (list-ref ranges i)))])
      (vector-set! intervals left (max (vector-ref intervals left) (- right left)))))
  ;; Initialize variables to track the current position, total taps needed, and the maximum reach so far
  (define position 0)
  (define total_taps 0)
  (define max_reach 0)
  ;; Loop through the garden until the end is reached
  (let loop ([position position]
             [max_reach max_reach]
             [total_taps total_taps])
    (if (< position n)
        ;; If current position is less than n, update max_reach and check if the garden can be watered
        (let ([new_reach (max max_reach (+ position (vector-ref intervals position)))])
          (if (= new_reach position)
              ;; If max_reach is the same as the current position, the garden cannot be fully watered
              -1
              ;; Otherwise, move to the new max reach position and increment total taps
              (loop new_reach new_reach (add1 total_taps))))
        ;; If the end of the garden is reached, return the total taps needed
        total_taps)))
(require rackunit)

(define (test-humaneval) 

  (let (( candidate min_taps))
    (check-within (candidate 2 (list 2 1 2)) 1 0.001)
    (check-within (candidate 5 (list 3 2 1 1 2 2)) 2 0.001)
    (check-within (candidate 8 (list 4 0 0 0 0 0 0 0 4)) 2 0.001)
    (check-within (candidate 3 (list 2 3 1 1 1)) 1 0.001)
    (check-within (candidate 3 (list 0 0 0 0)) -1 0.001)
    (check-within (candidate 2 (list 1 2 1)) 1 0.001)
    (check-within (candidate 1 (list 1 1)) 1 0.001)
    (check-within (candidate 5 (list 2 2 1 2 1 3)) 2 0.001)
    (check-within (candidate 5 (list 3 4 1 1 0 0)) 1 0.001)
    (check-within (candidate 3 (list 0 0 3 0)) 1 0.001)
    (check-within (candidate 3 (list 3 2 1 1)) 1 0.001)
    (check-within (candidate 3 (list 3 0 0 0)) 1 0.001)
    (check-within (candidate 8 (list 4 0 0 0 4 0 0 0 4)) 1 0.001)
    (check-within (candidate 3 (list 2 1 1 1)) 2 0.001)
    (check-within (candidate 0 (list 0)) 0 0.001)
    (check-within (candidate 10 (list 5 5 2 1 5 1 5 2 1 5 2)) -1 0.001)
    (check-within (candidate 5 (list 3 2 1 1 1 1)) 2 0.001)
    (check-within (candidate 2 (list 2 1 1)) 1 0.001)
    (check-within (candidate 3 (list 2 1 2 1)) 1 0.001)
    (check-within (candidate 3 (list 4 0 0 0 0 0)) 1 0.001)
))

(test-humaneval)