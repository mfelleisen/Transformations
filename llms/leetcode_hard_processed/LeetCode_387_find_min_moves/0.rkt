#lang racket

;; You have n super washing machines on a line. Initially, each washing machine has some dresses or is empty.
;; For each move, you could choose any m (1 <= m <= n) washing machines, and pass one dress of each washing machine to one of its adjacent washing machines at the same time.
;; Given an integer list machines representing the number of dresses in each washing machine from left to right on the line, return the minimum number of moves to make all the washing machines have the same number of dresses. If it is not possible to do it, return -1.
;; Example 1:
;; Input: machines = [1,0,5]
;; Output: 3
;; Explanation:
;; 1st move:    1     0 <-- 5    =>    1     1     4
;; 2nd move:    1 <-- 1 <-- 4    =>    2     1     3
;; 3rd move:    2     1 <-- 3    =>    2     2     2
;; Example 2:
;; Input: machines = [0,3,0]
;; Output: 2
;; Explanation:
;; 1st move:    0 <-- 3     0    =>    1     2     0
;; 2nd move:    1     2 --> 0    =>    1     1     1
;; Example 3:
;; Input: machines = [0,2,0]
;; Output: -1
;; Explanation:
;; It's impossible to make all three washing machines have the same number of dresses.
;; Constraints:
;; n == machines.length
;; 1 <= n <= 104
;; 0 <= machines[i] <= 105
(define (find_min_moves machines)
  ;; Calculate the total number of dresses and the number of machines.
  (let* ((total-dresses (apply + machines))
         (n (length machines))
         ;; Check if it's possible to distribute dresses evenly.
         (possible? (zero? (remainder total-dresses n))))
    (if (not possible?)
        ;; If it's not possible, return -1.
        -1
        ;; If it's possible, calculate the minimum number of moves.
        (let ((average (/ total-dresses n)))
          (let loop ((i 0)                  ;; Current index in machines.
                     (imbalance 0)         ;; Current imbalance in distribution.
                     (moves 0))            ;; Maximum moves required so far.
            (if (= i n)
                ;; If we've processed all machines, return the moves required.
                moves
                (let* ((current-imbalance (+ imbalance (- (list-ref machines i) average)))
                       ;; Calculate the moves needed considering the current imbalance and the requirement of the current machine.
                       (current-moves (max moves (abs current-imbalance) (- (list-ref machines i) average))))
                  (loop (+ i 1) current-imbalance current-moves))))))))
(require rackunit)

(define (test-humaneval) 

  (let (( candidate find_min_moves))
    (check-within (candidate (list 10 0 0)) -1 0.001)
    (check-within (candidate (list 1 0 5)) 3 0.001)
    (check-within (candidate (list 0 3 0)) 2 0.001)
    (check-within (candidate (list 1000000000 1000000000 1000000000 1000000000 1000000000 1000000000 1000000000 1000000000 1000000000)) 0 0.001)
    (check-within (candidate (list 10 10 10)) 0 0.001)
    (check-within (candidate (list 0 0 0)) 0 0.001)
    (check-within (candidate (list 1000 1000 1000)) 0 0.001)
    (check-within (candidate (list 1000 1000 1000 1000 1000)) 0 0.001)
    (check-within (candidate (list 1000 0 0)) -1 0.001)
    (check-within (candidate (list 1 1 1 1 1)) 0 0.001)
    (check-within (candidate (list 0 0 100)) -1 0.001)
    (check-within (candidate (list 0 100 0)) -1 0.001)
    (check-within (candidate (list 200000000000000000 200000000000000000 200000000000000000)) 0 0.001)
    (check-within (candidate (list 2 2 2 2 2)) 0 0.001)
    (check-within (candidate (list 1 1 1)) 0 0.001)
    (check-within (candidate (list 0 2 0)) -1 0.001)
    (check-within (candidate (list 100 100 100)) 0 0.001)
    (check-within (candidate (list 5 5 5)) 0 0.001)
    (check-within (candidate (list 1000000000000000000 1000000000000000000)) 0 0.001)
    (check-within (candidate (list 5 0 0)) -1 0.001)
    (check-within (candidate (list 3 3 3 3 3)) 0 0.001)
))

(test-humaneval)