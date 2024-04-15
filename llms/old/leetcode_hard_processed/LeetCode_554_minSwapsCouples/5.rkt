#lang racket

;; There are n couples sitting in 2n seats arranged in a row and want to hold hands.
;; The people and seats are represented by an integer list row where row[i] is the ID of the person sitting in the ith seat. The couples are numbered in order, the first couple being (0, 1), the second couple being (2, 3), and so on with the last couple being (2n - 2, 2n - 1).
;; Return the minimum number of swaps so that every couple is sitting side by side. A swap consists of choosing any two people, then they stand up and switch seats.
;; Example 1:
;; Input: row = [0,2,1,3]
;; Output: 1
;; Explanation: We only need to swap the second (row[1]) and third (row[2]) person.
;; Example 2:
;; Input: row = [3,2,0,1]
;; Output: 0
;; Explanation: All couples are already seated side by side.
;; Constraints:
;; 2n == row.length
;; 2 <= n <= 30
;; n is even.
;; 0 <= row[i] < 2n
;; All the elements of row are unique.
(define (minSwapsCouples row)
  ;; Create a list to store the current position of each person.
  (define positions (make-vector (length row)))
  ;; Populate the positions vector with the initial positions of each person.
  (for ([i (in-range (length row))])
    (vector-set! positions (list-ref row i) i))
  
  (define swaps 0) ; Initialize swaps counter.
  
  ;; Iterate over each pair in the row.
  (for ([i (in-range 0 (length row) 2)])
    ;; Determine the partner of the current person. If the current ID is even, the partner's ID is next (odd),
    ;; and vice versa if the current ID is odd (partner's ID is previous, even).
    (define partner (if (even? (list-ref row i))
                        (+ 1 (list-ref row i))
                        (- (list-ref row i) 1)))
    
    ;; If the current person's partner is not already next to them, perform a swap.
    (unless (= partner (list-ref row (+ i 1)))
      ;; Swap the partners in the row list.
      (let* ([partner-pos (vector-ref positions partner)]
             [next-person (list-ref row (+ i 1))])
        (set! row (list-set row (+ i 1) partner))
        (set! row (list-set row partner-pos next-person))
        ;; Update their positions in the positions vector.
        (vector-set! positions partner (+ i 1))
        (vector-set! positions next-person partner-pos)
        ;; Increment the swap counter.
        (set! swaps (+ swaps 1))))
    )
  swaps) ; Return the total number of swaps required.

;; Example usage:
(minSwapsCouples '(0 2 1 3)) ; Output: 1
(minSwapsCouples '(3 2 0 1)) ; Output: 0
(require rackunit)

(define (test-humaneval) 

  (let (( candidate minSwapsCouples))
    (check-within (candidate (list 0 1 2 3)) 0 0.001)
    (check-within (candidate (list 0 2 1 3 4 5)) 1 0.001)
    (check-within (candidate (list 0 3 2 1 6 5 4 7)) 2 0.001)
    (check-within (candidate (list 0 2 1 3)) 1 0.001)
    (check-within (candidate (list 0 1 2 3)) 0 0.001)
    (check-within (candidate (list 0 3 2 1)) 1 0.001)
    (check-within (candidate (list 0 2 1 4 3 5)) 2 0.001)
    (check-within (candidate (list 2 0 1 3)) 1 0.001)
    (check-within (candidate (list 5 0 2 1 4 3)) 2 0.001)
    (check-within (candidate (list 3 2 0 1)) 0 0.001)
    (check-within (candidate (list 0 1 2 3 4 5)) 0 0.001)
    (check-within (candidate (list 0 2 1 3)) 1 0.001)
    (check-within (candidate (list 0 2 1 3 4 5 6 7)) 1 0.001)
    (check-within (candidate (list 0 2 5 3 4 1)) 2 0.001)
    (check-within (candidate (list 3 2 0 1)) 0 0.001)
    (check-within (candidate (list 0 2 1 3 5 4)) 1 0.001)
))

(test-humaneval)