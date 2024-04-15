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
  ;; Create a mapping from person IDs to their positions.
  (define position (make-vector (* 2 (length row))))
  (for ([i (in-range (length row))])
    (vector-set! position (list-ref row i) i))
  
  ;; Initialize the number of swaps to 0.
  (define swaps 0)
  
  ;; Iterate over the row by two steps, checking each couple.
  (for ([i (in-range 0 (length row) 2)])
    ;; Determine the partner ID. If ID is even, add 1, else subtract 1.
    (define partner (if (even? (list-ref row i))
                        (+ (list-ref row i) 1)
                        (- (list-ref row i) 1)))
    
    ;; If the partner is not sitting next to the person, swap them.
    (when (not (= (list-ref row (+ i 1)) partner))
      ;; Perform the swap in the 'row' list.
      (define partner-pos (vector-ref position partner))
      (define temp (list-ref row (+ i 1)))
      (set! row (list-set row (+ i 1) (list-ref row partner-pos)))
      (set! row (list-set row partner-pos temp))
      
      ;; Update the positions in the 'position' vector.
      (vector-set! position (list-ref row (+ i 1)) (+ i 1))
      (vector-set! position partner partner-pos)
      
      ;; Increment the swap count.
      (set! swaps (+ swaps 1))))
  
  ;; Return the number of swaps required.
  swaps)

;; The function can be tested with the provided examples:
(minSwapsCouples '(0 2 1 3)) ;; Expected output: 1
(minSwapsCouples '(3 2 0 1)) ;; Expected output: 0
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