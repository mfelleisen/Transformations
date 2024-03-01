#lang racket

;; You are given a 2D list of integers envelopes where envelopes[i] = [wi, hi] represents the width and the height of an envelope.
;; One envelope can fit into another if and only if both the width and height of one envelope are greater than the other envelope's width and height.
;; Return the maximum number of envelopes you can Russian doll (i.e., put one inside the other).
;; Note: You cannot rotate an envelope.
;; Example 1:
;; Input: envelopes = [[5,4],[6,4],[6,7],[2,3]]
;; Output: 3
;; Explanation: The maximum number of envelopes you can Russian doll is 3 ([2,3] => [5,4] => [6,7]).
;; Example 2:
;; Input: envelopes = [[1,1],[1,1],[1,1]]
;; Output: 1
;; Constraints:
;; 1 <= envelopes.length <= 105
;; envelopes[i].length == 2
;; 1 <= wi, hi <= 105
(define (max_envelopes envelopes)
  ;; Sort the envelopes. First by width in ascending order, then by height in descending order.
  ;; This ensures that when iterating over the heights, we can apply the Longest Increasing Subsequence (LIS) pattern.
  (define sorted-envs (sort envelopes
                            (lambda (env1 env2)
                              (or (< (car env1) (car env2))
                                  (and (= (car env1) (car env2))
                                       (> (cadr env1) (cadr env2)))))))
  
  ;; Helper function: binary search to find the leftmost position to insert the height
  ;; so that the dp list (which tracks the heights of the envelopes in the increasing sequence)
  ;; remains in sorted order.
  (define (insert-position dp h)
    (let loop ((low 0) (high (length dp)))
      (if (< low high)
          (let ((mid (+ low (quotient (- high low) 2))))
            (if (< (list-ref dp mid) h)
                (loop (add1 mid) high)
                (loop low mid)))
          low)))
  
  ;; dp list to track the heights of the envelopes in the increasing sequence.
  (define dp (list))
  
  ;; Iterate over each envelope, applying the logic of the Longest Increasing Subsequence (LIS)
  ;; on the heights of the sorted envelopes.
  (for-each (lambda (env)
              (let* ((h (cadr env)) ;; Extract height
                     (pos (insert-position dp h)))
                ;; If pos is equal to the length of dp, it means h is greater than all elements in dp
                ;; and can be appended. Otherwise, replace the element at pos with h.
                (if (= pos (length dp))
                    (set! dp (append dp (list h)))
                    (begin
                      (set! dp (list-set dp pos h))))))
            sorted-envs)
  
  ;; The length of dp represents the maximum number of envelopes
  ;; that can be Russian dolled.
  (length dp))
(require rackunit)

(define (test-humaneval) 

  (let (( candidate max_envelopes))
    (check-within (candidate (list (list 5 4) (list 6 4) (list 6 7) (list 2 3) (list 1 2))) 4 0.001)
    (check-within (candidate (list (list 1 1) (list 1 1) (list 1 1))) 1 0.001)
    (check-within (candidate (list (list 1 10) (list 2 9) (list 3 8) (list 4 7) (list 5 6) (list 6 5) (list 7 4) (list 8 3) (list 9 2) (list 10 1))) 1 0.001)
    (check-within (candidate (list (list 1 2) (list 2 3) (list 3 4) (list 4 5) (list 5 6) (list 6 7) (list 7 8) (list 8 9) (list 9 10) (list 10 11))) 10 0.001)
    (check-within (candidate (list (list 10 20) (list 20 30) (list 30 40) (list 40 50))) 4 0.001)
    (check-within (candidate (list (list 5 4) (list 6 4) (list 6 7) (list 2 3))) 3 0.001)
    (check-within (candidate (list (list 1 1) (list 2 2) (list 3 3) (list 4 4) (list 5 5))) 5 0.001)
    (check-within (candidate (list (list 10 20) (list 15 25) (list 20 30) (list 25 35) (list 30 40))) 5 0.001)
    (check-within (candidate (list (list 1 2) (list 2 3) (list 3 4) (list 4 5) (list 5 6) (list 6 7) (list 7 8) (list 8 9) (list 9 10) (list 11 12))) 10 0.001)
    (check-within (candidate (list (list 5 5) (list 5 5) (list 5 5) (list 5 5) (list 5 5) (list 5 5) (list 5 5) (list 5 5) (list 5 5) (list 5 5))) 1 0.001)
    (check-within (candidate (list (list 1 1))) 1 0.001)
    (check-within (candidate (list (list 2 3) (list 3 4) (list 4 5) (list 5 6) (list 6 7) (list 7 8) (list 8 9) (list 9 10) (list 10 11) (list 11 12))) 10 0.001)
    (check-within (candidate (list (list 4 5) (list 5 6) (list 6 7) (list 7 8))) 4 0.001)
))

(test-humaneval)