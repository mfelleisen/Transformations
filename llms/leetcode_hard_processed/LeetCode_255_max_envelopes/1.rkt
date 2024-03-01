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
  ;; First, sort the envelopes list. 
  ;; Sort by width ascending and then by height descending if widths are equal.
  (define sorted-envelopes
    (sort envelopes
          (lambda (a b)
            (or (< (car a) (car b))
                (and (= (car a) (car b))
                     (> (cadr a) (cadr b)))))))
  
  ;; Initialize an empty list to keep track of the heights in the increasing subsequence
  (define dp '())
  
  ;; A helper function to perform binary search and return the index where the height
  ;; should be inserted or replaced.
  (define (binary-search list target)
    (let loop ((low 0) (high (length list)))
      (if (< low high)
          (let ((mid (+ low (quotient (- high low) 2))))
            (if (< (list-ref list mid) target)
                (loop (add1 mid) high)
                (loop low mid)))
          low)))
  
  ;; Iterate over the sorted envelopes, using only the heights for LIS calculation.
  (for-each (lambda (env)
              (let ((idx (binary-search dp (cadr env))))
                ;; If idx is equal to the length of dp, it means the height is larger than
                ;; any element in dp and can be appended to dp.
                ;; Otherwise, replace the element at idx with the current height.
                (if (= idx (length dp))
                    (set! dp (append dp (list (cadr env))))
                    (set! dp (append (take dp idx) (list (cadr env)) (drop dp (add1 idx)))))))
            sorted-envelopes)
  
  ;; The length of dp is the maximum number of envelopes that can be Russian dolled.
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