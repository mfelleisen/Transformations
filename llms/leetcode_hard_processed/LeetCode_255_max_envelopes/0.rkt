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
  ;; First, sort the envelopes by width in ascending order and then by height in descending order.
  ;; This way, when we are iterating through heights for the same width,
  ;; we won't choose more than one envelope of the same width because the heights are sorted in descending order,
  ;; ensuring that a larger height doesn't come after a smaller one.
  (let ([sorted-envs (sort envelopes
                           (lambda (e1 e2)
                             (or (< (car e1) (car e2))
                                 (and (= (car e1) (car e2))
                                      (> (cadr e1) (cadr e2))))))])
    ;; dp will store the increasing sequence of heights. We initialize it as an empty list.
    (let loop ([sorted-envs sorted-envs]
               [dp '()])
      (if (null? sorted-envs)
          ;; The length of dp is the maximum number of envelopes we can Russian doll.
          (length dp)
          (let* ([env (car sorted-envs)] ;; Current envelope
                 [h (cadr env)] ;; Height of the current envelope
                 [idx (bisect-left dp h 0 (length dp))]) ;; Find the position where h fits in dp
            ;; If idx is equal to the length of dp, it means h is greater than all elements in dp, so we append it.
            ;; Otherwise, we replace the element at idx with h, maintaining the increasing sequence.
            (let ([new-dp (if (= idx (length dp))
                              (append dp (list h))
                              (update-list dp idx h))])
              (loop (cdr sorted-envs) new-dp)))))))

;; Helper function to find the index where to insert the element in a sorted list
(define (bisect-left lst x low high)
  (if (= low high)
      low
      (let* ([mid (+ low (quotient (- high low) 2))]
             [mid-val (list-ref lst mid)])
        (if (< mid-val x)
            (bisect-left lst x (+ mid 1) high)
            (bisect-left lst x low mid)))))

;; Helper function to replace an element at a specific index in a list
(define (update-list lst idx val)
  (if (= idx 0)
      (cons val (cdr lst))
      (cons (car lst) (update-list (cdr lst) (- idx 1) val))))

;; Example usage
(max_envelopes '((5 4) (6 4) (6 7) (2 3))) ;; Returns: 3
(max_envelopes '((1 1) (1 1) (1 1)))       ;; Returns: 1
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