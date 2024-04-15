#lang racket

;; You are given a binary list nums and an integer k.
;; A k-bit flip is choosing a sublist of length k from nums and simultaneously changing every 0 in the sublist to 1, and every 1 in the sublist to 0.
;; Return the minimum number of k-bit flips required so that there is no 0 in the list. If it is not possible, return -1.
;; A sublist is a contiguous part of a list.
;; Example 1:
;; Input: nums = [0,1,0], k = 1
;; Output: 2
;; Explanation: Flip nums[0], then flip nums[2].
;; Example 2:
;; Input: nums = [1,1,0], k = 2
;; Output: -1
;; Explanation: No matter how we flip sublists of size 2, we cannot make the list become [1,1,1].
;; Example 3:
;; Input: nums = [0,0,0,1,0,1,1,0], k = 3
;; Output: 3
;; Explanation: 
;; Flip nums[0],nums[1],nums[2]: nums becomes [1,1,1,1,0,1,1,0]
;; Flip nums[4],nums[5],nums[6]: nums becomes [1,1,1,1,1,0,0,0]
;; Flip nums[5],nums[6],nums[7]: nums becomes [1,1,1,1,1,1,1,1]
;; Constraints:
;; 1 <= nums.length <= 105
;; 1 <= k <= nums.length
(define (min_k_bit_flips nums k)
  ;; Initialize the length of nums, a list to track flips, the number of flips, and the answer.
  (let ([n (length nums)]
        [flipped (make-vector (length nums) 0)]
        [flips 0]
        [ans 0])
    ;; Loop over each index in nums.
    (let loop ([i 0])
      (if (< i n)
          (begin
            ;; Adjust flips count based on the window defined by k.
            (when (>= i k)
              (set! flips (- flips (vector-ref flipped (- i k)))))
            ;; If the current element requires flipping.
            (if (= (modulo (+ flips (list-ref nums i)) 2) 0)
                (if (> (+ i k) n)
                    ;; If flipping is not possible due to the window exceeding array length, return -1.
                    -1
                    ;; Otherwise, mark the current position flipped, increment flips and ans.
                    (begin
                      (vector-set! flipped i 1)
                      (set! flips (+ flips 1))
                      (set! ans (+ ans 1))
                      (loop (+ i 1))))
                ;; If no flip is needed at this position, continue.
                (loop (+ i 1))))
          ;; If all elements have been checked, return the answer.
          ans))))
(require rackunit)

(define (test-humaneval) 

  (let (( candidate min_k_bit_flips))
    (check-within (candidate (list 0 1 0 1 0 1 0 1) 1) 4 0.001)
    (check-within (candidate (list 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1) 3) -1 0.001)
    (check-within (candidate (list 0 0 0 1 0 1 1 0) 3) 3 0.001)
    (check-within (candidate (list 0 1 0) 1) 2 0.001)
    (check-within (candidate (list 1 0 0 1 0 1 1 0) 2) 4 0.001)
    (check-within (candidate (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 2) 8 0.001)
    (check-within (candidate (list 1 1 0) 1) 1 0.001)
    (check-within (candidate (list 1 1 0) 2) -1 0.001)
    (check-within (candidate (list 1 1 1 1) 1) 0 0.001)
    (check-within (candidate (list 0) 1) 1 0.001)
    (check-within (candidate (list 0 0 0 1 0 1 1 0) 2) -1 0.001)
    (check-within (candidate (list 1 0) 2) -1 0.001)
    (check-within (candidate (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) 4) 4 0.001)
    (check-within (candidate (list 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1) 2) 8 0.001)
    (check-within (candidate (list 0 0 0 0 0 0 0 0 0 0) 2) 5 0.001)
))

(test-humaneval)