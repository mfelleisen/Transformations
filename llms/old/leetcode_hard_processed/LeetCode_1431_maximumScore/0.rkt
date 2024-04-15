#lang racket

;; Function to calculate the maximum score by choosing integers from either end of the list `nums`
;; and multiplying them by corresponding values from the list `multipliers`.
;; You are given two 0-indexed integer lists nums and multipliers of size n and m respectively, where n >= m.
;; You begin with a score of 0. You want to perform exactly m operations. On the ith operation (0-indexed) you will:
;; Choose one integer x from either the start or the end of the list nums.
;; Add multipliers[i] * x to your score.
;; Note that multipliers[0] corresponds to the first operation, multipliers[1] to the second operation, and so on.
;; Remove x from nums.
;; Return the maximum score after performing m operations.
;; Example 1:
;; Input: nums = [1,2,3], multipliers = [3,2,1]
;; Output: 14
;; Explanation: An optimal solution is as follows:
;; - Choose from the end, [1,2,3], adding 3 * 3 = 9 to the score.
;; - Choose from the end, [1,2], adding 2 * 2 = 4 to the score.
;; - Choose from the end, [1], adding 1 * 1 = 1 to the score.
;; The total score is 9 + 4 + 1 = 14.
;; Example 2:
;; Input: nums = [-5,-3,-3,-2,7,1], multipliers = [-10,-5,3,4,6]
;; Output: 102
;; Explanation: An optimal solution is as follows:
;; - Choose from the start, [-5,-3,-3,-2,7,1], adding -5 * -10 = 50 to the score.
;; - Choose from the start, [-3,-3,-2,7,1], adding -3 * -5 = 15 to the score.
;; - Choose from the start, [-3,-2,7,1], adding -3 * 3 = -9 to the score.
;; - Choose from the end, [-2,7,1], adding 1 * 4 = 4 to the score.
;; - Choose from the end, [-2,7], adding 7 * 6 = 42 to the score. 
;; The total score is 50 + 15 - 9 + 4 + 42 = 102.
;; Constraints:
;; n == nums.length
;; m == multipliers.length
;; 1 <= m <= 300
;; m <= n <= 105 
;; -1000 <= nums[i], multipliers[i] <= 1000
(define (maximumScore nums multipliers)
  (let ([n (length nums)] [m (length multipliers)])
    ;; Initialize a 2D list (matrix) for dynamic programming, filled with 0s.
    (define dp (make-vector (add1 m) (make-vector (add1 m) 0)))
    (define maxScore -inf.0) ;; Initialize the maximum score with negative infinity.
    
    ;; Populate the dp matrix with possible scores.
    (for ([i (in-range (add1 m))])
      (for ([j (in-range (- (add1 m) i))])
        ;; Skip the initial case where both i and j are 0.
        (when (or (> i 0) (> j 0))
          (define score 0)
          (cond
            [(= i 0) ;; Choose from the end when i is 0.
             (set! score (+ (vector-ref (vector-ref dp i) (- j 1))
                            (* (list-ref nums (- n j))
                               (list-ref multipliers (+ i j -1)))))]
            [(= j 0) ;; Choose from the start when j is 0.
             (set! score (+ (vector-ref (vector-ref dp (- i 1)) j)
                            (* (list-ref nums (- i 1))
                               (list-ref multipliers (+ i j -1)))))]
            [else ;; Choose the maximum score between choosing from the start or the end.
             (set! score (max (+ (vector-ref (vector-ref dp (- i 1)) j)
                                 (* (list-ref nums (- i 1))
                                    (list-ref multipliers (+ i j -1))))
                              (+ (vector-ref (vector-ref dp i) (- j 1))
                                 (* (list-ref nums (- n j))
                                    (list-ref multipliers (+ i j -1))))))])
          ;; Update the dp matrix with the calculated score.
          (vector-set! (vector-ref dp i) j score)
          ;; Update the maximum score if i + j equals m.
          (when (= (+ i j) m)
            (set! maxScore (max maxScore score))))))
    
    ;; Return the maximum score obtained after performing m operations.
    maxScore))

;; Example usage:
(maximumScore '(1 2 3) '(3 2 1))
(maximumScore '(-5 -3 -3 -2 7 1) '(-10 -5 3 4 6))
(require rackunit)

(define (test-humaneval) 

  (let (( candidate maximumScore))
    (check-within (candidate (list 1 2 3) (list 3 2 1)) 14 0.001)
    (check-within (candidate (list -1 -2 -3 -4 -5) (list 1 1 1 1 1)) -15 0.001)
    (check-within (candidate (list 1 2 3 4 5) (list -1 -1 -1 -1 -1)) -15 0.001)
    (check-within (candidate (list -1 -2 -3 -4 -5 -6 -7 -8 -9 -10) (list -1 -2 -3 -4 -5 -6 -7 -8 -9 -10)) 385 0.001)
    (check-within (candidate (list 1) (list 1)) 1 0.001)
    (check-within (candidate (list 1 2 3 4 5 6 7 8 9 10) (list 1 2 3 4 5 6 7 8 9 10)) 385 0.001)
    (check-within (candidate (list 1 2 3 4 5) (list 1 1 1 1 1)) 15 0.001)
    (check-within (candidate (list -5 -3 -3 -2 7 1) (list -10 -5 3 4 6)) 102 0.001)
    (check-within (candidate (list 1 2) (list 3 4)) 11 0.001)
))

(test-humaneval)