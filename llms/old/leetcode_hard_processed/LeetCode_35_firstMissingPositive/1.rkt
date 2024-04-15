#lang racket

;; Given an unsorted integer list nums, return the smallest missing positive integer.
;; You must implement an algorithm that runs in O(n) time and uses O(1) auxiliary space.
;; Example 1:
;; Input: nums = [1,2,0]
;; Output: 3
;; Explanation: The numbers in the range [1,2] are all in the list.
;; Example 2:
;; Input: nums = [3,4,-1,1]
;; Output: 2
;; Explanation: 1 is in the list but 2 is missing.
;; Example 3:
;; Input: nums = [7,8,9,11,12]
;; Output: 1
;; Explanation: The smallest positive integer 1 is missing.
;; Constraints:
;; 1 <= nums.length <= 105
;; -231 <= nums[i] <= 231 - 1
(define (firstMissingPositive nums)
  ;; Create a procedure to swap elements at indices i and j in a list
  (define (swap lst i j)
    (for/list ([index (in-range (length lst))])
      (cond
        [(= index i) (list-ref lst j)]
        [(= index j) (list-ref lst i)]
        [else (list-ref lst index)])))
  
  ;; Create a procedure to place each number in its right place if possible
  ;; For example, 3 should be placed at index 2 (considering 1-based indexing in explanation)
  (define (place-correctly lst)
    (let loop ([cur-list lst] [i 0])
      (if (< i (length cur-list))
          (let* ([val (list-ref cur-list i)]
                 [correct-pos (sub1 val)]) ; Convert to 0-based index
            (if (and (<= 1 val) (<= val (length cur-list)) (not (= val (list-ref cur-list correct-pos))))
                (loop (swap cur-list i correct-pos) i) ; Swap and repeat without incrementing i
                (loop cur-list (add1 i)))) ; Move to next index
          cur-list))) ; Return the list when done
  
  ;; Create a procedure to find the first missing positive
  (define (find-first-missing lst)
    (for/or ([i (in-range (length lst))])
      (if (not (= (add1 i) (list-ref lst i)))
          (add1 i) ; Return the first missing positive (1-based index)
          #f))) ; Continue if not found
  
  ;; Place every number in its correct place
  (let ([corrected-list (place-correctly nums)])
    ;; Find the first missing positive
    (or (find-first-missing corrected-list)
        (add1 (length nums))))) ; If all positions are filled correctly, answer is length + 1

;; Test the function with examples
(firstMissingPositive '(1 2 0)) ; => 3
(firstMissingPositive '(3 4 -1 1)) ; => 2
(firstMissingPositive '(7 8 9 11 12)) ; => 1
(require rackunit)

(define (test-humaneval) 

  (let (( candidate firstMissingPositive))
    (check-within (candidate (list 3 4 -1 1)) 2 0.001)
    (check-within (candidate (list 5 4 3 2 1)) 6 0.001)
    (check-within (candidate (list 1 1 1 1)) 2 0.001)
    (check-within (candidate (list 1)) 2 0.001)
    (check-within (candidate (list 2)) 1 0.001)
    (check-within (candidate (list 1 2 5 7 9 11)) 3 0.001)
    (check-within (candidate (list 7 8 9 11 12)) 1 0.001)
    (check-within (candidate (list 1 2 3 4 5)) 6 0.001)
    (check-within (candidate (list 1 2 3 5 5 6 6 7 8 9 10)) 4 0.001)
    (check-within (candidate (list 0 2 1 4 3 6 5 -2 7 9 8 11 10 12 13 14 15 16 17 18)) 19 0.001)
    (check-within (candidate (list 3 4 -1 1)) 2 0.001)
    (check-within (candidate (list 1 1 1 1 1 2 2 2 2 2 3 3 3 3 3)) 4 0.001)
    (check-within (candidate (list 1 1 1 1)) 2 0.001)
    (check-within (candidate (list -1 -2 -3 -4)) 1 0.001)
    (check-within (candidate (list 7 8 9 11 12)) 1 0.001)
    (check-within (candidate (list -1)) 1 0.001)
    (check-within (candidate (list -2 -3 -5 -1 -7 -10 -11 -12 -13 -15 -16 -17 -18 -19 -20 2 1 6 7 3)) 4 0.001)
    (check-within (candidate (list 0)) 1 0.001)
    (check-within (candidate (list 1 2 3 4 5 7 8 9 10)) 6 0.001)
    (check-within (candidate (list 1 2 0)) 3 0.001)
    (check-within (candidate (list 2 3)) 1 0.001)
    (check-within (candidate (list 2 2 2 2 3 3 3 3)) 1 0.001)
    (check-within (candidate (list 1 1 1 1 1 2 2 2 2 2 3 3 3 3 3)) 4 0.001)
    (check-within (candidate (list 1 2 0)) 3 0.001)
    (check-within (candidate (list 0)) 1 0.001)
    (check-within (candidate (list 100 101 102 103 104)) 1 0.001)
    (check-within (candidate (list 2 5 6 8 11 15)) 1 0.001)
    (check-within (candidate (list 2)) 1 0.001)
    (check-within (candidate (list 0 0 0 0)) 1 0.001)
    (check-within (candidate (list 1 1 2 2 3 3)) 4 0.001)
    (check-within (candidate (list 1 2 3 5 5 6 6 7 8 9 10)) 4 0.001)
    (check-within (candidate (list -1)) 1 0.001)
    (check-within (candidate (list 1 2 5 7 9 11)) 3 0.001)
    (check-within (candidate (list 5 4 3 2 1)) 6 0.001)
))

(test-humaneval)