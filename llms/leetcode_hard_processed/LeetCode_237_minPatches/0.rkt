#lang racket

;; Given a sorted integer list nums and an integer n, add/patch elements to the list such that any number in the range [1, n] inclusive can be formed by the sum of some elements in the list.
;; Return the minimum number of patches required.
;; Example 1:
;; Input: nums = [1,3], n = 6
;; Output: 1
;; Explanation:
;; Combinations of nums are [1], [3], [1,3], which form possible sums of: 1, 3, 4.
;; Now if we add/patch 2 to nums, the combinations are: [1], [2], [3], [1,3], [2,3], [1,2,3].
;; Possible sums are 1, 2, 3, 4, 5, 6, which now covers the range [1, 6].
;; So we only need 1 patch.
;; Example 2:
;; Input: nums = [1,5,10], n = 20
;; Output: 2
;; Explanation: The two patches can be [2, 4].
;; Example 3:
;; Input: nums = [1,2,2], n = 5
;; Output: 0
;; Constraints:
;; 1 <= nums.length <= 1000
;; 1 <= nums[i] <= 104
;; nums is sorted in ascending order.
;; 1 <= n <= 231 - 1
(define (minPatches nums n)
  ;; max-sum tracks the current maximum sum we can achieve with the numbers in nums and the patches we've added.
  ;; patches counts how many numbers we've had to add to the list to cover the range [1, n].
  ;; i is our index to iterate through nums.
  (let loop ([max-sum 0] [patches 0] [i 0])
    (cond
      ;; If max-sum >= n, we've covered the range [1, n], so we return the number of patches added.
      [(>= max-sum n) patches]
      ;; If i is within bounds and nums[i] is <= max-sum + 1, we can extend our range without adding a patch.
      [(and (< i (length nums)) (<= (list-ref nums i) (+ max-sum 1)))
       (loop (+ max-sum (list-ref nums i)) patches (+ i 1))]
      ;; Otherwise, we need to add a patch, which doubles the current max-sum + 1,
      ;; effectively extending our range to cover the next number not currently representable.
      [else
       (loop (+ max-sum (+ max-sum 1)) (+ patches 1) i)])))

;; Example usage:
(minPatches '(1 3) 6)  ;; Output: 1
(minPatches '(1 5 10) 20)  ;; Output: 2
(minPatches '(1 2 2) 5)  ;; Output: 0
(require rackunit)

(define (test-humaneval) 

  (let (( candidate minPatches))
    (check-within (candidate (list 1 2 4 8 16 32 64 128 256 512) 1023) 0 0.001)
    (check-within (candidate (list 1 2 3 4 5 6 7 8 9 10) 50) 0 0.001)
    (check-within (candidate (list 1 2 2) 5) 0 0.001)
    (check-within (candidate (list 1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536 131072 262144 524288 1048576) 2047) 0 0.001)
    (check-within (candidate (list 2 4 8 16 32) 64) 2 0.001)
    (check-within (candidate (list 1 3 6 12) 24) 1 0.001)
    (check-within (candidate (range 1 513) 8181) 0 0.001)
    (check-within (candidate (list 1 5 10) 20) 2 0.001)
    (check-within (candidate (list 1 2 4 8) 15) 0 0.001)
    (check-within (candidate (list 1 2 4 8 16) 31) 0 0.001)
    (check-within (candidate (list 1 2 3 4 5 6 7 8 9 10) 55) 0 0.001)
    (check-within (candidate (list 1 2 4 8 16 32) 63) 0 0.001)
    (check-within (candidate (list 1 2 3 4 5 6 7 8 9 10) 37) 0 0.001)
    (check-within (candidate (list 1 2 4) 7) 0 0.001)
    (check-within (candidate (list 1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536 131072 262144 524288 1048576) 2048) 0 0.001)
    (check-within (candidate (list 2 4 8 16 32) 1) 1 0.001)
    (check-within (candidate (list 1 3 5 7) 12) 1 0.001)
    (check-within (candidate (list 1 2 4 8 16 32 64 128 256 512) 1025) 1 0.001)
    (check-within (candidate (list 1 2 3 4 5 6 7 8 9 10) 56) 1 0.001)
    (check-within (candidate (list 1 3) 6) 1 0.001)
    (check-within (candidate (list 1 2 4 8 16 32) 65) 1 0.001)
    (check-within (candidate (list 1 4 8 16) 20) 1 0.001)
))

(test-humaneval)