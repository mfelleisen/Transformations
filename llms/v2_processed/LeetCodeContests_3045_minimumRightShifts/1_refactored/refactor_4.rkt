#lang racket

;; You are given a 0-indexed array nums of length n containing distinct positive integers. Return the minimum number of right shifts required to sort nums and -1 if this is not possible.
;; A right shift is defined as shifting the element at index i to index (i + 1) % n, for all indices.
;; Example 1:
;; Input: nums = [3,4,5,1,2]
;; Output: 2
;; Explanation:
;; After the first right shift, nums = [2,3,4,5,1].
;; After the second right shift, nums = [1,2,3,4,5].
;; Now nums is sorted; therefore the answer is 2.
;; Example 2:
;; Input: nums = [1,3,5]
;; Output: 0
;; Explanation: nums is already sorted therefore, the answer is 0.
;; Example 3:
;; Input: nums = [2,1,4]
;; Output: -1
;; Explanation: It's impossible to sort the array using right shifts.
;; Constraints:
;;  * 1 <= nums.length <= 100
;;  * 1 <= nums[i] <= 100
;;  * nums contains distinct integers.
(define (minimumRightShifts nums)
  (define n (length nums))
  (define sorted-nums (sort nums <))
  
  (define (valid-shift? start)
    (for/and ([i (in-range n)])
      (= (list-ref nums (modulo (+ start i) n))
         (list-ref sorted-nums i))))
  
  (define (find-valid-shift)
    (for/first ([start (in-range n)])
      (when (valid-shift? start)
        (modulo (- n start) n))))
  
  (or (find-valid-shift) -1))


(require rackunit)


(define (test-humaneval) 

  (let (( candidate minimumRightShifts))
    (check-within (candidate (list 3 4 5 1 2)) 2 0.001)
    (check-within (candidate (list 1 3 5)) 0 0.001)
    (check-within (candidate (list 2 1 4)) -1 0.001)
    (check-within (candidate (list 31 72 79 25)) 1 0.001)
    (check-within (candidate (list 27 33 42 58 81 8 9 17)) 3 0.001)
    (check-within (candidate (list 72 13 21 35 52)) 4 0.001)
    (check-within (candidate (list 65 73 77 1)) 1 0.001)
    (check-within (candidate (list 100 8 14 68 80 84)) 5 0.001)
    (check-within (candidate (list 53 60 64 69 98 40)) 1 0.001)
    (check-within (candidate (list 21)) 0 0.001)
    (check-within (candidate (list 78 12 18 21 23 36 64 70)) 7 0.001)
    (check-within (candidate (list 25 26 53 91 92 99 10 24)) 2 0.001)
    (check-within (candidate (list 63 51 65 87 6 17 32 14 42 46)) -1 0.001)
    (check-within (candidate (list 43 46 75 76 85 96 9 19 25)) 3 0.001)
    (check-within (candidate (list 5)) 0 0.001)
    (check-within (candidate (list 35 72 76 82 96 97 24 26)) 2 0.001)
    (check-within (candidate (list 82 30 94 55 76 51 3 89 52 96)) -1 0.001)
    (check-within (candidate (list 57 59 88 97 6 27 41 46 52)) 5 0.001)
    (check-within (candidate (list 17)) 0 0.001)
    (check-within (candidate (list 62)) 0 0.001)
    (check-within (candidate (list 24 46 55 61 71 78 1 4)) 2 0.001)
    (check-within (candidate (list 83 2 21 42 73 77 80)) 6 0.001)
    (check-within (candidate (list 83 94 14 43 50 62 63)) 5 0.001)
    (check-within (candidate (list 38 46 66 77 7 15 17 35)) 4 0.001)
    (check-within (candidate (list 35 68 82 90 9 18 29 34)) 4 0.001)
    (check-within (candidate (list 71)) 0 0.001)
    (check-within (candidate (list 71 73 88 12 49 55 59 70)) 5 0.001)
    (check-within (candidate (list 54 65 75 81 24 37)) 2 0.001)
    (check-within (candidate (list 57 67 73 78 79 2 45 48 51)) 4 0.001)
    (check-within (candidate (list 36 62 65 85 95 9 21)) 2 0.001)
    (check-within (candidate (list 68 12)) 1 0.001)
    (check-within (candidate (list 34 9 86 20 67 94 65 82 40 79)) -1 0.001)
    (check-within (candidate (list 92 84 37 19 16 85 20 79 25 89)) -1 0.001)
    (check-within (candidate (list 3 16 38 44 67 79 84)) 0 0.001)
    (check-within (candidate (list 14 24 58 69 71 94 13)) 1 0.001)
    (check-within (candidate (list 100 18)) 1 0.001)
    (check-within (candidate (list 13)) 0 0.001)
    (check-within (candidate (list 94 30 53 56 67 72 82)) 6 0.001)
    (check-within (candidate (list 92 14 65 80 85)) 4 0.001)
    (check-within (candidate (list 43 53 81 87 93 19 31 39)) 3 0.001)
    (check-within (candidate (list 80 38)) 1 0.001)
    (check-within (candidate (list 52 72 78 83 85 99 20)) 1 0.001)
    (check-within (candidate (list 3 6 89)) 0 0.001)
    (check-within (candidate (list 3)) 0 0.001)
    (check-within (candidate (list 55 56 63 91 3 46)) 2 0.001)
    (check-within (candidate (list 58 10 31 37 41)) 4 0.001)
    (check-within (candidate (list 17 33 53 58 78)) 0 0.001)
    (check-within (candidate (list 82 44)) 1 0.001)
    (check-within (candidate (list 89 96 35 48 57 71)) 4 0.001)
    (check-within (candidate (list 43 69 4 29 37)) 3 0.001)
    (check-within (candidate (list 65 88)) 0 0.001)
    (check-within (candidate (list 42 44 59 76 86)) 0 0.001)
    (check-within (candidate (list 29 56 78 96 1 10 27)) 3 0.001)
    (check-within (candidate (list 48 100)) 0 0.001)
    (check-within (candidate (list 4 33 17 3 8 91 28 13 72 42)) -1 0.001)
    (check-within (candidate (list 5 35 53 56)) 0 0.001)
    (check-within (candidate (list 65 67 70 27 41 50 52 57 60)) 6 0.001)
    (check-within (candidate (list 94 32 45 62)) 3 0.001)
    (check-within (candidate (list 23 25 34 47 61 65 6 21)) 2 0.001)
    (check-within (candidate (list 99 11 12 21 22 55 62 83)) 7 0.001)
    (check-within (candidate (list 92 13 33 58 61 85)) 5 0.001)
    (check-within (candidate (list 46)) 0 0.001)
    (check-within (candidate (list 12 27 30 36)) 0 0.001)
    (check-within (candidate (list 33 44 57 16 22 26 30)) 4 0.001)
    (check-within (candidate (list 67 24)) 1 0.001)
    (check-within (candidate (list 12 44 83 87)) 0 0.001)
    (check-within (candidate (list 19 52 3 8 12)) 3 0.001)
    (check-within (candidate (list 82 86 88 6 35 47 52 58 62)) 6 0.001)
    (check-within (candidate (list 48)) 0 0.001)
    (check-within (candidate (list 60 11)) 1 0.001)
    (check-within (candidate (list 69 60)) 1 0.001)
    (check-within (candidate (list 22 28 36 16 82 77 41 85 44 97)) -1 0.001)
    (check-within (candidate (list 63 94 2 14)) 2 0.001)
    (check-within (candidate (list 41 45 74 84 90 93 100 18 31)) 2 0.001)
    (check-within (candidate (list 21 38 57 64 12)) 1 0.001)
    (check-within (candidate (list 99 2 9 17 33 58 59 72)) 7 0.001)
    (check-within (candidate (list 36 89 90 98 11 14 23)) 3 0.001)
    (check-within (candidate (list 84 90 5 57 78)) 3 0.001)
    (check-within (candidate (list 48 73 76 30)) 1 0.001)
    (check-within (candidate (list 74)) 0 0.001)
    (check-within (candidate (list 21 84 35 65 12 74 30 95 46 23)) -1 0.001)
    (check-within (candidate (list 64 76 46 53 54)) 3 0.001)
    (check-within (candidate (list 77 84 89 47 52 74)) 3 0.001)
    (check-within (candidate (list 12 29 31 52 88 89 10)) 1 0.001)
    (check-within (candidate (list 20 25 28 41 57 89)) 0 0.001)
    (check-within (candidate (list 1 28 51 59)) 0 0.001)
    (check-within (candidate (list 59 76 2 26 49 78 36 70 64 24)) -1 0.001)
    (check-within (candidate (list 35 43 49 63 21)) 1 0.001)
    (check-within (candidate (list 1 35 38 47 54 56 58 74)) 0 0.001)
    (check-within (candidate (list 49 94 97)) 0 0.001)
    (check-within (candidate (list 32 30)) 1 0.001)
    (check-within (candidate (list 37 36)) 1 0.001)
    (check-within (candidate (list 31 41 65 14)) 1 0.001)
    (check-within (candidate (list 45 57 73 77 17 30 42 43)) 4 0.001)
    (check-within (candidate (list 17 65 11)) 1 0.001)
    (check-within (candidate (list 32 84 93 31 61 78 15 52 100 65)) -1 0.001)
    (check-within (candidate (list 61 72 90 3 8 17 23 55)) 5 0.001)
    (check-within (candidate (list 19 30 44 95 13)) 1 0.001)
    (check-within (candidate (list 42 46 66 71 87 3 4 5 14)) 4 0.001)
    (check-within (candidate (list 13 57 7)) 1 0.001)
))

(test-humaneval)