#lang racket

;; You are given a 0-indexed array of integers nums.
;; A prefix nums[0..i] is sequential if, for all 1 <= j <= i, nums[j] = nums[j - 1] + 1. In particular, the prefix consisting only of nums[0] is sequential.
;; Return the smallest integer x missing from nums such that x is greater than or equal to the sum of the longest sequential prefix.
;; Example 1:
;; Input: nums = [1,2,3,2,5]
;; Output: 6
;; Explanation: The longest sequential prefix of nums is [1,2,3] with a sum of 6. 6 is not in the array, therefore 6 is the smallest missing integer greater than or equal to the sum of the longest sequential prefix.
;; Example 2:
;; Input: nums = [3,4,5,1,12,14,13]
;; Output: 15
;; Explanation: The longest sequential prefix of nums is [3,4,5] with a sum of 12. 12, 13, and 14 belong to the array while 15 does not. Therefore 15 is the smallest missing integer greater than or equal to the sum of the longest sequential prefix.
;; Constraints:
;; 1 <= nums.length <= 50
;; 1 <= nums[i] <= 50
(define (missingInteger nums)
  ;; Helper function to find the longest sequential prefix
  (define (longest-sequential-prefix lst)
    (define (helper lst acc)
      (match lst
        [(list) acc]
        [(list x) (+ x acc)]
        [(list x y more ...)
         (if (= y (+ x 1))
             (helper (cons y more) (+ x acc))
             (+ x acc))]))
    (helper lst 0))

  ;; Helper function to find the smallest missing integer
  (define (smallest-missing x nums)
    (if (member x nums)
        (smallest-missing (+ x 1) nums)
        x))

  ;; Calculate the sum of the longest sequential prefix
  (define sum-prefix (longest-sequential-prefix nums))

  ;; Find the smallest missing integer greater than or equal to sum_prefix
  (smallest-missing sum-prefix nums))

;; Example usage
(missingInteger '(1 2 3 2 5)) ;; Output: 6
(missingInteger '(3 4 5 1 12 14 13)) ;; Output: 15

(require rackunit)


(define (test-humaneval) 

  (let (( candidate missingInteger))
    (check-within (candidate (list 1 2 3 2 5)) 6 0.001)
    (check-within (candidate (list 3 4 5 1 12 14 13)) 15 0.001)
    (check-within (candidate (list 29 30 31 32 33 34 35 36 37)) 297 0.001)
    (check-within (candidate (list 19 20 21 22)) 82 0.001)
    (check-within (candidate (list 18 19 20 21 22 23 24 25 26 27 28 9)) 253 0.001)
    (check-within (candidate (list 4 5 6 7 8 8 9 4 3 2 7)) 30 0.001)
    (check-within (candidate (list 38)) 39 0.001)
    (check-within (candidate (list 1)) 2 0.001)
    (check-within (candidate (list 11 12 13)) 36 0.001)
    (check-within (candidate (list 47 48 49 5 3)) 144 0.001)
    (check-within (candidate (list 23 24 25 4 5 1)) 72 0.001)
    (check-within (candidate (list 8 9 10 10 7 8)) 27 0.001)
    (check-within (candidate (list 31 32 33 34 10 8 7 9 7 9 9 5 10 1)) 130 0.001)
    (check-within (candidate (list 17 18 19 20 21 22 3 7 10 10)) 117 0.001)
    (check-within (candidate (list 6 7 8 9 10 8 6 7 4 1)) 40 0.001)
    (check-within (candidate (list 46 8 2 4 1 4 10 2 4 10 2 5 7 3 1)) 47 0.001)
    (check-within (candidate (list 37 1 2 9 5 8 5 2 9 4)) 38 0.001)
    (check-within (candidate (list 31 32 33 34 1)) 130 0.001)
    (check-within (candidate (list 45 46 47 48 49 10 8 1 7 4 10 10 6 6 2)) 235 0.001)
    (check-within (candidate (list 13 10 7 5 7 10 6 10 2)) 14 0.001)
    (check-within (candidate (list 32 33 34 35 36 37 38 39 40 41 42 43 44 8 6)) 494 0.001)
    (check-within (candidate (list 24 8 9)) 25 0.001)
    (check-within (candidate (list 47 48 49 9 3 8 1 9 2 5 4 5 9)) 144 0.001)
    (check-within (candidate (list 4 5 6 7 8 9 4 7 10 7 2)) 39 0.001)
    (check-within (candidate (list 28 29)) 57 0.001)
    (check-within (candidate (list 40 41 42 3 8 2 7 1 4)) 123 0.001)
    (check-within (candidate (list 17 18 19 20 21 22 23 24 25 26 27 9 2 5)) 242 0.001)
    (check-within (candidate (list 43 44)) 87 0.001)
    (check-within (candidate (list 19 20 5 3 10)) 39 0.001)
    (check-within (candidate (list 5)) 6 0.001)
    (check-within (candidate (list 14 15)) 29 0.001)
    (check-within (candidate (list 47 48 49)) 144 0.001)
    (check-within (candidate (list 10)) 11 0.001)
    (check-within (candidate (list 39)) 40 0.001)
    (check-within (candidate (list 11 12 13 14 15 7 5 2 10 5 6)) 65 0.001)
    (check-within (candidate (list 3 4 5 7 9 8 1 3 4 9)) 12 0.001)
    (check-within (candidate (list 29 30 31 32 33 34 35 36 37 38 39 40 41)) 455 0.001)
    (check-within (candidate (list 24 25 26 27 28 29 30 31 32 33 34 35 6 4 1)) 354 0.001)
    (check-within (candidate (list 7 8 9 10 11 12 13 14 15)) 99 0.001)
    (check-within (candidate (list 39 40 41 42 43 44 45 8 10 4)) 294 0.001)
    (check-within (candidate (list 36 37 6 8)) 73 0.001)
    (check-within (candidate (list 27 28 29 30)) 114 0.001)
    (check-within (candidate (list 34 35 5 7)) 69 0.001)
    (check-within (candidate (list 9 8 6 1)) 10 0.001)
    (check-within (candidate (list 36 37 38 39 8 10 7)) 150 0.001)
    (check-within (candidate (list 28 29 6)) 57 0.001)
    (check-within (candidate (list 14 15 16 17 18 19 20 10 9 10 9 7 3 6)) 119 0.001)
    (check-within (candidate (list 27 28 29 5)) 84 0.001)
    (check-within (candidate (list 42 43 44 45 46 47 48)) 315 0.001)
    (check-within (candidate (list 2 3 4 5 6 7 8 9 10 11 1 10 5 6)) 65 0.001)
    (check-within (candidate (list 32 33 34 35 36 37 5 8 5 3 4 2 10 3 7)) 207 0.001)
    (check-within (candidate (list 24 25 26 27 28 29 30 31 32 33 1 3 9)) 285 0.001)
    (check-within (candidate (list 48 49)) 97 0.001)
    (check-within (candidate (list 46 47 6 7 1)) 93 0.001)
    (check-within (candidate (list 32 33 34 35 36 37 38 39 40)) 324 0.001)
    (check-within (candidate (list 40 41 42 43 44 45 6)) 255 0.001)
    (check-within (candidate (list 5 6 7 8 9 10 11 12 13 14 15 16 17 18 9)) 161 0.001)
    (check-within (candidate (list 39 40 41 3 4 7 10 6 2 10 1 9)) 120 0.001)
    (check-within (candidate (list 17 18)) 35 0.001)
    (check-within (candidate (list 41 42 43 44 45 46 5 6)) 261 0.001)
    (check-within (candidate (list 6)) 7 0.001)
    (check-within (candidate (list 46 47 48 49 50 7)) 240 0.001)
    (check-within (candidate (list 17 18 19 20 21 22 23 24 25 26 4 7 5 4 4)) 215 0.001)
    (check-within (candidate (list 40 41 42 43 44 45 46 4 6)) 301 0.001)
    (check-within (candidate (list 13 4 2 2 3 4 1 8 3 7 7 7 1 6 3)) 14 0.001)
    (check-within (candidate (list 4 5 6 7 8 9 10 11 12 13 14 15 16 6 8)) 130 0.001)
    (check-within (candidate (list 12 10)) 13 0.001)
    (check-within (candidate (list 17 18 19 20 21 5 3 7 10 5 3 7 3 5 3)) 95 0.001)
    (check-within (candidate (list 38 39 40 41 42 43 44 45 5 7 9 9 4 1)) 332 0.001)
    (check-within (candidate (list 32 33 34 35)) 134 0.001)
    (check-within (candidate (list 33 34 7 3 4 4)) 67 0.001)
    (check-within (candidate (list 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47)) 600 0.001)
    (check-within (candidate (list 14 9 6 9 7 9 10 4 9 9 4 4)) 15 0.001)
    (check-within (candidate (list 18 19 20 21 22 23 24 25 26 6 8 2 1)) 198 0.001)
    (check-within (candidate (list 19 20 21 7 9)) 60 0.001)
    (check-within (candidate (list 19 20 21 10 1 8 2 1)) 60 0.001)
    (check-within (candidate (list 1 2 3 9 2 10 8 3 10 2)) 6 0.001)
    (check-within (candidate (list 48 10)) 49 0.001)
    (check-within (candidate (list 20 21 22 23 24 25 5)) 135 0.001)
    (check-within (candidate (list 40 41 42 43 3 4 10 3 7 8 9 1 5)) 166 0.001)
    (check-within (candidate (list 21 22 23 24 25 26 27 8)) 168 0.001)
    (check-within (candidate (list 2 3 4 5 6 4)) 20 0.001)
    (check-within (candidate (list 9 10 11 12 13 14 15 16 17 4)) 117 0.001)
    (check-within (candidate (list 25 26 27 28 29 6 8)) 135 0.001)
    (check-within (candidate (list 16 17 18 19 20 21 22 23 24 25 6)) 205 0.001)
    (check-within (candidate (list 7 8 9 10 11 12)) 57 0.001)
    (check-within (candidate (list 32 9 2 6 4 1 4 3 5)) 33 0.001)
    (check-within (candidate (list 1 4 3)) 2 0.001)
    (check-within (candidate (list 34 35 36 37 38 39 1 9 3 3 10 7 1)) 219 0.001)
    (check-within (candidate (list 37 7 6 4 3 1 10 8 7 2 6)) 38 0.001)
    (check-within (candidate (list 32)) 33 0.001)
    (check-within (candidate (list 25 26 27 4)) 78 0.001)
    (check-within (candidate (list 31 32 33 8 5 3 7 2)) 96 0.001)
    (check-within (candidate (list 38 39 40 41 42 43 44 45 1)) 332 0.001)
    (check-within (candidate (list 35 36 3 10)) 71 0.001)
    (check-within (candidate (list 31 32 33 34 35 7 6 1 9)) 165 0.001)
    (check-within (candidate (list 47 48 49 2 2)) 144 0.001)
    (check-within (candidate (list 3 4 5 6 7 8 9 10 11 10 1)) 63 0.001)
    (check-within (candidate (list 50)) 51 0.001)
    (check-within (candidate (list 14 15 16 17 7 10 3 10)) 62 0.001)
))

(test-humaneval)