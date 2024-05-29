#lang racket

;; You are given a 0-indexed integer array nums and an integer x.
;; Find the minimum absolute difference between two elements in the array that are at least x indices apart.
;; In other words, find two indices i and j such that abs(i - j) >= x and abs(nums[i] - nums[j]) is minimized.
;; Return an integer denoting the minimum absolute difference between two elements that are at least x indices apart.
;; Example 1:
;; Input: nums = [4,3,2,4], x = 2
;; Output: 0
;; Explanation: We can select nums[0] = 4 and nums[3] = 4.
;; They are at least 2 indices apart, and their absolute difference is the minimum, 0.
;; It can be shown that 0 is the optimal answer.
;; Example 2:
;; Input: nums = [5,3,2,10,15], x = 1
;; Output: 1
;; Explanation: We can select nums[1] = 3 and nums[2] = 2.
;; They are at least 1 index apart, and their absolute difference is the minimum, 1.
;; It can be shown that 1 is the optimal answer.
;; Example 3:
;; Input: nums = [1,2,3,4], x = 3
;; Output: 3
;; Explanation: We can select nums[0] = 1 and nums[3] = 4.
;; They are at least 3 indices apart, and their absolute difference is the minimum, 3.
;; It can be shown that 3 is the optimal answer.
;; Constraints:
;;  * 1 <= nums.length <= 105
;;  * 1 <= nums[i] <= 109
;;  * 0 <= x < nums.length
(define (minAbsoluteDifference nums x)
  (define n (length nums))

  ;; Helper function to find the minimum absolute difference for a given index i
  (define (find-min-diff i min-diff)
    (for/fold ([min-diff min-diff]) ([j (in-range (+ i x) n)])
      (define diff (abs (- (list-ref nums i) (list-ref nums j))))
      (min min-diff diff)))

  ;; Iterate over each index and update the minimum difference
  (for/fold ([min-diff +inf.0]) ([i (in-range (- n x))])
    (find-min-diff i min-diff)))

;; Example usage:
(minAbsoluteDifference '(4 3 2 4) 2)  ;; Output: 0
(minAbsoluteDifference '(5 3 2 10 15) 1)  ;; Output: 1
(minAbsoluteDifference '(1 2 3 4) 3)  ;; Output: 3

(require rackunit)


(define (test-humaneval) 

  (let (( candidate minAbsoluteDifference))
    (check-within (candidate (list 4 3 2 4) 2) 0 0.001)
    (check-within (candidate (list 5 3 2 10 15) 1) 1 0.001)
    (check-within (candidate (list 1 2 3 4) 3) 3 0.001)
    (check-within (candidate (list 1 67) 1) 66 0.001)
    (check-within (candidate (list 7 398) 1) 391 0.001)
    (check-within (candidate (list 12 141) 1) 129 0.001)
    (check-within (candidate (list 21 75) 1) 54 0.001)
    (check-within (candidate (list 22 147) 1) 125 0.001)
    (check-within (candidate (list 25 197) 1) 172 0.001)
    (check-within (candidate (list 27 275) 1) 248 0.001)
    (check-within (candidate (list 37 192) 1) 155 0.001)
    (check-within (candidate (list 41 163) 1) 122 0.001)
    (check-within (candidate (list 45 49) 1) 4 0.001)
    (check-within (candidate (list 48 195) 1) 147 0.001)
    (check-within (candidate (list 68 68) 1) 0 0.001)
    (check-within (candidate (list 71 4) 1) 67 0.001)
    (check-within (candidate (list 72 169) 1) 97 0.001)
    (check-within (candidate (list 74 62) 1) 12 0.001)
    (check-within (candidate (list 75 1) 1) 74 0.001)
    (check-within (candidate (list 76 49) 1) 27 0.001)
    (check-within (candidate (list 88 72) 1) 16 0.001)
    (check-within (candidate (list 99 370) 1) 271 0.001)
    (check-within (candidate (list 103 39) 1) 64 0.001)
    (check-within (candidate (list 109 99) 1) 10 0.001)
    (check-within (candidate (list 111 161) 1) 50 0.001)
    (check-within (candidate (list 113 117) 1) 4 0.001)
    (check-within (candidate (list 119 184) 1) 65 0.001)
    (check-within (candidate (list 122 118) 1) 4 0.001)
    (check-within (candidate (list 123 13) 1) 110 0.001)
    (check-within (candidate (list 123 162) 1) 39 0.001)
    (check-within (candidate (list 126 69) 1) 57 0.001)
    (check-within (candidate (list 127 18) 1) 109 0.001)
    (check-within (candidate (list 127 346) 1) 219 0.001)
    (check-within (candidate (list 132 110) 1) 22 0.001)
    (check-within (candidate (list 134 23) 1) 111 0.001)
    (check-within (candidate (list 136 150) 1) 14 0.001)
    (check-within (candidate (list 139 215) 1) 76 0.001)
    (check-within (candidate (list 153 3) 1) 150 0.001)
    (check-within (candidate (list 156 67) 1) 89 0.001)
    (check-within (candidate (list 160 168) 1) 8 0.001)
    (check-within (candidate (list 161 93) 1) 68 0.001)
    (check-within (candidate (list 164 81) 1) 83 0.001)
    (check-within (candidate (list 167 83) 1) 84 0.001)
    (check-within (candidate (list 174 58) 1) 116 0.001)
    (check-within (candidate (list 174 102) 1) 72 0.001)
    (check-within (candidate (list 175 137) 1) 38 0.001)
    (check-within (candidate (list 176 99) 1) 77 0.001)
    (check-within (candidate (list 178 179) 1) 1 0.001)
    (check-within (candidate (list 228 359) 1) 131 0.001)
    (check-within (candidate (list 243 280) 1) 37 0.001)
    (check-within (candidate (list 283 62) 1) 221 0.001)
    (check-within (candidate (list 288 149) 1) 139 0.001)
    (check-within (candidate (list 293 278) 1) 15 0.001)
    (check-within (candidate (list 327 425) 1) 98 0.001)
    (check-within (candidate (list 337 187) 1) 150 0.001)
    (check-within (candidate (list 346 160) 1) 186 0.001)
    (check-within (candidate (list 347 369) 1) 22 0.001)
    (check-within (candidate (list 355 199) 1) 156 0.001)
    (check-within (candidate (list 413 311) 1) 102 0.001)
    (check-within (candidate (list 417 320) 1) 97 0.001)
    (check-within (candidate (list 418 131) 1) 287 0.001)
    (check-within (candidate (list 3274 71) 1) 3203 0.001)
    (check-within (candidate (list 5 14 81) 2) 76 0.001)
    (check-within (candidate (list 9 25 15) 2) 6 0.001)
    (check-within (candidate (list 9 113 136) 1) 23 0.001)
    (check-within (candidate (list 13 19 12) 1) 1 0.001)
    (check-within (candidate (list 13 94 59) 2) 46 0.001)
    (check-within (candidate (list 14 111 16) 1) 2 0.001)
    (check-within (candidate (list 17 173 69) 1) 52 0.001)
    (check-within (candidate (list 24 39 28) 2) 4 0.001)
    (check-within (candidate (list 32 129 93) 1) 36 0.001)
    (check-within (candidate (list 33 18 131) 1) 15 0.001)
    (check-within (candidate (list 36 19 27) 2) 9 0.001)
    (check-within (candidate (list 40 18 17) 1) 1 0.001)
    (check-within (candidate (list 43 49 20) 2) 23 0.001)
    (check-within (candidate (list 44 186 163) 2) 119 0.001)
    (check-within (candidate (list 56 23 158) 1) 33 0.001)
    (check-within (candidate (list 62 37 182) 2) 120 0.001)
    (check-within (candidate (list 63 116 12) 2) 51 0.001)
    (check-within (candidate (list 66 345 278) 2) 212 0.001)
    (check-within (candidate (list 67 81 165) 2) 98 0.001)
    (check-within (candidate (list 70 184 70) 2) 0 0.001)
    (check-within (candidate (list 73 106 172) 1) 33 0.001)
    (check-within (candidate (list 74 199 57) 1) 17 0.001)
    (check-within (candidate (list 83 14 14) 1) 0 0.001)
    (check-within (candidate (list 86 1 129) 2) 43 0.001)
    (check-within (candidate (list 87 194 107) 1) 20 0.001)
    (check-within (candidate (list 88 75 122) 2) 34 0.001)
    (check-within (candidate (list 93 96 28) 1) 3 0.001)
    (check-within (candidate (list 95 86 132) 2) 37 0.001)
    (check-within (candidate (list 96 41 24) 1) 17 0.001)
    (check-within (candidate (list 116 6 3) 1) 3 0.001)
    (check-within (candidate (list 120 102 184) 1) 18 0.001)
    (check-within (candidate (list 123 113 20) 1) 10 0.001)
    (check-within (candidate (list 125 14 141) 1) 16 0.001)
    (check-within (candidate (list 126 2 180) 1) 54 0.001)
    (check-within (candidate (list 136 24 114) 1) 22 0.001)
    (check-within (candidate (list 136 177 98) 2) 38 0.001)
    (check-within (candidate (list 136 177 123) 1) 13 0.001)
    (check-within (candidate (list 136 178 18) 2) 118 0.001)
))

(test-humaneval)