#lang racket

;; You are given a 0-indexed integer array nums of even length and there is also an empty array arr. Alice and Bob decided to play a game where in every round Alice and Bob will do one move. The rules of the game are as follows:
;; Every round, first Alice will remove the minimum element from nums, and then Bob does the same.
;; Now, first Bob will append the removed element in the array arr, and then Alice does the same.
;; The game continues until nums becomes empty.
;; Return the resulting array arr.
;; Example 1:
;; Input: nums = [5,4,2,3]
;; Output: [3,2,5,4]
;; Explanation: In round one, first Alice removes 2 and then Bob removes 3. Then in arr firstly Bob appends 3 and then Alice appends 2. So arr = [3,2].
;; At the begining of round two, nums = [5,4]. Now, first Alice removes 4 and then Bob removes 5. Then both append in arr which becomes [3,2,5,4].
;; Example 2:
;; Input: nums = [2,5]
;; Output: [5,2]
;; Explanation: In round one, first Alice removes 2 and then Bob removes 5. Then in arr firstly Bob appends and then Alice appends. So arr = [5,2].
;; Constraints:
;; 1 <= nums.length <= 100
;; 1 <= nums[i] <= 100
;; nums.length % 2 == 0
(define (numberGame nums)
  (define (game-loop nums acc)
    (match nums
      ['() (reverse acc)]
      [(list-rest alice-choice bob-choice remaining-nums)
       (game-loop (sort remaining-nums <) (cons alice-choice (cons bob-choice acc)))]))
  
  (game-loop (sort nums <) '()))

;; Example usage:
(numberGame '(5 4 2 3))  ; Output: '(3 2 5 4)
(numberGame '(2 5))      ; Output: '(5 2)

(require rackunit)


(define (test-humaneval) 

  (let (( candidate numberGame))
    (check-within (candidate (list 5 4 2 3)) (list 3 2 5 4) 0.001)
    (check-within (candidate (list 2 5)) (list 5 2) 0.001)
    (check-within (candidate (list 4 4 3 8)) (list 4 3 8 4) 0.001)
    (check-within (candidate (list 2 5 3 8)) (list 3 2 8 5) 0.001)
    (check-within (candidate (list 2 7 9 6 4 6)) (list 4 2 6 6 9 7) 0.001)
    (check-within (candidate (list 18 26 37 46 13 33 39 1 37 16)) (list 13 1 18 16 33 26 37 37 46 39) 0.001)
    (check-within (candidate (list 17 2 4 11 14 18)) (list 4 2 14 11 18 17) 0.001)
    (check-within (candidate (list 20 30 12 3 11 17 32 12)) (list 11 3 12 12 20 17 32 30) 0.001)
    (check-within (candidate (list 9 32 6 11 11 39 18 29 44 29)) (list 9 6 11 11 29 18 32 29 44 39) 0.001)
    (check-within (candidate (list 7 2 3 4)) (list 3 2 7 4) 0.001)
    (check-within (candidate (list 8 7 1 3)) (list 3 1 8 7) 0.001)
    (check-within (candidate (list 2 6 6 6)) (list 6 2 6 6) 0.001)
    (check-within (candidate (list 1 2)) (list 2 1) 0.001)
    (check-within (candidate (list 4 1 1 3)) (list 1 1 4 3) 0.001)
    (check-within (candidate (list 13 12 18 11 15 28 26 2)) (list 11 2 13 12 18 15 28 26) 0.001)
    (check-within (candidate (list 14 30 29 3 23 21 26 23)) (list 14 3 23 21 26 23 30 29) 0.001)
    (check-within (candidate (list 1 1)) (list 1 1) 0.001)
    (check-within (candidate (list 2 1)) (list 2 1) 0.001)
    (check-within (candidate (list 12 1 28 23 2 31 11 26)) (list 2 1 12 11 26 23 31 28) 0.001)
    (check-within (candidate (list 21 11 37 1 40 50 49 45 28 47)) (list 11 1 28 21 40 37 47 45 50 49) 0.001)
    (check-within (candidate (list 25 22 31 7 30 9 9 18)) (list 9 7 18 9 25 22 31 30) 0.001)
    (check-within (candidate (list 2 4 10 9 16 9)) (list 4 2 9 9 16 10) 0.001)
    (check-within (candidate (list 5 2 3 5)) (list 3 2 5 5) 0.001)
    (check-within (candidate (list 6 44 37 6 28 44 30 36 25 24)) (list 6 6 25 24 30 28 37 36 44 44) 0.001)
    (check-within (candidate (list 17 10 6 14 10 18)) (list 10 6 14 10 18 17) 0.001)
    (check-within (candidate (list 40 24 23 29 37 26 39 34 39 23)) (list 23 23 26 24 34 29 39 37 40 39) 0.001)
    (check-within (candidate (list 2 2)) (list 2 2) 0.001)
    (check-within (candidate (list 33 5 31 43 48 18 31 11 19 8)) (list 8 5 18 11 31 19 33 31 48 43) 0.001)
    (check-within (candidate (list 37 46 42 19 10 8 43 10 40 13)) (list 10 8 13 10 37 19 42 40 46 43) 0.001)
    (check-within (candidate (list 2 19 8 22 1 27 29 7)) (list 2 1 8 7 22 19 29 27) 0.001)
    (check-within (candidate (list 2 3 2 3)) (list 2 2 3 3) 0.001)
    (check-within (candidate (list 1 4 7 14 8 14)) (list 4 1 8 7 14 14) 0.001)
    (check-within (candidate (list 28 47 36 34 19 7 40 46 33 43)) (list 19 7 33 28 36 34 43 40 47 46) 0.001)
    (check-within (candidate (list 29 41 20 22 16 27 22 44 10 47)) (list 16 10 22 20 27 22 41 29 47 44) 0.001)
    (check-within (candidate (list 14 6 40 19 47 46 34 27 28 10)) (list 10 6 19 14 28 27 40 34 47 46) 0.001)
    (check-within (candidate (list 42 43 50 43 36 26 16 12 3 2)) (list 3 2 16 12 36 26 43 42 50 43) 0.001)
    (check-within (candidate (list 1 7 24 24 23 32 28 2)) (list 2 1 23 7 24 24 32 28) 0.001)
    (check-within (candidate (list 20 19 16 16 19 29 21 5)) (list 16 5 19 16 20 19 29 21) 0.001)
    (check-within (candidate (list 20 9 29 29 17 39 27 44 1 8)) (list 8 1 17 9 27 20 29 29 44 39) 0.001)
    (check-within (candidate (list 14 11 12 18 9 15)) (list 11 9 14 12 18 15) 0.001)
    (check-within (candidate (list 17 22 2 35 15 19 25 5 33 44)) (list 5 2 17 15 22 19 33 25 44 35) 0.001)
    (check-within (candidate (list 22 3 26 15 1 5 14 28)) (list 3 1 14 5 22 15 28 26) 0.001)
    (check-within (candidate (list 5 24 3 2 17 9 2 4)) (list 2 2 4 3 9 5 24 17) 0.001)
    (check-within (candidate (list 2 6 4 7)) (list 4 2 7 6) 0.001)
    (check-within (candidate (list 1 33 29 21 25 14 26 35 34 30)) (list 14 1 25 21 29 26 33 30 35 34) 0.001)
    (check-within (candidate (list 50 25 42 41 16 23 47 31 23 16)) (list 16 16 23 23 31 25 42 41 50 47) 0.001)
    (check-within (candidate (list 31 31 31 12 24 17 11 3 33 13)) (list 11 3 13 12 24 17 31 31 33 31) 0.001)
    (check-within (candidate (list 8 3 2 7)) (list 3 2 8 7) 0.001)
    (check-within (candidate (list 8 2 8 6)) (list 6 2 8 8) 0.001)
    (check-within (candidate (list 4 15 16 2 12 7)) (list 4 2 12 7 16 15) 0.001)
    (check-within (candidate (list 5 4 2 4)) (list 4 2 5 4) 0.001)
    (check-within (candidate (list 17 13 7 12 19 15 6 22)) (list 7 6 13 12 17 15 22 19) 0.001)
    (check-within (candidate (list 2 15 12 16 12 13)) (list 12 2 13 12 16 15) 0.001)
    (check-within (candidate (list 3 15 18 16 6 7)) (list 6 3 15 7 18 16) 0.001)
    (check-within (candidate (list 4 7 11 6 11 8)) (list 6 4 8 7 11 11) 0.001)
    (check-within (candidate (list 1 7 24 23 16 21 9 11)) (list 7 1 11 9 21 16 24 23) 0.001)
    (check-within (candidate (list 6 3 10 16 15 6)) (list 6 3 10 6 16 15) 0.001)
    (check-within (candidate (list 17 9 1 29 30 5 31 26)) (list 5 1 17 9 29 26 31 30) 0.001)
    (check-within (candidate (list 3 6 4 14 9 15)) (list 4 3 9 6 15 14) 0.001)
    (check-within (candidate (list 37 38 24 15 12 1 37 19 38 11)) (list 11 1 15 12 24 19 37 37 38 38) 0.001)
    (check-within (candidate (list 17 3 8 12 6 9)) (list 6 3 9 8 17 12) 0.001)
    (check-within (candidate (list 32 23 27 32 24 26 24 27)) (list 24 23 26 24 27 27 32 32) 0.001)
    (check-within (candidate (list 15 16 26 6 5 9 22 14)) (list 6 5 14 9 16 15 26 22) 0.001)
    (check-within (candidate (list 14 21 13 10 2 16 14 30)) (list 10 2 14 13 16 14 30 21) 0.001)
    (check-within (candidate (list 1 6 30 1 13 25 18 1)) (list 1 1 6 1 18 13 30 25) 0.001)
    (check-within (candidate (list 32 12 17 32 11 25 22 18 10 1)) (list 10 1 12 11 18 17 25 22 32 32) 0.001)
    (check-within (candidate (list 2 8 5 6)) (list 5 2 8 6) 0.001)
    (check-within (candidate (list 27 3 10 25 10 7 15 16)) (list 7 3 10 10 16 15 27 25) 0.001)
    (check-within (candidate (list 5 18 19 25 13 21 16 7)) (list 7 5 16 13 19 18 25 21) 0.001)
    (check-within (candidate (list 8 6 6 8)) (list 6 6 8 8) 0.001)
    (check-within (candidate (list 23 15 39 9 19 10 6 9 33 28)) (list 9 6 10 9 19 15 28 23 39 33) 0.001)
    (check-within (candidate (list 16 42 47 16 31 39 8 26 50 33)) (list 16 8 26 16 33 31 42 39 50 47) 0.001)
    (check-within (candidate (list 4 31 9 2 4 28 28 12)) (list 4 2 9 4 28 12 31 28) 0.001)
    (check-within (candidate (list 9 5 8 11 4 7)) (list 5 4 8 7 11 9) 0.001)
    (check-within (candidate (list 44 2 23 3 7 2 36 33 7 21)) (list 2 2 7 3 21 7 33 23 44 36) 0.001)
    (check-within (candidate (list 19 9 4 7 29 22 50 28 2 40)) (list 4 2 9 7 22 19 29 28 50 40) 0.001)
    (check-within (candidate (list 4 5 5 5)) (list 5 4 5 5) 0.001)
    (check-within (candidate (list 42 6 44 47 11 6 30 38 41 43)) (list 6 6 30 11 41 38 43 42 47 44) 0.001)
    (check-within (candidate (list 28 4 47 1 7 35 10 10 5 8)) (list 4 1 7 5 10 8 28 10 47 35) 0.001)
    (check-within (candidate (list 12 20 14 46 22 1 42 50 47 47)) (list 12 1 20 14 42 22 47 46 50 47) 0.001)
    (check-within (candidate (list 37 13 1 38 28 46 18 22 12 7)) (list 7 1 13 12 22 18 37 28 46 38) 0.001)
    (check-within (candidate (list 36 41 5 33 5 30 33 31 6 45)) (list 5 5 30 6 33 31 36 33 45 41) 0.001)
    (check-within (candidate (list 13 50 42 24 47 41 8 26 34 3)) (list 8 3 24 13 34 26 42 41 50 47) 0.001)
    (check-within (candidate (list 24 39 26 46 47 9 33 6 33 40)) (list 9 6 26 24 33 33 40 39 47 46) 0.001)
    (check-within (candidate (list 14 13 17 14 12 15 6 32)) (list 12 6 14 13 15 14 32 17) 0.001)
    (check-within (candidate (list 46 50 35 11 14 44 17 45 23 34)) (list 14 11 23 17 35 34 45 44 50 46) 0.001)
    (check-within (candidate (list 8 27 19 7 10 12 14 50 45 14)) (list 8 7 12 10 14 14 27 19 50 45) 0.001)
    (check-within (candidate (list 9 8 5 7 10 9)) (list 7 5 9 8 10 9) 0.001)
    (check-within (candidate (list 5 5 3 7)) (list 5 3 7 5) 0.001)
    (check-within (candidate (list 26 21 7 13 3 10 9 15)) (list 7 3 10 9 15 13 26 21) 0.001)
    (check-within (candidate (list 8 5 8 3)) (list 5 3 8 8) 0.001)
    (check-within (candidate (list 18 1 16 18 13 3)) (list 3 1 16 13 18 18) 0.001)
    (check-within (candidate (list 25 2 17 26 17 20 19 24)) (list 17 2 19 17 24 20 26 25) 0.001)
    (check-within (candidate (list 24 1 18 25 29 17 9 3)) (list 3 1 17 9 24 18 29 25) 0.001)
    (check-within (candidate (list 23 17 18 18 18 30 8 19)) (list 17 8 18 18 19 18 30 23) 0.001)
    (check-within (candidate (list 12 13 13 18 5 16)) (list 12 5 13 13 18 16) 0.001)
    (check-within (candidate (list 19 4 11 7 24 12 24 14)) (list 7 4 12 11 19 14 24 24) 0.001)
    (check-within (candidate (list 28 11 11 29 18 2 6 32)) (list 6 2 11 11 28 18 32 29) 0.001)
    (check-within (candidate (list 12 17 3 31 15 18 18 2)) (list 3 2 15 12 18 17 31 18) 0.001)
    (check-within (candidate (list 24 6 21 30 29 8 23 18)) (list 8 6 21 18 24 23 30 29) 0.001)
))

(test-humaneval)