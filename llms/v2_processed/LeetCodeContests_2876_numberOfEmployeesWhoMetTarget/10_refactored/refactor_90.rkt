#lang racket

;; There are n employees in a company, numbered from 0 to n - 1. Each employee i has worked for hours[i] hours in the company.
;; The company requires each employee to work for at least target hours.
;; You are given a 0-indexed array of non-negative integers hours of length n and a non-negative integer target.
;; Return the integer denoting the number of employees who worked at least target hours.
;; Example 1:
;; Input: hours = [0,1,2,3,4], target = 2
;; Output: 3
;; Explanation: The company wants each employee to work for at least 2 hours.
;; - Employee 0 worked for 0 hours and didn't meet the target.
;; - Employee 1 worked for 1 hours and didn't meet the target.
;; - Employee 2 worked for 2 hours and met the target.
;; - Employee 3 worked for 3 hours and met the target.
;; - Employee 4 worked for 4 hours and met the target.
;; There are 3 employees who met the target.
;; Example 2:
;; Input: hours = [5,1,4,2,2], target = 6
;; Output: 0
;; Explanation: The company wants each employee to work for at least 6 hours.
;; There are 0 employees who met the target.
;; Constraints:
;;  * 1 <= n == hours.length <= 50
;;  * 0 <= hours[i], target <= 105
(define (numberOfEmployeesWhoMetTarget hours target)
  ;; Calculate the number of employees who have met or exceeded the target working hours.
  (for/sum ([hour (in-list hours)]
            #:when (>= hour target))
    1))

;; Example use cases
(displayln (numberOfEmployeesWhoMetTarget '(0 1 2 3 4) 2))  ; Output: 3
(displayln (numberOfEmployeesWhoMetTarget '(5 1 4 2 2) 6))  ; Output: 0

(require rackunit)


(define (test-humaneval) 

  (let (( candidate numberOfEmployeesWhoMetTarget))
    (check-within (candidate (list 0 1 2 3 4) 2) 3 0.001)
    (check-within (candidate (list 5 1 4 2 2) 6) 0 0.001)
    (check-within (candidate (list 98) 5) 1 0.001)
    (check-within (candidate (list 19) 13) 1 0.001)
    (check-within (candidate (list 70) 13) 1 0.001)
    (check-within (candidate (list 26) 14) 1 0.001)
    (check-within (candidate (list 2) 16) 0 0.001)
    (check-within (candidate (list 77) 19) 1 0.001)
    (check-within (candidate (list 6) 21) 0 0.001)
    (check-within (candidate (list 27) 21) 1 0.001)
    (check-within (candidate (list 36) 22) 1 0.001)
    (check-within (candidate (list 42) 25) 1 0.001)
    (check-within (candidate (list 70) 27) 1 0.001)
    (check-within (candidate (list 2) 28) 0 0.001)
    (check-within (candidate (list 14) 31) 0 0.001)
    (check-within (candidate (list 45) 34) 1 0.001)
    (check-within (candidate (list 44) 35) 1 0.001)
    (check-within (candidate (list 11) 39) 0 0.001)
    (check-within (candidate (list 71) 39) 1 0.001)
    (check-within (candidate (list 91) 45) 1 0.001)
    (check-within (candidate (list 81) 51) 1 0.001)
    (check-within (candidate (list 15) 52) 0 0.001)
    (check-within (candidate (list 90) 59) 1 0.001)
    (check-within (candidate (list 40) 64) 0 0.001)
    (check-within (candidate (list 12) 69) 0 0.001)
    (check-within (candidate (list 83) 70) 1 0.001)
    (check-within (candidate (list 38) 74) 0 0.001)
    (check-within (candidate (list 18) 78) 0 0.001)
    (check-within (candidate (list 60) 83) 0 0.001)
    (check-within (candidate (list 50) 87) 0 0.001)
    (check-within (candidate (list 75) 92) 0 0.001)
    (check-within (candidate (list 91) 96) 0 0.001)
    (check-within (candidate (list 11) 97) 0 0.001)
    (check-within (candidate (list 48 28) 2) 2 0.001)
    (check-within (candidate (list 38 46) 3) 2 0.001)
    (check-within (candidate (list 30 79) 6) 2 0.001)
    (check-within (candidate (list 45 78) 6) 2 0.001)
    (check-within (candidate (list 20 69) 10) 2 0.001)
    (check-within (candidate (list 82 67) 11) 2 0.001)
    (check-within (candidate (list 29 75) 12) 2 0.001)
    (check-within (candidate (list 97 37) 17) 2 0.001)
    (check-within (candidate (list 42 100) 20) 2 0.001)
    (check-within (candidate (list 11 58) 21) 1 0.001)
    (check-within (candidate (list 12 46) 21) 1 0.001)
    (check-within (candidate (list 70 84) 37) 2 0.001)
    (check-within (candidate (list 7 100) 38) 1 0.001)
    (check-within (candidate (list 47 94) 40) 2 0.001)
    (check-within (candidate (list 18 34) 50) 0 0.001)
    (check-within (candidate (list 47 79) 55) 1 0.001)
    (check-within (candidate (list 74 99) 56) 2 0.001)
    (check-within (candidate (list 53 81) 67) 1 0.001)
    (check-within (candidate (list 36 61) 68) 0 0.001)
    (check-within (candidate (list 48 98) 71) 1 0.001)
    (check-within (candidate (list 71 94) 72) 1 0.001)
    (check-within (candidate (list 60 99) 73) 1 0.001)
    (check-within (candidate (list 12 12) 74) 0 0.001)
    (check-within (candidate (list 100 87) 75) 2 0.001)
    (check-within (candidate (list 12 56) 77) 0 0.001)
    (check-within (candidate (list 15 36) 86) 0 0.001)
    (check-within (candidate (list 53 45) 86) 0 0.001)
    (check-within (candidate (list 4 77) 89) 0 0.001)
    (check-within (candidate (list 23 29) 93) 0 0.001)
    (check-within (candidate (list 76 62 96) 5) 3 0.001)
    (check-within (candidate (list 82 67 33) 5) 3 0.001)
    (check-within (candidate (list 28 96 39) 10) 3 0.001)
    (check-within (candidate (list 42 93 58) 13) 3 0.001)
    (check-within (candidate (list 53 22 48) 13) 3 0.001)
    (check-within (candidate (list 68 81 61) 13) 3 0.001)
    (check-within (candidate (list 68 32 33) 22) 3 0.001)
    (check-within (candidate (list 59 65 70) 26) 3 0.001)
    (check-within (candidate (list 15 43 21) 29) 1 0.001)
    (check-within (candidate (list 40 80 75) 33) 3 0.001)
    (check-within (candidate (list 64 11 73) 34) 2 0.001)
    (check-within (candidate (list 1 74 34) 44) 1 0.001)
    (check-within (candidate (list 96 79 91) 44) 3 0.001)
    (check-within (candidate (list 59 9 9) 48) 1 0.001)
    (check-within (candidate (list 79 48 62) 53) 2 0.001)
    (check-within (candidate (list 58 83 2) 54) 2 0.001)
    (check-within (candidate (list 51 40 12) 57) 0 0.001)
    (check-within (candidate (list 54 2 80) 60) 1 0.001)
    (check-within (candidate (list 92 45 91) 65) 2 0.001)
    (check-within (candidate (list 93 23 46) 67) 1 0.001)
    (check-within (candidate (list 17 60 1) 70) 0 0.001)
    (check-within (candidate (list 9 63 77) 73) 1 0.001)
    (check-within (candidate (list 44 86 37) 73) 1 0.001)
    (check-within (candidate (list 75 37 68) 73) 1 0.001)
    (check-within (candidate (list 33 26 77) 78) 0 0.001)
    (check-within (candidate (list 11 88 27) 79) 1 0.001)
    (check-within (candidate (list 12 48 44) 80) 0 0.001)
    (check-within (candidate (list 19 88 13) 82) 1 0.001)
    (check-within (candidate (list 61 56 67) 82) 0 0.001)
    (check-within (candidate (list 46 24 38) 84) 0 0.001)
    (check-within (candidate (list 93 51 83) 85) 1 0.001)
    (check-within (candidate (list 58 14 83) 87) 0 0.001)
    (check-within (candidate (list 52 33 56) 89) 0 0.001)
    (check-within (candidate (list 34 73 46) 91) 0 0.001)
    (check-within (candidate (list 26 59 55) 94) 0 0.001)
    (check-within (candidate (list 41 89 34) 100) 0 0.001)
    (check-within (candidate (list 9 26 77 55) 0) 4 0.001)
    (check-within (candidate (list 24 79 63 37) 1) 4 0.001)
))

(test-humaneval)