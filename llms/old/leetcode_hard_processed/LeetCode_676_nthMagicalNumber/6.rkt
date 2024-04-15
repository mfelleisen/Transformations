#lang racket

;; Function to calculate the Greatest Common Divisor (GCD) using Euclid's algorithm
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (modulo a b))))

;; Main function to find the nth magical number divisible by a or b
;; A positive integer is magical if it is divisible by either a or b.
;; Given the three integers n, a, and b, return the nth magical number. Since the answer may be very large, return it modulo 109 + 7.
;; Example 1:
;; Input: n = 1, a = 2, b = 3
;; Output: 2
;; Example 2:
;; Input: n = 4, a = 2, b = 3
;; Output: 6
;; Constraints:
;; 1 <= n <= 109
;; 2 <= a, b <= 4 * 104
(define (nthMagicalNumber n a b)
  (let* (;; Define the modulo value as per problem statement
         (mod 1000000007)
         ;; Calculate the Least Common Multiple (LCM) using the formula lcm(a,b) = a*b / gcd(a,b)
         (lcm (/ (* a b) (gcd a b)))
         ;; Initialize search bounds
         (left 1)
         (right 100000000000000))
    ;; Binary search to find the nth magical number
    (let loop ((left left) (right right))
      (if (< left right)
          (let ((mid (+ left (quotient (- right left) 2))))
            (if (< (+ (quotient mid a) (quotient mid b) (- (quotient mid lcm))) n)
                (loop (+ mid 1) right) ; If mid is too small, search in the right half
                (loop left mid))) ; Else, search in the left half
          (modulo left mod)))) ; Return the left bound modulo mod when the condition is met
)

;; Example usage
;; (nthMagicalNumber 1 2 3) ; Returns 2
;; (nthMagicalNumber 4 2 3) ; Returns 6
(require rackunit)

(define (test-humaneval) 

  (let (( candidate nthMagicalNumber))
    (check-within (candidate 1 1 2) 1 0.001)
    (check-within (candidate 15 15 15) 225 0.001)
    (check-within (candidate 8 4 2) 16 0.001)
    (check-within (candidate 1 1 8) 1 0.001)
    (check-within (candidate 9 3 3) 27 0.001)
    (check-within (candidate 7 7 7) 49 0.001)
    (check-within (candidate 1 2 3) 2 0.001)
    (check-within (candidate 11 11 11) 121 0.001)
    (check-within (candidate 1 1 4) 1 0.001)
    (check-within (candidate 8 8 8) 64 0.001)
    (check-within (candidate 10 10000 10000) 100000 0.001)
    (check-within (candidate 10 1 1) 10 0.001)
    (check-within (candidate 1 1 9) 1 0.001)
    (check-within (candidate 1 1 7) 1 0.001)
    (check-within (candidate 1 3 2) 2 0.001)
    (check-within (candidate 1 100 10000) 100 0.001)
    (check-within (candidate 1 1 10000) 1 0.001)
    (check-within (candidate 1 1 5) 1 0.001)
    (check-within (candidate 4 4 4) 16 0.001)
    (check-within (candidate 4 2 3) 6 0.001)
    (check-within (candidate 2 2 1) 2 0.001)
    (check-within (candidate 6 2 4) 12 0.001)
    (check-within (candidate 1 10000 10000) 10000 0.001)
    (check-within (candidate 2 2 2) 4 0.001)
    (check-within (candidate 10 4 2) 20 0.001)
    (check-within (candidate 5 5 5) 25 0.001)
    (check-within (candidate 12 12 12) 144 0.001)
    (check-within (candidate 6 6 6) 36 0.001)
    (check-within (candidate 10000 10000 10000) 100000000 0.001)
    (check-within (candidate 14 14 14) 196 0.001)
    (check-within (candidate 10 10 10) 100 0.001)
    (check-within (candidate 9 9 9) 81 0.001)
    (check-within (candidate 1 1 1) 1 0.001)
    (check-within (candidate 100000 10000 10000) 1000000000 0.001)
    (check-within (candidate 1000 2 1) 1000 0.001)
    (check-within (candidate 3 3 3) 9 0.001)
    (check-within (candidate 1 2 1) 1 0.001)
    (check-within (candidate 13 13 13) 169 0.001)
    (check-within (candidate 10 2 2) 20 0.001)
    (check-within (candidate 1 3 4) 3 0.001)
    (check-within (candidate 1 1 6) 1 0.001)
    (check-within (candidate 1 1 3) 1 0.001)
))

(test-humaneval)