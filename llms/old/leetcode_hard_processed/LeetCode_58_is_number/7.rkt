#lang racket

;; A valid number can be split up into these components (in order):
;; A decimal number or an integer.
;; (Optional) An 'e' or 'E', followed by an integer.
;; A decimal number can be split up into these components (in order):
;; (Optional) A sign character (either '+' or '-').
;; One of the following formats:
;; One or more digits, followed by a dot '.'.
;; One or more digits, followed by a dot '.', followed by one or more digits.
;; A dot '.', followed by one or more digits.
;; An integer can be split up into these components (in order):
;; (Optional) A sign character (either '+' or '-').
;; One or more digits.
;; For example, all the following are valid numbers: ["2", "0089", "-0.1", "+3.14", "4.", "-.9", "2e10", "-90E3", "3e+7", "+6e-1", "53.5e93", "-123.456e789"], while the following are not valid numbers: ["abc", "1a", "1e", "e3", "99e2.5", "--6", "-+3", "95a54e53"].
;; Given a string s, return true if s is a valid number.
;; Example 1:
;; Input: s = "0"
;; Output: true
;; Example 2:
;; Input: s = "e"
;; Output: false
;; Example 3:
;; Input: s = "."
;; Output: false
;; Constraints:
;; 1 <= s.length <= 20
;; s consists of only English letters (both uppercase and lowercase), digits (0-9), plus '+', minus '-', or dot '.'.
(define (is_number s)
  ;; Use regular expressions to define a pattern that matches the criteria for a valid number.
  ;; The pattern is translated directly from the Python version to a Racket regular expression.
  (define pattern
    #"^[-+]?(([0-9]+(\\.[0-9]*)?)|(\\.[0-9]+))(e[-+]?[0-9]+)?$")

  ;; Use regexp-match to check if the string matches the pattern.
  ;; regexp-match returns #f if there is no match, otherwise it returns a list of matches.
  ;; The result is coerced into a boolean: #true if there's a match, #false otherwise.
  (not (equal? #f (regexp-match pattern s))))
(require rackunit)

(define (test-humaneval) 

  (let (( candidate is_number))
    (check-within (candidate "3e+") #f 0.001)
    (check-within (candidate "-1.2e-5.") #f 0.001)
    (check-within (candidate "6e-1.") #f 0.001)
    (check-within (candidate ".") #f 0.001)
    (check-within (candidate "-.8e-1") #t 0.001)
    (check-within (candidate ".1e2") #t 0.001)
    (check-within (candidate "0e") #f 0.001)
    (check-within (candidate "0.123e7") #t 0.001)
    (check-within (candidate "3e+7") #t 0.001)
    (check-within (candidate "123.e-2") #t 0.001)
    (check-within (candidate "2") #t 0.001)
    (check-within (candidate "+.8") #t 0.001)
    (check-within (candidate "+3.14") #t 0.001)
    (check-within (candidate "1.2e5.") #f 0.001)
    (check-within (candidate ".1e") #f 0.001)
    (check-within (candidate "-4.3e-10") #t 0.001)
    (check-within (candidate "--6") #f 0.001)
    (check-within (candidate "+") #f 0.001)
    (check-within (candidate "2e10") #t 0.001)
    (check-within (candidate "99e2.5") #f 0.001)
    (check-within (candidate "3.e2") #t 0.001)
    (check-within (candidate "+12E") #f 0.001)
    (check-within (candidate "+.123") #t 0.001)
    (check-within (candidate "123.456e789") #t 0.001)
    (check-within (candidate "12.3") #t 0.001)
    (check-within (candidate "0") #t 0.001)
    (check-within (candidate "+6e-1") #t 0.001)
    (check-within (candidate "12e5") #t 0.001)
    (check-within (candidate "-.") #f 0.001)
    (check-within (candidate "3e-") #f 0.001)
    (check-within (candidate "-.9") #t 0.001)
    (check-within (candidate "3e+7.") #f 0.001)
    (check-within (candidate "-.123e-4") #t 0.001)
    (check-within (candidate "-") #f 0.001)
    (check-within (candidate "+12e-") #f 0.001)
    (check-within (candidate "e3") #f 0.001)
    (check-within (candidate "-1.2e-5") #t 0.001)
    (check-within (candidate "1.2e+5") #t 0.001)
    (check-within (candidate "e2") #f 0.001)
    (check-within (candidate "46.00") #t 0.001)
    (check-within (candidate "1a") #f 0.001)
    (check-within (candidate ".e1") #f 0.001)
    (check-within (candidate "+1.23e4") #t 0.001)
    (check-within (candidate "e-1") #f 0.001)
    (check-within (candidate "12e5.") #f 0.001)
    (check-within (candidate "-0.1") #t 0.001)
    (check-within (candidate "+1.2E-5.") #f 0.001)
    (check-within (candidate "46.e3") #t 0.001)
    (check-within (candidate "+E3") #f 0.001)
    (check-within (candidate "+.123e+4") #t 0.001)
    (check-within (candidate "1.2e-5") #t 0.001)
    (check-within (candidate "-123.456e789") #t 0.001)
    (check-within (candidate "3.") #t 0.001)
    (check-within (candidate "1.23") #t 0.001)
    (check-within (candidate "-1.2E5.") #f 0.001)
    (check-within (candidate "-0.1e-2") #t 0.001)
    (check-within (candidate "+-3") #f 0.001)
    (check-within (candidate "+22.") #t 0.001)
    (check-within (candidate "-.9e-1") #t 0.001)
    (check-within (candidate "3e-7") #t 0.001)
    (check-within (candidate "-.123") #t 0.001)
    (check-within (candidate "-e") #f 0.001)
    (check-within (candidate "abc") #f 0.001)
    (check-within (candidate "-.1e2") #t 0.001)
    (check-within (candidate "3e7") #t 0.001)
    (check-within (candidate "-e3") #f 0.001)
    (check-within (candidate "7.e-1") #t 0.001)
    (check-within (candidate "+1.23") #t 0.001)
    (check-within (candidate "0e-2") #t 0.001)
    (check-within (candidate "+1.2e-5.") #f 0.001)
    (check-within (candidate "-1.23e4") #t 0.001)
    (check-within (candidate "123.E-2.5") #f 0.001)
    (check-within (candidate "0089") #t 0.001)
    (check-within (candidate "4.") #t 0.001)
    (check-within (candidate "1e") #f 0.001)
    (check-within (candidate "-+3") #f 0.001)
    (check-within (candidate ".1") #t 0.001)
    (check-within (candidate "e+789") #f 0.001)
    (check-within (candidate " ") #f 0.001)
    (check-within (candidate "-6.e+8") #t 0.001)
    (check-within (candidate "12e-3.5") #f 0.001)
    (check-within (candidate "1e-1.3") #f 0.001)
    (check-within (candidate "+1.2e-5") #t 0.001)
    (check-within (candidate ".00e+2") #t 0.001)
    (check-within (candidate "3e-6") #t 0.001)
    (check-within (candidate "e") #f 0.001)
    (check-within (candidate "123.e-2.5") #f 0.001)
    (check-within (candidate "1e2.458") #f 0.001)
    (check-within (candidate "-e10") #f 0.001)
    (check-within (candidate "4") #t 0.001)
    (check-within (candidate "-1.2e5") #t 0.001)
    (check-within (candidate "95a54e53") #f 0.001)
    (check-within (candidate "53.5e93") #t 0.001)
    (check-within (candidate "123.") #t 0.001)
    (check-within (candidate "-12E") #f 0.001)
))

(test-humaneval)