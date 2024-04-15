#lang racket

;; Given a string containing just the characters '(' and ')', return the length of the longest valid (well-formed) parentheses substring.
;; Example 1:
;; Input: s = "(()"
;; Output: 2
;; Explanation: The longest valid parentheses substring is "()".
;; Example 2:
;; Input: s = ")()())"
;; Output: 4
;; Explanation: The longest valid parentheses substring is "()()".
;; Example 3:
;; Input: s = ""
;; Output: 0
;; Constraints:
;; 0 <= s.length <= 3 * 104
;; s[i] is '(', or ')'.
(define (longest_valid_parentheses s)
  ;; Initialize variables: n for the length of the string,
  ;; result to store the maximum length found,
  ;; and st as a stack to keep track of indices.
  (let ([n (string-length s)]
        [result 0]
        [st '()])
    ;; Iterate through the string
    (for ([i (in-range n)])
      (cond
        ;; If the current character is '(', push its index onto the stack.
        [(char=? (string-ref s i) #\()
         (set! st (cons i st))]
        ;; Otherwise, check if there's a matching '('.
        [else
         (if (and (not (null? st)) (char=? (string-ref s (car st)) #\())
             ;; If there's a match, pop the stack and continue.
             (set! st (cdr st))
             ;; If not, this ')' starts a new potential valid substring.
             (set! st (cons i st)))]))
    ;; If the stack is empty, the whole string is valid.
    (if (null? st)
        (set! result n)
        (let loop ([right n] [left 0])
          (if (null? st)
              (set! result (max result right))
              (begin
                (set! left (car st))
                (set! st (cdr st))
                (set! result (max result (- right left 1)))
                (set! right left)
                (loop right left)))))
    ;; Return the maximum length found.
    result))

;; Example usage:
(longest_valid_parentheses "(()")  ; Should return 2
(longest_valid_parentheses ")()())")  ; Should return 4
(longest_valid_parentheses "")  ; Should return 0
(require rackunit)

(define (test-humaneval) 

  (let (( candidate longest_valid_parentheses))
    (check-within (candidate "()()") 4 0.001)
    (check-within (candidate "()(((((((()())()))))))()(()))()())") 28 0.001)
    (check-within (candidate ")()())()()())(())") 6 0.001)
    (check-within (candidate ")()())") 4 0.001)
    (check-within (candidate ")(()()())") 8 0.001)
    (check-within (candidate "(()())())") 8 0.001)
    (check-within (candidate "()(()))") 6 0.001)
    (check-within (candidate "(()") 2 0.001)
    (check-within (candidate "(()(()())") 8 0.001)
    (check-within (candidate "()") 2 0.001)
    (check-within (candidate "()(())") 6 0.001)
    (check-within (candidate "()(((((((()())()))))))()(()))())") 28 0.001)
    (check-within (candidate "((((((((((()))))))))") 18 0.001)
    (check-within (candidate "((((((((((())))))))))") 20 0.001)
    (check-within (candidate "") 0 0.001)
    (check-within (candidate "()((((((((((((((((((())))))))))))))))()))(())))") 46 0.001)
    (check-within (candidate "((()()()())(()))())()()()()()") 18 0.001)
    (check-within (candidate "()(()())") 8 0.001)
    (check-within (candidate "(()((())())()()()()()()())") 26 0.001)
    (check-within (candidate "()(()") 2 0.001)
))

(test-humaneval)