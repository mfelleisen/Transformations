#lang racket

;; Calculate the price of a number based on its binary representation
;; and the given position x.
(define (calculate-price num x)
  (define bin-rep (number->string num 2))  ; Convert number to binary string
  (for/fold ([price 0])  ; Accumulate the price using a fold
            ([i (in-range (string-length bin-rep))]
             #:when (and (= (modulo (+ i 1) x) 0)  ; Check if the position is a multiple of x
                         (char=? (string-ref bin-rep (- (string-length bin-rep) i 1)) #\1)))  ; Check if the bit is set
    (add1 price)))  ; Increment the price if conditions are met

;; Find the maximum number such that the sum of prices from 1 to that number
;; does not exceed k.
(define (findMaximumNumber k x)
  (define (price num)
    (define bin (number->string num 2))
    (for/fold ([p 0]) ([i (in-range (string-length bin))])
      (if (and (= (modulo (add1 i) x) 0) (char=? (string-ref bin (- (string-length bin) (add1 i))) #\1))
          (add1 p)
          p)))
  
  (define (helper num sum)
    (define p (price num))
    (define new-sum (+ sum p))
    (if (> new-sum k)
        (sub1 num)
        (helper (add1 num) new-sum)))
  
  (helper 1 0))

;; Example use cases
(findMaximumNumber 9 1)  ; Output: 6
(findMaximumNumber 7 2)  ; Output: 9

(require rackunit)


(define (test-humaneval) 

  (let (( candidate findMaximumNumber))
    (check-within (candidate 9 1) 6 0.001)
    (check-within (candidate 7 2) 9 0.001)
    (check-within (candidate 19 6) 50 0.001)
    (check-within (candidate 57 4) 120 0.001)
    (check-within (candidate 58 5) 121 0.001)
    (check-within (candidate 60 8) 187 0.001)
    (check-within (candidate 72 5) 151 0.001)
    (check-within (candidate 81 6) 176 0.001)
    (check-within (candidate 83 1) 33 0.001)
    (check-within (candidate 83 7) 210 0.001)
    (check-within (candidate 116 5) 243 0.001)
    (check-within (candidate 157 6) 316 0.001)
    (check-within (candidate 201 3) 212 0.001)
    (check-within (candidate 268 6) 555 0.001)
    (check-within (candidate 281 5) 531 0.001)
    (check-within (candidate 283 3) 274 0.001)
    (check-within (candidate 309 4) 364 0.001)
    (check-within (candidate 363 7) 746 0.001)
    (check-within (candidate 409 2) 220 0.001)
    (check-within (candidate 456 7) 967 0.001)
    (check-within (candidate 466 3) 365 0.001)
    (check-within (candidate 500 3) 379 0.001)
    (check-within (candidate 513 1) 148 0.001)
    (check-within (candidate 521 8) 1160 0.001)
    (check-within (candidate 540 4) 571 0.001)
    (check-within (candidate 545 1) 156 0.001)
    (check-within (candidate 579 1) 165 0.001)
    (check-within (candidate 584 1) 166 0.001)
    (check-within (candidate 589 3) 427 0.001)
    (check-within (candidate 599 6) 1206 0.001)
    (check-within (candidate 632 2) 346 0.001)
    (check-within (candidate 692 3) 481 0.001)
    (check-within (candidate 701 7) 1404 0.001)
    (check-within (candidate 704 4) 727 0.001)
    (check-within (candidate 731 7) 1498 0.001)
    (check-within (candidate 781 1) 210 0.001)
    (check-within (candidate 782 7) 1613 0.001)
    (check-within (candidate 808 6) 1639 0.001)
    (check-within (candidate 814 7) 1645 0.001)
    (check-within (candidate 818 1) 218 0.001)
    (check-within (candidate 821 2) 433 0.001)
    (check-within (candidate 829 6) 1660 0.001)
    (check-within (candidate 865 7) 1760 0.001)
    (check-within (candidate 874 6) 1769 0.001)
    (check-within (candidate 879 1) 230 0.001)
    (check-within (candidate 879 3) 628 0.001)
    (check-within (candidate 898 8) 1921 0.001)
    (check-within (candidate 902 3) 653 0.001)
    (check-within (candidate 905 8) 1928 0.001)
    (check-within (candidate 937 8) 1960 0.001)
    (check-within (candidate 957 3) 701 0.001)
    (check-within (candidate 973 1) 247 0.001)
    (check-within (candidate 978 3) 737 0.001)
    (check-within (candidate 991 5) 1006 0.001)
    (check-within (candidate 1029 3) 771 0.001)
    (check-within (candidate 1065 6) 2083 0.001)
    (check-within (candidate 1086 3) 805 0.001)
    (check-within (candidate 1105 1) 280 0.001)
    (check-within (candidate 1113 3) 815 0.001)
    (check-within (candidate 1143 4) 1190 0.001)
    (check-within (candidate 1148 2) 564 0.001)
    (check-within (candidate 1150 7) 2301 0.001)
    (check-within (candidate 1156 3) 835 0.001)
    (check-within (candidate 1171 7) 2386 0.001)
    (check-within (candidate 1172 1) 297 0.001)
    (check-within (candidate 1227 7) 2506 0.001)
    (check-within (candidate 1236 8) 2515 0.001)
    (check-within (candidate 1270 5) 1525 0.001)
    (check-within (candidate 1274 6) 2220 0.001)
    (check-within (candidate 1281 6) 2223 0.001)
    (check-within (candidate 1282 6) 2224 0.001)
    (check-within (candidate 1288 5) 1543 0.001)
    (check-within (candidate 1376 6) 2287 0.001)
    (check-within (candidate 1393 7) 2800 0.001)
    (check-within (candidate 1415 4) 1454 0.001)
    (check-within (candidate 1446 7) 2917 0.001)
    (check-within (candidate 1459 1) 358 0.001)
    (check-within (candidate 1520 3) 1017 0.001)
    (check-within (candidate 1539 6) 2400 0.001)
    (check-within (candidate 1545 7) 3144 0.001)
    (check-within (candidate 1573 5) 1732 0.001)
    (check-within (candidate 1588 8) 3251 0.001)
    (check-within (candidate 1590 7) 3189 0.001)
    (check-within (candidate 1617 7) 3280 0.001)
    (check-within (candidate 1633 6) 2463 0.001)
    (check-within (candidate 1634 7) 3297 0.001)
    (check-within (candidate 1687 2) 741 0.001)
    (check-within (candidate 1731 6) 2528 0.001)
    (check-within (candidate 1750 5) 1850 0.001)
    (check-within (candidate 1751 7) 3542 0.001)
    (check-within (candidate 1760 8) 3551 0.001)
    (check-within (candidate 1782 8) 3573 0.001)
    (check-within (candidate 1787 2) 766 0.001)
    (check-within (candidate 1851 2) 797 0.001)
    (check-within (candidate 1856 2) 799 0.001)
    (check-within (candidate 1874 8) 3793 0.001)
    (check-within (candidate 1893 7) 3812 0.001)
    (check-within (candidate 1900 1) 444 0.001)
    (check-within (candidate 1900 7) 3819 0.001)
    (check-within (candidate 1902 3) 1336 0.001)
))

(test-humaneval)