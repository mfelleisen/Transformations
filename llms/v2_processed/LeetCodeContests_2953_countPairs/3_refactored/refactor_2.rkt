#lang racket

;; You are given a 2D integer array coordinates and an integer k, where coordinates[i] = [xi, yi] are the coordinates of the ith point in a 2D plane.
;; We define the distance between two points (x1, y1) and (x2, y2) as (x1 XOR x2) + (y1 XOR y2) where XOR is the bitwise XOR operation.
;; Return the number of pairs (i, j) such that i < j and the distance between points i and j is equal to k.
;; Example 1:
;; Input: coordinates = [[1,2],[4,2],[1,3],[5,2]], k = 5
;; Output: 2
;; Explanation: We can choose the following pairs:
;; - (0,1): Because we have (1 XOR 4) + (2 XOR 2) = 5.
;; - (2,3): Because we have (1 XOR 5) + (3 XOR 2) = 5.
;; Example 2:
;; Input: coordinates = [[1,3],[1,3],[1,3],[1,3],[1,3]], k = 0
;; Output: 10
;; Explanation: Any two chosen pairs will have a distance of 0. There are 10 ways to choose two pairs.
;; Constraints:
;;  * 2 <= coordinates.length <= 50000
;;  * 0 <= xi, yi <= 106
;;  * 0 <= k <= 100
(define (countPairs coordinates k)
  (define (calculate-distance p1 p2)
    ;; This helper function calculates the XOR-based distance between two points
    (+ (bitwise-xor (first p1) (first p2)) 
       (bitwise-xor (second p1) (second p2))))

  ;; Use a nested loop to iterate over all pairs (i, j) where i < j
  (for*/sum ([i (in-range (length coordinates))]
             [j (in-range (add1 i) (length coordinates))]
             #:when (= k (calculate-distance (list-ref coordinates i) (list-ref coordinates j))))
    1))

;; Example usage:
;; (countPairs '([1 2] [4 2] [1 3] [5 2]) 5)  ; Should return 2
;; (countPairs '([1 3] [1 3] [1 3] [1 3] [1 3]) 0)  ; Should return 10

(require rackunit)


(define (test-humaneval) 

  (let (( candidate countPairs))
    (check-within (candidate (list (list 1 2) (list 4 2) (list 1 3) (list 5 2)) 5) 2 0.001)
    (check-within (candidate (list (list 1 3) (list 1 3) (list 1 3) (list 1 3) (list 1 3)) 0) 10 0.001)
    (check-within (candidate (list (list 27 94) (list 61 68) (list 47 0) (list 100 4) (list 127 89) (list 61 103) (list 26 4) (list 51 54) (list 91 26) (list 98 23) (list 80 74) (list 19 93)) 95) 5 0.001)
    (check-within (candidate (list (list 39 29) (list 98 59) (list 65 77) (list 41 26) (list 95 12) (list 71 66) (list 41 93) (list 28 33) (list 96 40) (list 39 8) (list 106 54) (list 8 49) (list 68 59) (list 21 15) (list 3 66) (list 77 85) (list 111 51)) 21) 6 0.001)
    (check-within (candidate (list (list 100 32) (list 69 8) (list 85 31) (list 69 47) (list 62 34) (list 102 43) (list 81 39) (list 90 0) (list 123 6) (list 79 18) (list 21 94) (list 13 36) (list 49 97) (list 76 59) (list 42 74) (list 60 68) (list 21 11) (list 71 21) (list 64 13) (list 64 95) (list 5 85) (list 118 53) (list 70 44) (list 38 57) (list 32 119) (list 80 61) (list 13 68) (list 43 108) (list 86 49)) 39) 20 0.001)
    (check-within (candidate (list (list 60 55) (list 35 32) (list 99 2) (list 58 57) (list 16 2) (list 43 28) (list 30 35) (list 35 83) (list 104 41) (list 20 69) (list 58 14) (list 12 92) (list 71 49) (list 7 82) (list 65 68) (list 9 40) (list 15 56) (list 57 46) (list 21 8) (list 37 64) (list 42 94) (list 73 91) (list 12 121) (list 10 21) (list 41 89)) 54) 10 0.001)
    (check-within (candidate (list (list 94 23) (list 86 58) (list 126 55) (list 107 23) (list 121 60) (list 89 28) (list 123 15) (list 127 3) (list 100 49) (list 5 3) (list 81 49) (list 93 0) (list 95 37) (list 92 25)) 53) 18 0.001)
    (check-within (candidate (list (list 40 54) (list 8 68) (list 33 11) (list 51 93) (list 95 95) (list 17 53) (list 35 39) (list 59 42) (list 28 63) (list 41 63) (list 54 0) (list 88 31) (list 5 107) (list 32 124) (list 74 64) (list 15 27) (list 61 92) (list 16 47) (list 62 22) (list 2 28) (list 27 14) (list 53 39) (list 21 91) (list 7 11)) 60) 11 0.001)
    (check-within (candidate (list (list 28 14) (list 2 13) (list 28 14) (list 4 7) (list 23 1) (list 54 0) (list 43 22) (list 98 16)) 33) 5 0.001)
    (check-within (candidate (list (list 84 92) (list 84 92) (list 84 92) (list 84 92) (list 84 92) (list 54 59) (list 84 92) (list 93 44)) 0) 15 0.001)
    (check-within (candidate (list (list 10 57) (list 12 62) (list 92 44) (list 7 60) (list 8 55) (list 13 50) (list 5 55) (list 71 82) (list 64 26) (list 68 43) (list 61 88) (list 9 44) (list 95 16) (list 17 16) (list 12 53) (list 9 59) (list 81 44) (list 3 56) (list 70 94) (list 0 58) (list 84 29) (list 13 63) (list 79 87) (list 19 39) (list 74 35) (list 92 7) (list 31 6) (list 2 50)) 13) 19 0.001)
    (check-within (candidate (list (list 56 47) (list 26 50) (list 51 2) (list 40 7) (list 24 34) (list 55 2) (list 13 92) (list 57 50) (list 47 35) (list 32 96) (list 14 0) (list 4 84) (list 86 95)) 56) 4 0.001)
    (check-within (candidate (list (list 34 60) (list 17 93) (list 87 90) (list 32 125) (list 71 27) (list 27 26) (list 127 115) (list 91 27) (list 63 68) (list 97 48) (list 69 73) (list 120 78) (list 43 55) (list 101 125) (list 86 87) (list 12 35) (list 5 20) (list 46 12) (list 17 24) (list 107 62) (list 86 88) (list 26 80) (list 30 41) (list 110 114)) 81) 17 0.001)
    (check-within (candidate (list (list 65 19) (list 12 80) (list 90 64) (list 38 68) (list 17 25) (list 49 36) (list 91 47) (list 20 31) (list 81 54) (list 83 20) (list 90 100) (list 0 6) (list 93 121)) 36) 3 0.001)
    (check-within (candidate (list (list 24 75) (list 22 67)) 23) 0 0.001)
    (check-within (candidate (list (list 42 32) (list 62 60) (list 57 61) (list 100 56) (list 91 62) (list 57 21) (list 100 56) (list 63 63) (list 45 52) (list 59 75) (list 32 61) (list 57 43) (list 61 57) (list 64 52) (list 24 54) (list 92 15) (list 53 25) (list 84 63) (list 1 18) (list 21 57) (list 29 9) (list 68 91) (list 22 43) (list 105 27)) 48) 18 0.001)
    (check-within (candidate (list (list 70 98) (list 79 66) (list 71 63) (list 111 94) (list 3 50) (list 64 111) (list 98 67) (list 23 41) (list 66 14) (list 40 19) (list 15 13) (list 32 86) (list 59 58) (list 73 94) (list 18 10) (list 77 50) (list 20 60) (list 66 8) (list 15 30) (list 71 2) (list 55 9)) 60) 7 0.001)
    (check-within (candidate (list (list 5 100) (list 60 9) (list 84 65) (list 38 66) (list 83 35) (list 17 80) (list 88 76) (list 80 101) (list 55 74) (list 46 62) (list 28 73) (list 54 40) (list 119 71) (list 10 94) (list 45 82) (list 20 90) (list 47 27) (list 41 97) (list 66 5) (list 33 0) (list 101 5) (list 89 125) (list 6 58) (list 61 107) (list 25 17) (list 104 0) (list 29 2)) 73) 15 0.001)
    (check-within (candidate (list (list 29 23) (list 8 19) (list 26 5) (list 12 25) (list 37 2) (list 37 27) (list 18 68) (list 3 53) (list 81 85) (list 27 94) (list 29 39) (list 41 64) (list 26 28) (list 23 80) (list 13 46) (list 5 68) (list 16 18) (list 21 77)) 25) 8 0.001)
    (check-within (candidate (list (list 90 31) (list 113 54) (list 92 36) (list 67 49) (list 123 124) (list 127 112) (list 16 24) (list 85 50) (list 58 94) (list 115 48) (list 83 30) (list 51 112) (list 39 23) (list 0 21) (list 27 44) (list 99 100) (list 122 63) (list 34 39) (list 25 48) (list 44 49) (list 84 97) (list 31 61)) 84) 10 0.001)
    (check-within (candidate (list (list 51 47) (list 51 47) (list 8 14) (list 82 68) (list 55 85) (list 8 14) (list 51 47) (list 87 97) (list 75 65) (list 78 10) (list 51 47) (list 87 97) (list 74 19) (list 51 47) (list 56 66) (list 8 14) (list 78 10) (list 74 66) (list 65 92) (list 51 47) (list 3 31)) 0) 20 0.001)
    (check-within (candidate (list (list 25 82) (list 86 89) (list 25 82) (list 47 118) (list 14 58) (list 22 51) (list 0 93) (list 26 9) (list 67 27) (list 43 22) (list 78 49) (list 82 15) (list 93 22) (list 67 34) (list 54 43) (list 61 55) (list 74 77) (list 115 108) (list 54 55) (list 9 30) (list 31 3) (list 26 5) (list 60 49)) 90) 22 0.001)
    (check-within (candidate (list (list 29 23) (list 48 3) (list 58 62) (list 16 19) (list 0 30) (list 59 5) (list 96 50) (list 7 46) (list 5 18) (list 42 32) (list 78 55)) 17) 3 0.001)
    (check-within (candidate (list (list 47 68) (list 55 68) (list 36 73) (list 33 70) (list 36 81) (list 60 81) (list 32 18) (list 38 95) (list 34 75) (list 33 5) (list 33 78) (list 32 10) (list 36 93) (list 56 77) (list 43 17) (list 99 70) (list 15 77) (list 42 87) (list 30 18) (list 36 56) (list 47 68) (list 45 70) (list 48 77) (list 53 94) (list 0 86) (list 53 9) (list 68 35) (list 32 77) (list 95 90)) 24) 31 0.001)
    (check-within (candidate (list (list 5 100) (list 19 21) (list 83 36) (list 24 59) (list 92 49) (list 6 73) (list 57 78) (list 69 33) (list 3 81) (list 53 59) (list 23 40) (list 6 21) (list 57 55) (list 98 43) (list 33 15) (list 8 83) (list 29 29) (list 85 41) (list 47 64) (list 10 32) (list 82 94) (list 14 29) (list 13 99) (list 19 20) (list 85 108) (list 41 9)) 78) 12 0.001)
    (check-within (candidate (list (list 8 94) (list 19 13) (list 72 75) (list 17 8) (list 57 45) (list 17 15) (list 14 95) (list 74 78) (list 17 15) (list 9 95) (list 79 76) (list 13 91) (list 28 76) (list 94 12) (list 11 90) (list 94 11) (list 94 11) (list 15 89) (list 20 13) (list 23 14) (list 22 8) (list 21 71)) 7) 24 0.001)
    (check-within (candidate (list (list 37 76) (list 109 71) (list 66 1) (list 55 6) (list 90 22) (list 71 24) (list 3 19) (list 46 24) (list 74 74) (list 85 94) (list 2 96) (list 1 48) (list 31 86) (list 22 78) (list 93 80) (list 3 112) (list 11 11) (list 98 18) (list 81 86) (list 55 54) (list 82 18) (list 127 23)) 83) 11 0.001)
    (check-within (candidate (list (list 9 25) (list 56 25) (list 7 58) (list 9 48) (list 77 55) (list 6 10) (list 33 98) (list 22 26) (list 41 57) (list 18 4) (list 40 74)) 49) 8 0.001)
    (check-within (candidate (list (list 91 12) (list 86 8) (list 74 12) (list 85 58) (list 65 10) (list 49 51) (list 43 83) (list 34 91) (list 89 63) (list 26 44) (list 68 6) (list 71 8) (list 92 12) (list 49 79) (list 64 26) (list 0 87) (list 22 85) (list 15 72) (list 17 54) (list 33 37) (list 70 9) (list 88 95) (list 85 67) (list 32 85) (list 94 69) (list 87 77)) 17) 16 0.001)
    (check-within (candidate (list (list 54 60) (list 31 62) (list 76 56) (list 79 44)) 52) 0 0.001)
    (check-within (candidate (list (list 41 13) (list 15 74) (list 43 51) (list 44 10) (list 49 72) (list 63 48) (list 50 40) (list 90 86) (list 105 13) (list 11 118) (list 55 8) (list 3 39) (list 27 3) (list 55 72) (list 33 98) (list 10 59) (list 40 45) (list 10 59) (list 40 30) (list 97 43) (list 96 55) (list 47 32) (list 43 86) (list 57 61) (list 1 64)) 64) 23 0.001)
    (check-within (candidate (list (list 29 96) (list 82 101) (list 1 88) (list 9 100) (list 55 42) (list 37 77) (list 89 95) (list 40 10) (list 111 114) (list 89 53) (list 91 33) (list 93 18) (list 90 14) (list 50 49) (list 27 91) (list 99 92) (list 26 15) (list 69 17) (list 61 64)) 84) 7 0.001)
    (check-within (candidate (list (list 57 88) (list 83 2) (list 82 23) (list 19 7) (list 43 84) (list 54 87) (list 51 38) (list 61 68) (list 68 31) (list 74 49) (list 64 80) (list 2 19) (list 18 73) (list 52 73) (list 75 26) (list 32 71) (list 91 83) (list 84 15) (list 49 76)) 30) 8 0.001)
    (check-within (candidate (list (list 34 96) (list 53 25) (list 97 70) (list 48 31) (list 48 20) (list 54 26) (list 42 99) (list 52 24) (list 56 100) (list 35 106) (list 16 71) (list 34 69) (list 42 72) (list 28 8) (list 35 97) (list 103 67) (list 12 81) (list 8 86)) 11) 10 0.001)
    (check-within (candidate (list (list 60 56) (list 48 34) (list 21 82) (list 63 26) (list 97 51) (list 35 63) (list 39 29) (list 5 46) (list 16 115) (list 19 71) (list 34 54) (list 6 65) (list 11 21) (list 54 66) (list 2 103) (list 13 64) (list 30 73) (list 23 58) (list 31 75) (list 6 63) (list 16 66) (list 21 100)) 38) 10 0.001)
    (check-within (candidate (list (list 5 28) (list 16 39) (list 38 16) (list 21 34) (list 5 22) (list 73 52) (list 3 24) (list 24 37) (list 11 26)) 10) 5 0.001)
    (check-within (candidate (list (list 34 76) (list 50 71) (list 55 74) (list 36 6) (list 56 77) (list 56 86) (list 9 25) (list 7 38) (list 34 76) (list 96 85) (list 29 32)) 27) 8 0.001)
    (check-within (candidate (list (list 69 99) (list 60 80) (list 59 72) (list 74 67) (list 34 78) (list 73 95) (list 65 72) (list 86 64) (list 42 89) (list 90 25) (list 84 48)) 31) 8 0.001)
    (check-within (candidate (list (list 50 75) (list 84 10) (list 3 1) (list 8 12) (list 41 82) (list 68 39) (list 55 31) (list 4 103) (list 50 19) (list 15 85) (list 20 50) (list 118 81) (list 47 14) (list 1 40) (list 1 58) (list 8 58) (list 18 110) (list 62 10) (list 98 69) (list 25 31) (list 99 10) (list 74 29) (list 124 73)) 98) 15 0.001)
    (check-within (candidate (list (list 65 100) (list 43 13) (list 80 116) (list 40 82) (list 50 5) (list 53 14) (list 62 16) (list 38 8) (list 83 107) (list 56 11) (list 82 92) (list 62 16) (list 59 21) (list 38 8) (list 55 50) (list 67 76) (list 36 65)) 33) 14 0.001)
    (check-within (candidate (list (list 52 32) (list 42 21) (list 1 56) (list 93 52) (list 85 87) (list 14 58) (list 39 21) (list 3 105) (list 18 13) (list 5 119) (list 108 77) (list 91 81) (list 22 71) (list 76 39) (list 2 59) (list 23 54) (list 83 26) (list 28 23) (list 33 69) (list 27 91) (list 92 19) (list 53 5) (list 39 32) (list 14 124)) 83) 21 0.001)
    (check-within (candidate (list (list 84 63) (list 92 55) (list 56 94) (list 89 27) (list 53 93) (list 85 80) (list 65 91) (list 77 16) (list 28 99) (list 48 86) (list 54 44) (list 33 47) (list 47 10) (list 11 62) (list 2 17)) 16) 4 0.001)
    (check-within (candidate (list (list 78 84) (list 91 79) (list 1 35) (list 73 76) (list 89 92) (list 69 94) (list 78 1) (list 27 71) (list 17 58) (list 18 33) (list 82 67) (list 24 59) (list 23 53) (list 82 86)) 21) 8 0.001)
    (check-within (candidate (list (list 29 53) (list 40 74) (list 42 73) (list 24 53) (list 79 50) (list 13 7) (list 43 72) (list 26 54) (list 41 75) (list 66 27) (list 43 72) (list 81 75) (list 47 73) (list 74 43) (list 97 60) (list 42 76) (list 46 77) (list 21 69) (list 88 77)) 5) 16 0.001)
    (check-within (candidate (list (list 21 95) (list 53 15) (list 71 7) (list 22 40) (list 8 89) (list 66 62)) 74) 1 0.001)
    (check-within (candidate (list (list 93 3) (list 89 13) (list 70 48) (list 75 6) (list 43 82) (list 121 49) (list 80 1) (list 122 45) (list 57 45) (list 96 96) (list 86 82) (list 46 62) (list 63 79) (list 10 6) (list 55 36) (list 63 61) (list 79 99)) 92) 8 0.001)
    (check-within (candidate (list (list 0 36) (list 77 49) (list 25 41)) 98) 1 0.001)
    (check-within (candidate (list (list 42 18) (list 48 0) (list 64 62) (list 61 7) (list 33 51) (list 50 26) (list 1 91) (list 24 92)) 44) 4 0.001)
    (check-within (candidate (list (list 69 94) (list 83 39) (list 2 37) (list 117 117) (list 82 54) (list 20 84) (list 91 88) (list 67 63) (list 43 69) (list 109 42) (list 9 69) (list 46 42) (list 60 99) (list 69 74) (list 81 80) (list 12 19)) 91) 11 0.001)
    (check-within (candidate (list (list 75 44) (list 90 42) (list 62 96) (list 80 91) (list 82 78) (list 77 42)) 23) 3 0.001)
    (check-within (candidate (list (list 81 20) (list 74 53) (list 70 49) (list 99 66) (list 11 88)) 60) 2 0.001)
    (check-within (candidate (list (list 33 37) (list 35 52) (list 49 38) (list 47 32) (list 98 98) (list 84 83) (list 50 54) (list 45 34) (list 105 106) (list 54 44) (list 80 57) (list 96 80) (list 83 81) (list 36 22)) 19) 7 0.001)
    (check-within (candidate (list (list 45 38) (list 47 5) (list 13 69) (list 88 65) (list 123 11) (list 15 30) (list 91 45) (list 66 100) (list 25 50) (list 63 10) (list 46 70) (list 36 77) (list 27 9) (list 78 91)) 98) 6 0.001)
    (check-within (candidate (list (list 71 58) (list 60 37) (list 27 97) (list 7 56) (list 56 126) (list 24 59) (list 46 76) (list 15 79) (list 18 3) (list 98 8) (list 110 62) (list 76 30) (list 38 63)) 66) 8 0.001)
    (check-within (candidate (list (list 21 80) (list 17 111) (list 0 126) (list 20 81) (list 50 76) (list 80 32) (list 7 97) (list 21 19) (list 50 91) (list 58 68) (list 55 4) (list 37 56) (list 20 42) (list 6 35) (list 38 72) (list 96 6) (list 11 70) (list 10 91) (list 11 94) (list 46 88) (list 81 64) (list 37 78) (list 15 75) (list 90 79) (list 13 103) (list 46 66) (list 2 95)) 67) 26 0.001)
    (check-within (candidate (list (list 65 15) (list 73 72) (list 60 97) (list 101 107) (list 3 2) (list 4 20) (list 90 74) (list 71 7) (list 113 95) (list 39 17) (list 87 56) (list 2 76) (list 27 122) (list 48 41)) 79) 9 0.001)
    (check-within (candidate (list (list 82 41) (list 27 65) (list 94 92) (list 15 82) (list 56 69) (list 30 57) (list 28 28) (list 5 53) (list 100 2) (list 112 44) (list 23 6) (list 92 29) (list 18 69) (list 124 26) (list 125 88) (list 97 54) (list 7 31) (list 50 80)) 39) 7 0.001)
    (check-within (candidate (list (list 72 31) (list 86 19) (list 63 97) (list 11 118) (list 8 67) (list 14 6) (list 6 69) (list 51 1) (list 70 34) (list 98 68) (list 84 29) (list 47 37) (list 94 75) (list 73 15) (list 34 59) (list 71 42) (list 45 98) (list 22 52) (list 70 94) (list 67 78) (list 64 110) (list 104 5) (list 65 28) (list 87 100) (list 93 10)) 75) 10 0.001)
    (check-within (candidate (list (list 90 16) (list 30 5) (list 16 71) (list 21 75) (list 33 55) (list 76 76) (list 16 50) (list 19 42) (list 18 59) (list 30 46) (list 6 21) (list 19 73) (list 35 78) (list 36 98) (list 30 77) (list 6 65) (list 87 31) (list 69 46) (list 62 42) (list 14 50) (list 44 29) (list 86 56)) 17) 5 0.001)
    (check-within (candidate (list (list 27 30) (list 15 52) (list 26 30) (list 26 30) (list 15 53) (list 75 57) (list 27 30) (list 95 67) (list 26 31) (list 27 31) (list 15 53) (list 90 84) (list 27 30) (list 90 85) (list 10 3) (list 48 59)) 1) 15 0.001)
    (check-within (candidate (list (list 6 12) (list 53 6) (list 16 65) (list 22 42) (list 66 85)) 54) 1 0.001)
    (check-within (candidate (list (list 45 11) (list 43 19) (list 35 27) (list 43 13) (list 38 28) (list 41 59) (list 68 39) (list 29 47)) 30) 5 0.001)
    (check-within (candidate (list (list 39 98) (list 1 97) (list 41 90) (list 1 83) (list 65 2) (list 7 27) (list 79 51) (list 124 88) (list 32 97)) 87) 2 0.001)
    (check-within (candidate (list (list 54 49) (list 98 5) (list 98 25) (list 75 53) (list 117 42) (list 111 6) (list 31 85) (list 124 49) (list 120 115)) 70) 4 0.001)
    (check-within (candidate (list (list 33 9) (list 59 5) (list 71 12) (list 36 2) (list 6 92) (list 32 81) (list 45 72) (list 54 67) (list 17 83) (list 64 19) (list 24 68) (list 58 56) (list 69 87) (list 76 23) (list 86 14) (list 40 25) (list 50 38) (list 50 71)) 38) 8 0.001)
    (check-within (candidate (list (list 7 7) (list 44 51) (list 93 41) (list 43 37) (list 31 2) (list 39 52) (list 12 68) (list 92 78) (list 59 78) (list 95 70) (list 62 45) (list 30 79) (list 7 17) (list 3 89) (list 60 35)) 29) 6 0.001)
    (check-within (candidate (list (list 77 91) (list 3 84) (list 91 18) (list 83 18) (list 56 94) (list 92 19) (list 69 83) (list 88 0) (list 73 95) (list 65 87) (list 95 89) (list 90 90) (list 19 36) (list 94 1) (list 20 18) (list 14 62) (list 77 62) (list 76 92) (list 14 55) (list 22 39) (list 75 95) (list 94 17) (list 21 38)) 8) 10 0.001)
    (check-within (candidate (list (list 27 49) (list 44 38) (list 99 7) (list 32 33) (list 60 98) (list 98 84) (list 93 89) (list 85 80)) 95) 1 0.001)
    (check-within (candidate (list (list 86 74) (list 117 67) (list 106 78) (list 66 82) (list 15 75) (list 76 72) (list 116 64) (list 85 51) (list 109 87) (list 75 69) (list 103 89) (list 80 20) (list 101 95) (list 124 76) (list 91 53) (list 100 84) (list 112 108) (list 45 94) (list 14 96)) 44) 19 0.001)
    (check-within (candidate (list (list 43 81) (list 53 103) (list 106 66) (list 75 67) (list 88 96) (list 112 90) (list 23 87) (list 26 70) (list 75 78) (list 102 100) (list 82 15) (list 69 5) (list 32 106) (list 38 116) (list 10 32) (list 48 46) (list 7 93) (list 61 43) (list 11 38) (list 4 99) (list 58 4) (list 29 10) (list 28 6) (list 40 80) (list 7 110) (list 95 91) (list 24 56) (list 92 53)) 84) 19 0.001)
    (check-within (candidate (list (list 28 78) (list 90 77) (list 51 40) (list 67 125) (list 31 62) (list 19 116) (list 3 79) (list 61 5) (list 39 7) (list 27 9) (list 56 33) (list 100 69) (list 30 72) (list 0 66) (list 17 54) (list 123 6) (list 87 72) (list 11 25) (list 24 49) (list 103 81) (list 37 58) (list 26 53) (list 23 45) (list 120 1) (list 39 96) (list 58 84) (list 97 5)) 73) 17 0.001)
    (check-within (candidate (list (list 63 22) (list 10 98) (list 61 3) (list 7 4) (list 0 111) (list 56 17) (list 50 11) (list 30 97) (list 16 2) (list 59 77) (list 4 48) (list 42 94) (list 63 1) (list 42 3) (list 13 9) (list 27 100) (list 60 30) (list 1 34) (list 54 43) (list 3 32) (list 15 60) (list 39 9) (list 52 82) (list 19 7) (list 42 82) (list 88 96)) 23) 18 0.001)
    (check-within (candidate (list (list 76 84) (list 58 43) (list 15 66) (list 83 35) (list 38 10) (list 12 44) (list 70 34) (list 20 36) (list 13 29) (list 17 24) (list 53 100)) 61) 3 0.001)
    (check-within (candidate (list (list 5 32) (list 28 98) (list 26 96) (list 30 100) (list 29 101) (list 32 50) (list 0 73) (list 29 101) (list 65 92) (list 54 15) (list 1 36) (list 68 46) (list 98 62) (list 67 90) (list 28 98) (list 12 81) (list 16 83) (list 55 77) (list 49 14) (list 0 12) (list 25 101) (list 27 99) (list 4 47) (list 19 99) (list 63 62) (list 56 92)) 8) 18 0.001)
    (check-within (candidate (list (list 95 54) (list 53 94) (list 90 47) (list 89 90) (list 90 47) (list 73 36) (list 73 84) (list 72 49) (list 63 91) (list 39 66) (list 57 80) (list 80 59)) 30) 8 0.001)
    (check-within (candidate (list (list 66 53) (list 64 2) (list 94 55) (list 85 23) (list 74 7) (list 18 83) (list 32 95) (list 55 13) (list 81 34) (list 25 125) (list 73 75) (list 49 32) (list 57 19) (list 0 19) (list 72 79) (list 65 8) (list 118 38) (list 44 44) (list 68 16) (list 62 62) (list 0 116) (list 60 21)) 57) 7 0.001)
    (check-within (candidate (list (list 38 73) (list 37 117) (list 95 92) (list 28 22) (list 16 64) (list 53 0) (list 65 85) (list 91 16) (list 82 28) (list 57 9) (list 53 75) (list 47 45) (list 30 43) (list 91 47) (list 56 94) (list 53 39)) 63) 5 0.001)
    (check-within (candidate (list (list 11 11) (list 96 86) (list 86 64) (list 94 11) (list 121 100) (list 68 1) (list 84 54) (list 21 40) (list 8 3) (list 96 44) (list 96 127) (list 42 25) (list 43 119) (list 94 10) (list 71 0) (list 84 96) (list 79 73) (list 37 11) (list 74 15) (list 4 53) (list 27 59) (list 0 67)) 83) 13 0.001)
    (check-within (candidate (list (list 0 8) (list 45 94) (list 87 72) (list 12 98) (list 4 16) (list 91 88) (list 26 100) (list 8 31) (list 56 89) (list 13 54) (list 22 26) (list 2 18) (list 7 36) (list 19 13) (list 61 72) (list 44 10) (list 44 87) (list 1 38) (list 25 23) (list 24 36) (list 21 50) (list 27 13) (list 95 68) (list 15 13) (list 54 68) (list 5 62)) 28) 17 0.001)
    (check-within (candidate (list (list 97 95) (list 100 90) (list 99 87) (list 100 80) (list 102 82) (list 4 7) (list 0 69) (list 99 89)) 10) 6 0.001)
    (check-within (candidate (list (list 22 68) (list 75 70) (list 67 78)) 95) 2 0.001)
    (check-within (candidate (list (list 36 33) (list 73 78) (list 41 27) (list 58 34) (list 10 67)) 80) 1 0.001)
    (check-within (candidate (list (list 2 37) (list 39 2) (list 12 57) (list 33 38) (list 73 36) (list 85 22) (list 9 95) (list 31 64) (list 22 3)) 76) 4 0.001)
    (check-within (candidate (list (list 44 0) (list 95 53) (list 37 6) (list 40 4) (list 5 73) (list 33 2) (list 16 71) (list 36 8) (list 87 50) (list 31 71) (list 83 57) (list 4 31) (list 35 79) (list 12 70) (list 93 55) (list 21 77) (list 97 9) (list 95 53) (list 10 73) (list 78 100) (list 22 48) (list 87 50) (list 74 64)) 15) 17 0.001)
    (check-within (candidate (list (list 16 39) (list 17 57) (list 14 38) (list 22 62) (list 69 40) (list 2 53) (list 23 63) (list 20 35) (list 25 49)) 31) 15 0.001)
    (check-within (candidate (list (list 0 46) (list 13 69) (list 38 80) (list 60 17) (list 72 83) (list 27 78) (list 21 9) (list 9 29) (list 84 39) (list 59 117) (list 79 65) (list 1 116) (list 90 71) (list 53 91) (list 46 3) (list 100 73) (list 105 23) (list 12 81) (list 113 84) (list 111 25) (list 27 1) (list 48 49) (list 51 53) (list 93 83) (list 48 29) (list 27 21) (list 9 71)) 91) 19 0.001)
    (check-within (candidate (list (list 50 93) (list 12 98) (list 26 22) (list 50 19) (list 20 70) (list 53 119) (list 1 127) (list 38 100) (list 52 116) (list 89 71) (list 9 98) (list 34 94) (list 12 98) (list 29 119) (list 60 29) (list 97 81) (list 102 84) (list 13 15) (list 10 28) (list 40 26) (list 16 87) (list 45 83) (list 55 83) (list 62 35) (list 30 94) (list 7 75) (list 14 86) (list 16 12) (list 73 88) (list 60 124)) 78) 26 0.001)
    (check-within (candidate (list (list 19 26) (list 2 28) (list 3 10) (list 42 61) (list 56 56)) 23) 3 0.001)
    (check-within (candidate (list (list 56 55) (list 42 83) (list 35 97) (list 28 32) (list 52 76) (list 34 20) (list 68 88) (list 90 38) (list 99 76) (list 32 20) (list 22 85) (list 50 34) (list 4 11) (list 17 92) (list 59 80) (list 66 65) (list 47 60)) 59) 6 0.001)
    (check-within (candidate (list (list 87 78) (list 72 88) (list 82 69) (list 88 79) (list 36 24) (list 42 15) (list 66 94) (list 32 10) (list 92 71) (list 46 89) (list 74 86) (list 37 23) (list 61 44) (list 66 87) (list 35 17) (list 91 78) (list 43 15) (list 61 75) (list 62 70) (list 61 70) (list 34 7) (list 85 64) (list 35 20) (list 42 22) (list 41 27) (list 82 85) (list 90 89) (list 41 13)) 16) 20 0.001)
    (check-within (candidate (list (list 48 86) (list 98 33) (list 46 68) (list 91 21) (list 39 73)) 22) 1 0.001)
    (check-within (candidate (list (list 71 47) (list 68 44) (list 65 45) (list 97 43) (list 97 45) (list 97 45) (list 71 41) (list 103 43) (list 96 20) (list 99 41) (list 57 4) (list 17 77) (list 68 44) (list 16 72) (list 17 75) (list 64 69) (list 19 75) (list 99 41) (list 2 21) (list 71 47) (list 91 4) (list 57 2)) 6) 21 0.001)
    (check-within (candidate (list (list 5 11) (list 16 87) (list 48 55) (list 26 15) (list 41 58) (list 12 14) (list 81 66) (list 30 5)) 14) 2 0.001)
    (check-within (candidate (list (list 85 89) (list 119 89) (list 34 16) (list 54 41) (list 55 29) (list 33 34) (list 54 30) (list 80 74) (list 12 92) (list 42 49) (list 69 7) (list 47 13) (list 26 38) (list 39 96) (list 61 58) (list 24 48) (list 46 47)) 34) 6 0.001)
    (check-within (candidate (list (list 35 45) (list 58 17) (list 64 60) (list 117 23) (list 18 63) (list 26 55) (list 65 54)) 85) 4 0.001)
    (check-within (candidate (list (list 60 1) (list 57 6) (list 39 3) (list 58 7) (list 61 14) (list 19 80) (list 46 0) (list 84 35) (list 43 3) (list 46 4) (list 48 71) (list 48 75) (list 85 40) (list 46 45) (list 6 20) (list 35 7) (list 57 6) (list 51 78) (list 68 25) (list 17 0)) 12) 12 0.001)
    (check-within (candidate (list (list 95 0) (list 36 24) (list 68 27) (list 80 14) (list 39 2) (list 93 52) (list 107 52) (list 86 63) (list 82 13) (list 55 14) (list 8 52) (list 99 20) (list 101 36) (list 50 70) (list 26 98) (list 95 41)) 54) 8 0.001)
    (check-within (candidate (list (list 43 14) (list 55 83) (list 33 89) (list 44 74) (list 46 84) (list 51 87) (list 61 69) (list 1 89)) 32) 10 0.001)
    (check-within (candidate (list (list 88 15) (list 93 65) (list 52 39) (list 20 24) (list 100 36) (list 39 17) (list 26 77) (list 52 39) (list 47 83) (list 98 99) (list 43 28) (list 72 29) (list 21 48) (list 43 32) (list 60 108) (list 44 47) (list 45 125) (list 84 94)) 83) 13 0.001)
    (check-within (candidate (list (list 12 2) (list 43 87) (list 21 100) (list 79 63) (list 5 6) (list 70 75) (list 20 55) (list 23 55) (list 17 31) (list 121 89) (list 27 71) (list 27 22) (list 42 34) (list 15 14) (list 16 40) (list 49 68) (list 30 48) (list 45 43) (list 88 23) (list 47 15) (list 16 41) (list 8 5)) 81) 8 0.001)
))

(test-humaneval)