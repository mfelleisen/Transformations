#lang racket

(require racket/list) ; Required for list manipulation functions

;; There are n different online courses numbered from 1 to n. You are given a list courses where courses[i] = [durationi, lastDayi] indicate that the ith course should be taken continuously for durationi days and must be finished before or on lastDayi.
;; You will start on the 1st day and you cannot take two or more courses simultaneously.
;; Return the maximum number of courses that you can take.
;; Example 1:
;; Input: courses = [[100,200],[200,1300],[1000,1250],[2000,3200]]
;; Output: 3
;; Explanation: 
;; There are totally 4 courses, but you can take 3 courses at most:
;; First, take the 1st course, it costs 100 days so you will finish it on the 100th day, and ready to take the next course on the 101st day.
;; Second, take the 3rd course, it costs 1000 days so you will finish it on the 1100th day, and ready to take the next course on the 1101st day. 
;; Third, take the 2nd course, it costs 200 days so you will finish it on the 1300th day. 
;; The 4th course cannot be taken now, since you will finish it on the 3300th day, which exceeds the closed date.
;; Example 2:
;; Input: courses = [[1,2]]
;; Output: 1
;; Example 3:
;; Input: courses = [[3,2],[4,3]]
;; Output: 0
;; Constraints:
;; 1 <= courses.length <= 104
;; 1 <= durationi, lastDayi <= 104
(define (scheduleCourse courses)
  ;; First, sort the courses based on their last day in ascending order.
  (define sorted-courses (sort courses (lambda (x y) (< (second x) (second y)))))
  (define curr-time 0) ; Tracks the current day count
  (define duration-sum '()) ; A list to keep track of the courses taken
  
  ;; Iterate over each course in the sorted list
  (for-each (lambda (course)
              (define duration (first course))
              (define deadline (second course))
              ;; Add the duration of the current course to the current time
              (set! curr-time (+ curr-time duration))
              ;; Add the duration to the list (using negative values for min-heap behavior)
              (set! duration-sum (cons (- duration) duration-sum))
              ;; If the current time exceeds the deadline
              (when (> curr-time deadline)
                ;; Remove the course with the largest duration to minimize the exceedance
                (define max-duration (apply max duration-sum))
                ;; Adjust the current time by removing the longest course duration
                (set! curr-time (- curr-time (- max-duration)))
                ;; Remove the longest course duration from the list
                (set! duration-sum (remove max-duration duration-sum))))
            sorted-courses)
  
  ;; The length of the duration-sum list represents the maximum number of courses that can be taken
  (length duration-sum))

;; Example test
(scheduleCourse '((100 200) (200 1300) (1000 1250) (2000 3200)))
(require rackunit)

(define (test-humaneval) 

  (let (( candidate scheduleCourse))
    (check-within (candidate (list (list 100 200) (list 200 1300) (list 1000 1250) (list 2000 3200))) 3 0.001)
    (check-within (candidate (list (list 3 2) (list 4 3))) 0 0.001)
    (check-within (candidate (list (list 5 3) (list 4 2) (list 3 1) (list 2 4) (list 1 5))) 2 0.001)
    (check-within (candidate (list (list 1 2))) 1 0.001)
))

(test-humaneval)