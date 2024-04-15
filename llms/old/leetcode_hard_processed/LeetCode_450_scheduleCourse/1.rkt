#lang racket

;; Define a function to schedule courses based on their duration and last day
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
  ;; Sort the courses by their last day
  (define sorted-courses (sort courses (lambda (x y) (< (second x) (second y)))))
  ;; Initialize current time and a max-heap for course durations
  (define curr-time 0)
  (define duration-heap '())
  
  ;; Define a helper function to insert into the max-heap
  ;; Since Racket does not have a built-in heap, we simulate it with a sorted list
  (define (heap-push heap val)
    (sort (cons (- val) heap) >))
  
  ;; Define a helper function to pop from the max-heap
  (define (heap-pop heap)
    (if (null? heap)
        (values '() 0)
        (values (cdr heap) (- (car heap)))))
  
  ;; Iterate over each course
  (for ([course sorted-courses])
    ;; Update the current time with the duration of the current course
    (set! curr-time (+ curr-time (first course)))
    ;; Push the course duration into the max-heap
    (set! duration-heap (heap-push duration-heap (first course)))
    
    ;; If the current time exceeds the course's last day,
    ;; pop the longest course duration from the max-heap and adjust the current time
    (when (> curr-time (second course))
      (define-values (new-heap popped-duration) (heap-pop duration-heap))
      (set! curr-time (- curr-time popped-duration))
      (set! duration-heap new-heap)))
  
  ;; The maximum number of courses taken is the size of the duration-heap
  (length duration-heap))

;; Example usage:
;; (scheduleCourse '(([100 200] [200 1300] [1000 1250] [2000 3200])))
;; Should return 3
(require rackunit)

(define (test-humaneval) 

  (let (( candidate scheduleCourse))
    (check-within (candidate (list (list 100 200) (list 200 1300) (list 1000 1250) (list 2000 3200))) 3 0.001)
    (check-within (candidate (list (list 3 2) (list 4 3))) 0 0.001)
    (check-within (candidate (list (list 5 3) (list 4 2) (list 3 1) (list 2 4) (list 1 5))) 2 0.001)
    (check-within (candidate (list (list 1 2))) 1 0.001)
))

(test-humaneval)