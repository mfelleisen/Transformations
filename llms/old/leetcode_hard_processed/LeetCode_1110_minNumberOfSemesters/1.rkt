#lang racket

;; You are given an integer n, which indicates that there are n courses labeled from 1 to n. You are also given a list relations where relations[i] = [prevCoursei, nextCoursei], representing a prerequisite relationship between course prevCoursei and course nextCoursei: course prevCoursei has to be taken before course nextCoursei. Also, you are given the integer k.
;; In one semester, you can take at most k courses as long as you have taken all the prerequisites in the previous semesters for the courses you are taking.
;; Return the minimum number of semesters needed to take all courses. The testcases will be generated such that it is possible to take every course.
;; Example 1:
;; Input: n = 4, relations = [[2,1],[3,1],[1,4]], k = 2
;; Output: 3
;; Explanation: The figure above represents the given graph.
;; In the first semester, you can take courses 2 and 3.
;; In the second semester, you can take course 1.
;; In the third semester, you can take course 4.
;; Example 2:
;; Input: n = 5, relations = [[2,1],[3,1],[4,1],[1,5]], k = 2
;; Output: 4
;; Explanation: The figure above represents the given graph.
;; In the first semester, you can only take courses 2 and 3 since you cannot take more than two per semester.
;; In the second semester, you can take course 4.
;; In the third semester, you can take course 1.
;; In the fourth semester, you can take course 5.
;; Constraints:
;; 1 <= n <= 15
;; 1 <= k <= n
;; 0 <= relations.length <= n * (n-1) / 2
;; relations[i].length == 2
;; 1 <= prevCoursei, nextCoursei <= n
;; prevCoursei != nextCoursei
;; All the pairs [prevCoursei, nextCoursei] are unique.
;; The given graph is a directed acyclic graph.
(define (minNumberOfSemesters n relations k)
  ;; Initialize the indegree list and the adjacency list for the graph representation
  (define indegree (make-vector (+ n 1) 0))
  (define graph (make-vector (+ n 1) '()))
  
  ;; Populate the graph and indegree list based on relations
  (for-each (lambda (relation)
              (let ([prev (car relation)]
                    [next (cadr relation)])
                (vector-set! graph prev (cons next (vector-ref graph prev)))
                (vector-set! indegree next (+ 1 (vector-ref indegree next)))))
            relations)
  
  ;; Initialize semester counter
  (define semesters 0)
  
  ;; Loop until all courses are taken
  (let loop ((remaining n))
    (when (> remaining 0)
      ;; Find courses with zero indegree
      (define zero-indegree
        (let loop ([i 1] [acc '()])
          (if (> i n)
              acc
              (if (= (vector-ref indegree i) 0)
                  (loop (+ i 1) (cons i acc))
                  (loop (+ i 1) acc)))))
      
      ;; Take up to k courses with zero indegree
      (for-each (lambda (course)
                  (when (< (length zero-indegree) k)
                    (set! zero-indegree (cons course zero-indegree)))
                  (set! remaining (- remaining 1))
                  (vector-set! indegree course -1) ; Mark course as taken
                  ;; Decrease indegree of next courses
                  (for-each (lambda (next-course)
                              (vector-set! indegree next-course (- (vector-ref indegree next-course) 1)))
                            (vector-ref graph course)))
                zero-indegree)
      
      ;; Increment semester count
      (set! semesters (+ semesters 1))
      
      ;; Recursively continue with remaining courses
      (loop remaining)))
  
  ;; Return the total number of semesters needed
  semesters)

;; Example usage:
(define n 4)
(define relations '((2 1) (3 1) (1 4)))
(define k 2)

(minNumberOfSemesters n relations k) ; Returns 3
(require rackunit)

(define (test-humaneval) 

  (let (( candidate minNumberOfSemesters))
    (check-within (candidate 6 (list (list 6 5) (list 5 4) (list 4 3) (list 3 2) (list 2 1)) 1) 6 0.001)
    (check-within (candidate 2 (list (list 1 2)) 1) 2 0.001)
    (check-within (candidate 1 (list ) 2) 1 0.001)
    (check-within (candidate 10 (list (list 2 1) (list 3 2) (list 4 3) (list 5 4) (list 6 5) (list 7 6) (list 8 7) (list 9 8) (list 10 9)) 1) 10 0.001)
    (check-within (candidate 1 (list ) 1) 1 0.001)
    (check-within (candidate 3 (list (list 3 2) (list 2 1)) 1) 3 0.001)
    (check-within (candidate 4 (list (list 2 1) (list 3 1) (list 1 4)) 2) 3 0.001)
    (check-within (candidate 5 (list (list 1 2) (list 3 4) (list 2 5) (list 4 5)) 2) 3 0.001)
))

(test-humaneval)