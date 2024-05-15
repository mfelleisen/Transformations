#lang racket

(require "../testing-2.rkt")

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai8

(define (peaks-ai8 mountain)
  ;; This function takes a list of integers representing mountain heights and returns a list of indices
  ;; of all peak elements. A peak is defined as an element that is strictly greater than its neighboring elements.
  ;; The first and last elements are not considered for peaks.
  
  ;; Use `map` to create a list of indices that are peaks, then filter out #f (false) values.
  (filter-map
   (lambda (i)
     ;; Check if the current element is a peak by comparing it with its neighbors.
     (and (> i 0) (< i (- (length mountain) 1)) ; Ensure index is not the first or last
          (> (list-ref mountain i) (list-ref mountain (- i 1))) ; Current element > Previous element
          (> (list-ref mountain i) (list-ref mountain (+ i 1))) ; Current element > Next element
          i)) ; Return the index if it's a peak
   (range (length mountain))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai7 

(define (peaks-ai7 mountain)
  ;; Using `map` and `filter` to find indices of peaks in the mountain list
  (filter-map
    ;; Filter-map combines filtering and mapping in one step
    ;; Here, we check if the current index `i` is a peak and return the index if true
    (lambda (i)
      (if (and (> (list-ref mountain i) (list-ref mountain (sub1 i)))  ; Check if current element > previous element
               (> (list-ref mountain i) (list-ref mountain (add1 i)))) ; Check if current element > next element
          i  ; Return the index if it's a peak
          #f))  ; Return #f (false) to filter out non-peaks
    ;; Generate a list of indices from 1 to (- (length mountain) 2) to avoid first and last elements
    (range 1 (- (length mountain) 1))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai6

(define (peaks-ai6 mountain)
  ;; This function uses a combination of `map`, `filter`, and `range` to find peaks in the list.
  ;; It returns a list of indices where the peaks occur.
  (filter-map
   ;; `filter-map` is used to both filter and map in a single pass.
   ;; It takes a function and a list, applies the function to each element of the list,
   ;; and includes the result in the output list only if the function returns a non-#f value.
   (lambda (i)
     (let ((current (list-ref mountain i))
           (prev (list-ref mountain (- i 1)))
           (next (list-ref mountain (+ i 1))))
       ;; Check if the current element is greater than both its neighbors.
       (and (> current prev) (> current next) i)))
   ;; Generate a range from 1 to (- (length mountain) 1) to avoid the first and last elements.
   (range 1 (- (length mountain) 1))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai5

(define (peaks-ai5 mountain)
  ;; Use the `map` function to iterate over the indices from 1 to (- (length mountain) 2),
  ;; and then filter out the indices where the condition of being a peak is met.
  (filter-map
   (lambda (i) ; lambda function to check if the current element is a peak
     (and (> (list-ref mountain i) (list-ref mountain (- i 1))) ; current > previous
          (> (list-ref mountain i) (list-ref mountain (+ i 1))) ; current > next
          i)) ; return the index if it's a peak
   (range 1 (- (length mountain) 1)))) ; range from 1 to one before the last index


;; ---------------------------------------------------------------------------------------------------
;; MODULE ai4

(define (peaks-ai4 mountain)
  ;; This function finds all the peaks in the given mountain list.
  ;; A peak is an element that is strictly greater than its neighbors.
  ;; The first and last elements are never considered as peaks.
  (define (is-peak i)
    ;; Helper function to determine if the element at index i is a peak.
    (and (> (list-ref mountain i)
            (list-ref mountain (sub1 i)))
         (> (list-ref mountain i)
            (list-ref mountain (add1 i)))))
  
  ;; Generate a list of indices from 1 to second last element index.
  (define indices (range 1 (sub1 (length mountain))))
  
  ;; Filter indices where the element is a peak.
  (filter is-peak indices))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai3

(define (peaks-ai3 mountain)
  ;; Function to find indices of peaks in the mountain list.
  ;; A peak is defined as an element that is strictly greater than its neighboring elements.
  ;; The first and last elements of the array are not considered peaks.
  (define len (length mountain))  ; Store the length of the mountain list
  (if (< len 3)                  ; If there are less than 3 elements, return an empty list
      '()
      (let loop ((i 1)            ; Start from index 1 to avoid the first element
                 (peaks '()))     ; Initialize peaks as an empty list
        (if (< i (- len 1))       ; Loop until the second last element
            (if (and (> (list-ref mountain i) (list-ref mountain (- i 1)))  ; Check if current element is greater than the previous
                     (> (list-ref mountain i) (list-ref mountain (+ i 1)))) ; and greater than the next
                (loop (+ i 1) (cons i peaks))  ; If it's a peak, add index to peaks and continue
                (loop (+ i 1) peaks))          ; Otherwise, just continue
            (reverse peaks)))))  ; Return the list of peaks, reversed to maintain order of discovery

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai2

(define (peaks-ai2 mountain)
  ;; Define a helper function to check if an element at a given index is a peak
  (define (is-peak? i)
    (and (> (list-ref mountain i) (list-ref mountain (sub1 i)))  ;; Check if current is greater than previous
         (> (list-ref mountain i) (list-ref mountain (add1 i))))) ;; Check if current is greater than next

  ;; Generate a list of indices from 1 to (- (length mountain) 2) and filter those that are peaks
  (filter is-peak? (range 1 (- (length mountain) 1))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai1

(define (peaks-ai1 mountain)
  ;; This function finds all the peak indices in the given list `mountain`.
  ;; A peak is defined as an element that is strictly greater than its neighbors.
  ;; The first and last elements are not considered as potential peaks.
  (define len (length mountain))
  (if (< len 3) ; If there are less than 3 elements, there can't be any internal peaks.
      '() ; Return an empty list
      (let loop ((i 1) ; Start checking from the second element
                 (peaks '())) ; Accumulate peaks in this list
        (if (>= i (- len 1)) ; If we reach the last element, stop.
            (reverse peaks) ; Return the accumulated list of peaks, reversed to maintain order.
            (if (and (> (list-ref mountain i) (list-ref mountain (- i 1))) ; Check if current element is greater than the previous
                     (> (list-ref mountain i) (list-ref mountain (+ i 1)))) ; and greater than the next.
                (loop (+ i 1) (cons i peaks)) ; If it's a peak, add to peaks and continue.
                (loop (+ i 1) peaks)))))) ; Otherwise, just continue.

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai0

(define (peaks-ai0 mountain)
  ;; Using filter-map to identify the indices of peaks
  (define peaks
    (filter-map
     ;; Filter-map combines filtering and mapping in one step
     ;; Here, we use a lambda function to check if the current element is a peak
     (lambda (i)
       (if (and (> (list-ref mountain i) (list-ref mountain (sub1 i)))  ;; Check if current element > previous element
                (> (list-ref mountain i) (list-ref mountain (add1 i)))) ;; Check if current element > next element
           i  ;; If both conditions are true, return the index
           #f)) ;; Otherwise, return false, which will be filtered out
     ;; Generate a list of indices from 1 to (- (length mountain) 2)
     (range 1 (- (length mountain) 1))))
  peaks)  ;; Return the list of peak indices

;; ---------------------------------------------------------------------------------------------------
;; MODULE base-bsl

(define (peaks-base-bsl measurements0) ;; contract  peaks/c
  #; {Real Real Real -> Boolean}
  (define (peak? left x right)
    (and (< left x) (> x right)))

  #; {[Listof Real] Real N -> [Listof N]}
  ;; ACCU `to-the-left` is the item in `measurements0` to the immediate left of `mesasurememts`
  ;; ACCU `index` is the index of the first item on `measurements`
  (define (peaks/accu-base-bsl measurements to-the-left index)
    (cond
      [(empty? (rest measurements)) '()]
      [else
       (define x (first measurements))
       (define to-the-right (second measurements))
       (define remainder (peaks/accu-base-bsl (rest measurements) x (add1 index)))
       (if (peak? to-the-left x to-the-right) (cons index remainder) remainder)]))
    
  (peaks/accu-base-bsl (rest measurements0) (first measurements0) 1))

;; ---------------------------------------------------------------------------------------------------
;; MODULE functional

(define (peaks-HIGH measurements0) ;; contract  peaks/c
  #; {Real Real Real -> Boolean}
  (define (peak? left x right)
    (and (< left x) (> x right)))
  
  (for/fold ([r '()] [left (first measurements0)] [x (second measurements0)] #:result (reverse r))
            ([right (rest (rest measurements0))] [i (in-naturals)])
    (if (peak? left x right)
        (values (cons (add1 i) r) x right)
        (values r                 x right))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE imperative

(define (peaks-imperative measurements0) ;; contract  peaks/c
  #; {Real Real Real -> Boolean}
  (define (peak? left x right)
    (and (< left x) (> x right)))
  
  (define r     '[])
  (define left  (first measurements0))
  (define x     (second measurements0))
  (for/fold  ([_ 'any] #:result (reverse r)) ([right (rest (rest measurements0))] [i (in-naturals)])
    (when (peak? left x right) (set! r (cons (add1 i) r)))
    (set! left x)
    (set! x    right)))
  
;; ---------------------------------------------------------------------------------------------------
;; MODULE imperative-simplified

(define (peaks-imperative-simplified measurements0) ;; contract  peaks/c
  #; {Real Real Real -> Boolean}
  (define (peak? left x right)
    (and (< left x) (> x right)))
  
  (define left (first measurements0))
  (define x    (second measurements0))
  (for/list ([right (rest (rest measurements0))]
             [i (in-naturals)]
             #:when (begin0 (peak? left x right) (set!-values [left x] (values x right))))
    (+ i 1)))

;; ---------------------------------------------------------------------------------------------------
(test peaks
      in
      ai8 ai7 ai6 ai5 ai4 ai3 ai2 ai1 ai0

      base-bsl HIGH imperative-simplified imperative
      [#:show-graph #true]
      with
      #;
      (check-exn #px"short" (λ () (peaks '[2])))
      (check-equal? (peaks (list 2 4 4)) '[])
      (check-equal? (peaks (list 1 4 3 8 5)) (list 1 3))
      (check-equal? (peaks (list 1 1 1)) '[])
      (check-equal? (peaks (list 1 1 3)) '[])
      (check-equal? (peaks (list 1 1 5)) '[])
      (check-equal? (peaks (list 1 2 5)) '[])
      (check-equal? (peaks (list 1 4 1)) (list 1))
      (check-equal? (peaks (list 1 4 3)) (list 1))
      (check-equal? (peaks (list 1 5 5)) '[])
      (check-equal? (peaks (list 1 6 4)) (list 1))
      (check-equal? (peaks (list 2 1 1)) '[])
      (check-equal? (peaks (list 2 1 2)) '[])
      (check-equal? (peaks (list 2 2 3)) '[])
      (check-equal? (peaks (list 2 2 5)) '[])
      (check-equal? (peaks (list 2 3 2)) (list 1))
      (check-equal? (peaks (list 2 3 6)) '[])
      (check-equal? (peaks (list 2 4 3)) (list 1))
      (check-equal? (peaks (list 2 4 5)) '[])
      (check-equal? (peaks (list 2 6 4)) (list 1))
      (check-equal? (peaks (list 3 3 3)) '[])
      (check-equal? (peaks (list 3 3 5)) '[])
      (check-equal? (peaks (list 3 4 6)) '[])
      (check-equal? (peaks (list 3 5 1)) (list 1))
      (check-equal? (peaks (list 3 5 3)) (list 1))
      (check-equal? (peaks (list 3 5 4)) (list 1))
      (check-equal? (peaks (list 3 5 6)) '[])
      (check-equal? (peaks (list 4 2 1)) '[])
      (check-equal? (peaks (list 4 2 2)) '[])
      (check-equal? (peaks (list 4 2 4)) '[])
      (check-equal? (peaks (list 4 2 6)) '[])
      (check-equal? (peaks (list 4 4 1)) '[])
      (check-equal? (peaks (list 4 4 2)) '[])
      (check-equal? (peaks (list 4 4 5)) '[])
      (check-equal? (peaks (list 4 5 4)) (list 1))
      (check-equal? (peaks (list 4 6 1)) (list 1))
      (check-equal? (peaks (list 4 6 6)) '[])
      (check-equal? (peaks (list 5 1 2)) '[])
      (check-equal? (peaks (list 5 2 1)) '[])
      (check-equal? (peaks (list 5 2 2)) '[])
      (check-equal? (peaks (list 5 2 4)) '[])
      (check-equal? (peaks (list 5 3 1)) '[])
      (check-equal? (peaks (list 5 5 1)) '[])
      (check-equal? (peaks (list 5 5 2)) '[])
      (check-equal? (peaks (list 5 5 6)) '[])
      (check-equal? (peaks (list 5 6 1)) (list 1))
      (check-equal? (peaks (list 5 6 4)) (list 1))
      (check-equal? (peaks (list 6 1 1)) '[])
      (check-equal? (peaks (list 6 1 2)) '[])
      (check-equal? (peaks (list 6 1 5)) '[])
      (check-equal? (peaks (list 6 2 2)) '[])
      (check-equal? (peaks (list 6 2 5)) '[])
      (check-equal? (peaks (list 6 3 2)) '[])
      (check-equal? (peaks (list 6 3 3)) '[])
      (check-equal? (peaks (list 6 3 6)) '[])
      (check-equal? (peaks (list 6 4 3)) '[])
      (check-equal? (peaks (list 6 5 2)) '[])
      (check-equal? (peaks (list 6 5 4)) '[])
      (check-equal? (peaks (list 6 6 4)) '[])
      (check-equal? (peaks (list 1 1 1 4)) '[])
      (check-equal? (peaks (list 1 1 7 7)) '[])
      (check-equal? (peaks (list 1 3 6 5)) (list 2))
      (check-equal? (peaks (list 1 4 7 8)) '[])
      (check-equal? (peaks (list 1 6 6 6)) '[])
      (check-equal? (peaks (list 1 8 1 8)) (list 1))
      (check-equal? (peaks (list 2 2 1 2)) '[])
      (check-equal? (peaks (list 2 3 7 6)) (list 2))
      (check-equal? (peaks (list 2 5 4 5)) (list 1))
      (check-equal? (peaks (list 2 7 1 2)) (list 1))
      (check-equal? (peaks (list 2 7 2 6)) (list 1))
      (check-equal? (peaks (list 2 7 5 3)) (list 1))
      (check-equal? (peaks (list 2 7 7 6)) '[])
      (check-equal? (peaks (list 3 1 2 5)) '[])
      (check-equal? (peaks (list 3 3 4 2)) (list 2))
      (check-equal? (peaks (list 3 3 7 8)) '[])
      (check-equal? (peaks (list 3 4 2 4)) (list 1))
      (check-equal? (peaks (list 3 4 5 4)) (list 2))
      (check-equal? (peaks (list 3 4 7 6)) (list 2))
      (check-equal? (peaks (list 3 5 5 3)) '[])
      (check-equal? (peaks (list 3 6 4 7)) (list 1))
      (check-equal? (peaks (list 3 8 5 5)) (list 1))
      (check-equal? (peaks (list 4 2 4 3)) (list 2))
      (check-equal? (peaks (list 4 2 6 8)) '[])
      (check-equal? (peaks (list 4 3 3 8)) '[])
      (check-equal? (peaks (list 4 4 8 7)) (list 2))
      (check-equal? (peaks (list 4 5 1 1)) (list 1))
      (check-equal? (peaks (list 4 6 1 7)) (list 1))
      (check-equal? (peaks (list 4 6 2 1)) (list 1))
      (check-equal? (peaks (list 4 6 2 2)) (list 1))
      (check-equal? (peaks (list 5 1 7 6)) (list 2))
      (check-equal? (peaks (list 5 3 2 2)) '[])
      (check-equal? (peaks (list 5 3 6 3)) (list 2))
      (check-equal? (peaks (list 5 3 8 3)) (list 2))
      (check-equal? (peaks (list 5 4 4 6)) '[])
      (check-equal? (peaks (list 5 4 4 8)) '[])
      (check-equal? (peaks (list 5 5 1 7)) '[])
      (check-equal? (peaks (list 5 5 8 2)) (list 2))
      (check-equal? (peaks (list 5 6 7 4)) (list 2))
      (check-equal? (peaks (list 5 7 4 3)) (list 1))
      (check-equal? (peaks (list 5 8 7 8)) (list 1))
      (check-equal? (peaks (list 6 2 8 6)) (list 2))

      )
