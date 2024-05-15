#lang racket

(require "../testing-2.rkt")

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai8

(define (falling-squares-ai8 positions)
  (let loop ([positions positions] [intervals '()] [ans '()])
    (if (null? positions)
        (reverse ans) ; Since we're prepending, reverse at the end for correct order
        (let* ([pos (car positions)]
               [L (car pos)] ; Left edge of the current square
               [size (cadr pos)] ; Size of the current square
               [R (+ L size)] ; Right edge of the current square
               [h size]) ; Initial height is the size of the square
          ; Iterate over intervals to find the maximum height at which the current square can land
          (for ([interval intervals])
            (let* ([h2 (car interval)] ; Height of the interval
                   [L2 (cadr interval)] ;; FIX 
                   [R2 (caddr interval)]) ; Right edge of the interval
              #; { working thru the fix
                           situation 1:
                           L       R
                           |------|
                           |----------|
                           L2          R2

                           (and (<= L2 R) (< R R2))

                           situation 2:
                           L          R
                           |---------|
                           |---------|
                           L2         R2

                           (and (<= L2 L) (< L R2))
                           }
              (when (or (and (<= L2 R) (< R R2)) (and (<= L2 L) (< L R2)))
                #;(and (>= L2 L) (<= R2 R)) ; Check if the current square overlaps with this interv
                (set! h (max h (+ size h2))) ; Adjust height accordingly
                )))
          
          ; Find the maximum height among all intervals to update the answer
          (let ([maxHeight (if (null? intervals) 0 (apply max (map car intervals)))])
            (loop (cdr positions)
                  (cons (list h L R) intervals) ; Add the current interval (height and right edge)
                  (cons (max maxHeight h) ans)))))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai7 

(define (falling-squares-ai7 positions)
  ;; Initialize the answer list and intervals list to keep track of the right edge and height
  (let loop ([positions positions] [ans '()] [intervals '()])
    ;; If there are no more positions, return the reversed ans list
    (if (null? positions)
        (reverse ans)
        (let* ([pos (car positions)] ;; Take the first position (left edge and side length)
               [L (car pos)] ;; Extract left edge
               [size (cadr pos)] ;; Extract side length
               [R (+ L size)] ;; Calculate the right edge
               [h size] ;; Initialize the height of the current square
               [maxHeight 0]) ;; Initialize max height
          ;; Loop through each interval to calculate the max height
          (for-each (lambda (interval)
                      (let ([h2 (car interval)] ;; Height of the interval
                            [L2 (cadr interval)] ;; FIX
                            [R2 (caddr interval)]) ;; Right edge of the interval
                        (when (or (and (<= L2 R) (< R R2)) (and (<= L2 L) (< L R2))) ;; FIX
                          #;(and (> R2 L) (< R2 R)) ;; Check if the square lands on the interval
                          (set! h (max h (+ size h2)))) ;; Update height if it lands on an interval
                        ;; Update maxHeight
                        (set! maxHeight (max maxHeight h2))))
                    intervals)
          ;; Append the max of maxHeight and h to ans
          (let ([newAns (cons (max maxHeight h) ans)])
            ;; Add the current square's height and right edge to intervals
            (let ([newIntervals (cons (list h L R) intervals)])
              ;; Recursive call with the rest of the positions
              (loop (cdr positions) newAns newIntervals)))))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai6 

(define (falling-squares-ai6 positions)
  ;; Initialize an empty list for storing the heights after each square is dropped
  (define ans '())
  ;; Initialize an empty list for storing the intervals (height, right edge) of dropped squares
  (define intervals '())
  
  ;; Iterate through each position given in the input
  (for-each (lambda (p)
              ;; Extract the left edge and size of the current square
              (define L (car p))
              (define size (cadr p))
              ;; Calculate the right edge of the current square
              (define R (+ L size))
              ;; Initialize the height of the current square
              (define h size)
              
              ;; Iterate through each interval in intervals to find if the current square
              ;; lands on another square and to adjust its height accordingly
              (for-each (lambda (interval)
                          (define h2 (car interval))
                          [define L2 (cadr interval)] ;; FIX
                          (define R2 (caddr interval))
                          ;; Check if the current square intersects with an existing square
                          ;; and adjust the height if necessary
                          (when (or (and (<= L2 R) (< R R2)) (and (<= L2 L) (< L R2)))
                            #;(and (> R2 L) (> R R2))
                            (set! h (max h (+ size h2)))))
                        intervals)
              
              ;; Calculate the maximum height among all intervals including the current square
              (define maxHeight (if (null? intervals) 0 (apply max (map car intervals))))
              ;; Update the ans list with the maximum height after dropping the current square
              (set! ans (append ans (list (max maxHeight h))))
              ;; Add the current square (height, right edge) to intervals
              (set! intervals (append intervals (list (list h L R)))))
            positions)
  ;; Return the list of heights after each square is dropped
  ans)

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai5

(define (falling-squares-ai5 positions)
  ;; Initialize an empty list to store the results
  (let loop ((positions positions) (ans '()) (intervals '()))
    ;; Base case: if there are no more positions, return the reversed list of answers
    (if (null? positions)
        (reverse ans)
        ;; Process each position
        (let* ((p (car positions)) ;; Current position
               (L (car p)) ;; Left edge of the current square
               (size (cadr p)) ;; Size of the current square
               (R (+ L size)) ;; Right edge of the current square
               ;; Calculate the height where the square will land
               (h (foldl (lambda (interval h)
                           (let ((h2 (car interval)) [L2 (cadr interval)] (R2 (caddr interval)))
                             (if (or (and (<= L2 R) (< R R2)) (and (<= L2 L) (< L R2)))
                                 #; (and (> R2 L) (< R2 R))
                                 (max h (+ size h2))
                                 h)))
                         size
                         intervals))
               ;; Calculate the maximum height so far
               (maxHeight (if (null? intervals) 0
                              (apply max (map car intervals)))))
          ;; Recurse with the rest of the positions, updated answers, and intervals
          (loop (cdr positions)
                (cons (max maxHeight h) ans)
                (cons (list h L R) intervals))))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai4

(define (falling-squares-ai4 positions)
  ;; Initialize the list to keep track of the intervals and their heights
  (define intervals '())
  ;; Initialize the list to store the maximum height after each drop
  (define ans '())
  
  ;; Iterate through each position
  (for-each (lambda (p)
              ;; Extract left and size from the position
              (define L (first p))
              (define size (second p))
              ;; Calculate the right edge of the current square
              (define R (+ L size))
              ;; Initialize the height of the current square
              (define h size)
              
              ;; Check for overlap with other squares to find the maximum height
              (for-each (lambda (interval)
                          (define h2 (first interval))
                          [define L2 (second interval)] ;; FIX
                          (define R2 (third interval))
                          (when (or (and (<= L2 R) (< R R2)) (and (<= L2 L) (< L R2)))
                            #; (and (> R2 L) (< R2 R))
                            (set! h (max h (+ size h2)))))
                        intervals)
              
              ;; Update the maximum height seen so far
              (define maxHeight (if (null? intervals) 0 (apply max (map first intervals))))
              (set! ans (append ans (list (max maxHeight h))))
              
              ;; Add the current square to the list of intervals
              (set! intervals (append intervals (list (list h L R)))))
            positions)
  
  ;; Return the list of maximum heights after each drop
  ans)

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai3 

(define (falling-squares-ai3 positions)
  ;; Create an empty list to store the answers.
  (let loop ([positions positions]
             [intervals '()]
             [ans '()])
    ;; If there are no more positions, return the reversed list of answers.
    (if (null? positions)
        (reverse ans)
        ;; Deconstruct the first position into L (left) and size.
        (let* ([p (car positions)]
               [L (car p)]
               [size (cadr p)]
               [R (+ L size)] ;; Calculate the right edge of the current square.
               ;; Initialize the height of the current square with its own size.
               [h size]
               ;; For each interval, check if it overlaps with the current square.
               ;; If it does, update the height of the current square.
               [h (foldl (lambda (interval h)
                           (let ([h2 (car interval)]
                                 [L2 (cadr interval)]
                                 [R2 (caddr interval)])
                             (if (or (and (<= L2 R) (< R R2)) (and (<= L2 L) (< L R2)))
                                 #; (and (> R2 L) (< R2 R)) ;; Check for overlap.
                                 (max h (+ size h2)) ;; Update the height if there is an overlap.
                                 h)))
                         h
                         intervals)]
               ;; Calculate the max height among all intervals to update the answer.
               [maxHeight (if (null? intervals) 0 (apply max (map car intervals)))]
               [ans (cons (max maxHeight h) ans)] ;; Update the list of answers.
               [intervals (cons (list h L R) intervals)]) ;; Update the list of intervals.
          ;; Recur with the rest of the positions.
          (loop (cdr positions) intervals ans)))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai2

(define (falling-squares-ai2 positions)
  ;; Initialize an empty list to store the result
  (define ans '())
  ;; Initialize an empty list to store intervals with their heights
  (define intervals '())
  
  ;; Loop through each position in the input list
  (for-each (lambda (p)
              ;; Extract left and size from the current position
              (define L (car p))
              (define size (cadr p))
              (define R (+ L size))
              ;; Initialize height of the current square
              (define h size)
              
              ;; Calculate the height of the current square by comparing it with existing intervals
              (for-each (lambda (interval)
                          (define h2 (car interval))
                          [define L2 (cadr interval)] ;; FIX
                          (define R2 (caddr interval))
                          (when (or (and (<= L2 R) (< R R2)) (and (<= L2 L) (< L R2)))
                            #; (and (> R2 L) (> R R2))
                            (set! h (max h (+ size h2)))))
                        intervals)
              
              ;; Calculate the max height so far
              (define maxHeight (if (null? intervals) 0 (apply max (map car intervals))))
              ;; Add the max height after dropping the current square to the ans list
              (set! ans (append ans (list (max maxHeight h))))
              ;; Add the current interval with its height to the intervals list
              (set! intervals (append intervals (list (list h L R)))))
            positions)
  ;; Return the ans list which contains heights after each drop
  ans)

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai1

(define (falling-squares-ai1 positions)
  ;; Initialize the list to store the final heights
  ;; after each drop and the list to store intervals with their heights.
  (let ([ans '()]
        [intervals '()])
    ;; Loop through each position to process the squares being dropped.
    (for-each (lambda (p)
                (let* ([L (first p)] ; Left edge of the current square.
                       [size (second p)] ; Side length of the current square.
                       [R (+ L size)] ; Right edge of the current square.
                       ;; Calculate the height at which the current square will land.
                       [h (foldl (lambda (interval acc)
                                   (let ([h2 (first interval)]
                                         [L2 (second interval)]
                                         [R2 (third interval)])
                                     (if (or (and (<= L2 R) (< R R2)) (and (<= L2 L) (< L R2)))
                                         #;(and (> R2 L) (< R2 R))
                                         (max acc (+ size h2))
                                         acc)))
                                 size
                                 intervals)])
                  ;; Update the maximum height seen so far after dropping this square.
                  (let ([maxHeight (if (null? intervals) 0 (apply max (map first intervals)))])
                    (set! ans (append ans (list (max maxHeight h)))))
                  ;; Add the current square's height and right edge to the intervals list.
                  (set! intervals (append intervals (list (list h L R))))))
              positions)
    ;; Return the list of heights after each square drop.
    ans))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai0 

(define (falling-squares-ai0 positions)
  ;; Create an empty list to store the results (heights after each drop)
  (define ans '())
  ;; Create an empty list to store the intervals and their heights
  (define intervals '())

  ;; Iterate over each position provided
  (for-each (lambda (p)
              ;; Extract the left position and size of the current square
              (define L (car p))
              (define size (cadr p))
              (define R (+ L size))
              ;; Initialize the height of the current square
              (define h size)

              ;; Check each previously placed square to see if the current square lands on it
              (for-each (lambda (interval)
                          (define h2 (car interval))
                          [define L2 (cadr interval)] ;; FIX
                          (define R2 (caddr interval))
                          (when (or (and (<= L2 R) (< R R2)) (and (<= L2 L) (< L R2)))
                            #; (and (> R2 L) (> R R2)) ;; Check for overlap
                            (set! h (max h (+ size h2))))) ;; Update height if landed on
                        intervals)

              ;; Calculate the max height among all intervals and the current square
              (define maxHeight (if (null? intervals) 0 (apply max (map car intervals))))
              (set! ans (append ans (list (max maxHeight h))))
              
              ;; Add the current square to the intervals list
              (set! intervals (append intervals (list (list h L R)))))
            positions)
  ans)

;; ---------------------------------------------------------------------------------------------------
(define WIDTH 1000)
(define SIDE   106)
(define Xs     108)
  
;; ---------------------------------------------------------------------------------------------------
;; MODULE base
(define (falling-squares-base l) ;; contract  fs/c
  (for/fold ([scene (plain-scene-base)] [maxs '()] #:result (reverse maxs)) ([square l])
    (define scene++ (add-square-base scene square))
    (values scene++ (cons (height-base scene++) maxs))))

#; {type Scene = [vector Natural]}
;; it records the height of the stack of resting squares at each index
;; `(vector-ref scene i)` is the height of the stack 

#; {-> Scene}
(define (plain-scene-base) (make-vector WIDTH 0))

#; {Scene Square -> Scene}
;; a `(list x side)` square increases the height between x-1 and x-1+side 
(define (add-square-base scene square)
  (match-define [list x-left side] square)
  (define left  (- x-left 1))
  (define right (+ left side))
  (define height-at (height-between-base scene left right))
  (add-height-to-all-base scene left right (+ height-at side)))

#; {Scene Natural Natural -> Scene}
;; the max height of the scene on [left, right)
(define (height-between-base scene left right)
  (apply max (for/list ([i (in-range left right 1)]) (vector-ref scene i))))

#; {Scene Natural Natural Natural+ -> Scene}
(define (add-height-to-all-base scene left right nu-height)
  (for/vector ([h (in-vector scene)] [i (in-naturals)])
    (if (or (< i left) (<= right i)) h nu-height)))

#; {Scene -> Integer}
(define (height-base scene) (apply max (vector->list scene)))

;; ---------------------------------------------------------------------------------------------------
;; MODULE inline


(define (falling-squares-inline l) ;; contract  fs/c
  #; {type Scene = [vector Natural]}
  ;; it records the height of the stack of resting squares at each index
  ;; `(vector-ref scene i)` is the height of the stack 
  (for/fold ([scene (make-vector WIDTH 0)] [maxs '()] #:result (reverse maxs)) ([square l])
    (match-define [list x-left side] square)
    (define left  (- x-left 1))
    (define right (+ left side))
    (define height-at (apply max (for/list ([i (in-range left right 1)]) (vector-ref scene i))))
    (define nu-height (+ height-at side))
    (define scene++ 
      (for/vector ([h (in-vector scene)] [i (in-naturals)])
        (if (or (< i left) (<= right i)) h nu-height)))
    (define height++ (apply max (vector->list scene++)))
    (values scene++ (cons height++ maxs))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE c


(define (falling-squares-c l) ;; contract  fs/c
  #; {type Scene = [vector Natural]}
  ;; it records the height of the stack of resting squares at each index
  ;; `(vector-ref scene i)` is the height of the stack
  (define scene  (make-vector WIDTH 0))
  (define max-of 0)
  (define the-list '())

  (for ([square l])
    (match-define [list x-left side] square)
    (define left  (- x-left 1))
    (define right (+ left side))
    (define height-at 0)
    (for ([i (in-range left right 1)])
      (set! height-at (max (vector-ref scene i) height-at)))
    (define nu-height (+ height-at side))
    (for ([i (in-range left right 1)])
      (vector-set! scene i nu-height))
    (set! max-of (max nu-height max-of))
    (set! the-list (cons max-of the-list)))

  (reverse the-list))

;; ---------------------------------------------------------------------------------------------------
;; MODULE accumulator 


(define (falling-squares-accumulator  l) ;; contract  fs/c
  (for/fold ([scene (plain-scene-accumulator )] [maxs '()] #:result (reverse maxs)) ([square l])
    (define scene++ (add-square-accumulator  scene square))
    (values scene++ (cons (scene2-max scene++) maxs))))

#; {type Scene = [scene [vector Natural] Natural]}
;; it records the height of the stack of resting squares at each index
#; (vector-ref (scene-heights scene i)) ; is the height of the stack
#; (scene-max scene) ; is the maximum height of all heights 

[struct scene2 [heights max]]

#; {-> Scene}
(define (plain-scene-accumulator ) (scene2 (make-vector WIDTH 0) 0))

#; {Scene Square -> Scene}
;; a `(list x side)` square increases the height between x-1 and x-1+side 
(define (add-square-accumulator  scene0 square)
  (match-define [scene2 heights max-height] scene0)
  (match-define [list x-left side] square)
  (define left  (- x-left 1))
  (define right (+ left side))
  (define nu-height (+ side (height-between-accumulator  heights left right)))
  (scene2 (add-height-to-all-accumulator  heights left right nu-height) (max max-height nu-height)))

#; {Scene Natural Natural -> Scene}
;; the max height of the scene on [left, right)
(define (height-between-accumulator  heights left right)
  (apply max (for/list ([i (in-range left right 1)]) (vector-ref heights i))))

#; {Scene Natural Natural Natural+ -> Scene}
(define (add-height-to-all-accumulator  heights left right nu-height)
  (for/vector ([h (in-vector heights)] [i (in-naturals)])
    (if (or (< i left) (<= right i)) h nu-height)))

;; ---------------------------------------------------------------------------------------------------
;; MODULE stateful-scene 


(define (falling-squares-stateful-scene  l) ;; contract  fs/c
  (for/fold ([scene (plain-scene-stateful-scene )] [maxs '()] #:result (reverse maxs)) ([square l])
    (define scene++ (add-square-stateful-scene  scene square))
    (values scene++ (cons (height-stateful-scene  scene++) maxs))))

#; {type Scene = [vector Natural]}
;; it records the height of the stack of resting squares at each index
;; `(vector-ref scene i)` is the height of the stack 

#; {-> Scene}
(define (plain-scene-stateful-scene ) (make-vector WIDTH 0))

#; {Scene Square -> Scene}
;; EFFECT a `(list x side)` square increases the height between x-1 and x-1+side 
(define (add-square-stateful-scene  scene square)
  (match-define [list x-left side] square)
  (define left  (- x-left 1))
  (define right (+ left side))
  (define height-at (height-between-stateful-scene  scene left right))
  (add-height-to-all!-stateful-scene  scene left right (+ side height-at)))

#; {Scene Natural Natural -> Scene}
;; the max height of the scene on [left, right)
(define (height-between-stateful-scene  scene left right)
  (apply max (for/list ([i (in-range left right 1)]) (vector-ref scene i))))

#; {Scene Natural Natural Natural+ -> Scene}
(define (add-height-to-all!-stateful-scene  scene left right nu-height)
  (for ([i (in-range left right 1)])
    (vector-set! scene i nu-height))
  scene)

#; {Scene -> Integer}
(define (height-stateful-scene  scene) (apply max (vector->list scene)))

;; ---------------------------------------------------------------------------------------------------
;; MODULE state-accu 


(define (falling-squares-state-accu  l) ;; contract  fs/c
  (for/fold ([scene (plain-scene-state-accu )] [maxs '()] #:result (reverse maxs)) ([square l])
    (define scene++ (add-square-state-accu  scene square))
    (values scene++ (cons (scene-max scene++) maxs))))

#; {type Scene = [scene [vector Natural] Natural]}
;; it records the height of the stack of resting squares at each index
#; (vector-ref (scene-heights scene i)) ; is the height of the stack
#; (scene-max scene) ; is the maximum height of all heights 

[struct scene [heights max]]

#; {-> Scene}
(define (plain-scene-state-accu ) (scene (make-vector WIDTH 0) 0))
  
#; {Scene Square -> Scene}
;; EFFECT a `(list x side)` square increases the height between x-1 and x-1+side 
(define (add-square-state-accu  scene0 square)
  (match-define [scene heights max-height] scene0)
  (match-define [list x-left side] square)
  (define left  (- x-left 1))
  (define right (+ left side))
  (define nu-height (+ side (height-between-state-accu  heights left right)))
  (scene (add-height-to-all!-state-accu  heights left right nu-height) (max max-height nu-height)))
  
#; {Scene Natural Natural -> Scene}
;; the max height of the scene on [left, right)
(define (height-between-state-accu  scene left right)
  (apply max (for/list ([i (in-range left right 1)]) (vector-ref scene i))))

#; {Scene Natural Natural Natural+ -> Scene}
(define (add-height-to-all!-state-accu  scene left right nu-height)
  (for ([i (in-range left right 1)])
    (vector-set! scene i nu-height))
  scene)

;; ---------------------------------------------------------------------------------------------------
;; MODULE object


(define (falling-squares-HIGH l) ;; contract  fs/c
  (define scene [new scene%])
  (for/fold ([maxs '()] #:result (reverse maxs)) ([square l])
    (send scene add-square square)
    (cons (get-field max-height scene) maxs)))

#; {type Scene = [instanceof/c Scene% (class/c [add-square ->m void?])]}
  
(define scene%
  (class object%
    (super-new)

    ;; it records the height of the stack of resting squares at each index
    #; (vector-ref heights scene i) ; is the height of the stack at i
    (field [heights    (make-vector WIDTH 0)])

    ;; is the maximum height of all heights     
    (field [max-height 0])
  
    #; {Square -> Void}
    ;; EFFECT a `(list x side)` square increases the height between x-1 and x-1+side 
    (define/public (add-square square)
      (match-define [list x-left side] square)
      (define left  (- x-left 1))
      (define right (+ left side))
      (define nu-height (+ side (height-between left right)))
      (add-height-to-all! left right nu-height)
      (set! max-height (max max-height nu-height)))
  
    #; {Natural Natural -> Scene}
    ;; the max height of the scene on [left, right)
    (define/private (height-between left right)
      (apply max (for/list ([i (in-range left right 1)]) (vector-ref heights i))))

    #; {Natural Natural Natural+ -> Scene}
    (define/private (add-height-to-all! left right nu-height)
      (for ([i (in-range left right 1)])
        (vector-set! heights i nu-height)))))

;; ---------------------------------------------------------------------------------------------------


;; ---------------------------------------------------------------------------------------------------
;; test them all 

(test falling-squares
      in
      ;; AI
      ai8 ai7 ai6 ai5 ai4 ai3 ai2 ai1 ai0
      ;; MINE
      base inline c stateful-scene state-accu accumulator HIGH
      [#:show-graph #true]
      with
      ;; my silly tests:
      (check-equal? (falling-squares '[ [1 106] ]) '(106) "simple test A")

      #;
      (check-equal? (falling-squares (build-list 1000 (λ (x) (list 1 1)))) (build-list 1000 add1) "B")

      ;; llms's tests:
      (check-equal? (falling-squares (list (list 1 2) (list 5 3) (list 3 1))) (list 2 3 3) "1")
      (check-equal? (falling-squares (list (list 1 2) (list 2 2) (list 3 2))) (list 2 4 6) "2")

      ;; corrected from faulty solution in ../llms/
      (check-equal? (falling-squares (list (list 1 2) (list 2 2) (list 2 1))) (list 2 4 5) "3")
      
      ;; scene 1:  [2 2]; scene 2: [2 4 4]; scene 3: [2 5 4]
  
      (check-equal? (falling-squares (list (list 1 1))) (list 1) "4")
      (check-equal? (falling-squares (list (list 1 2) (list 2 3) (list 6 1))) (list 2 5 5) "5")
      (check-equal? (falling-squares (list (list 1 2) (list 3 2) (list 5 2))) (list 2 2 2) "6")

      ;; corrected from faulty solution in ../llms/
      (check-equal? (falling-squares (list (list 1 2) (list 2 2) (list 1 2))) (list 2 4 6) "7")      
      ;; scene 1: [2 2] scene 2: [2 4] scene 3 [6 4 6]
      ;; 1. ||
      ;; 2.  ||
      ;;    ||
      ;; 3. ||
      ;;     ||
      ;;    ||

      ;; corrected from faulty solution in ../llms/ -- which created a test that violates constraints
      #;
      (check-exn exn:fail:contract? (λ () (falling-squares (list (list 100 100) (list 200 100))) "8"))

      )

