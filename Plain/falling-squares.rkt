#lang racket

(require "../testing-2.rkt")

;; ---------------------------------------------------------------------------------------------------
  (define WIDTH 1000)
  (define SIDE   106)
  (define Xs     108)
  
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


(define (falling-squares-object l) ;; contract  fs/c
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
;; test them all 

(test falling-squares
      in
      base inline c stateful-scene state-accu accumulator object
      [#:show-graph #true]
      with
      ;; my silly tests:
      (check-equal? (falling-squares '[ [1 106] ]) '(106) "simple test A")
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

