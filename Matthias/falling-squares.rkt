#lang racket

(require "../testing.rkt")

(module general racket ;; constraints collected from problem statememt 
  (provide WIDTH SIDE Xs fs/c)

  (define WIDTH 1000)
  (define SIDE   106)
  (define Xs     108)

  (define lops/c
    (and/c (listof (and/c [list/c (integer-in 1 Xs) (integer-in 1 SIDE)]))
           (flat-named-contract 'short-list (compose (<=/c WIDTH) length))))

  (define fs/c (-> lops/c (listof natural?))))

(def-module module% falling-squares general)

;; ---------------------------------------------------------------------------------------------------
(module% base

  (define from '[])
  (define rationale "functional scene")

  (define/contract (falling-squares l) fs/c
    (for/fold ([scene [plain-scene]] [maxs '()] #:result (reverse maxs)) ([square l])
      (define scene++ (add-square scene square))
      (values scene++ (cons (height scene++) maxs))))

  #; {type Scene = [vector Natural]}
  ;; it records the height of the stack of resting squares at each index
  ;; `(vector-ref scene i)` is the height of the stack 

  #; {-> Scene}
  (define [plain-scene] (make-vector WIDTH 0))

  #; {Scene Square -> Scene}
  ;; a `(list x side)` square increases the height between x-1 and x-1+side 
  (define (add-square scene square)
    (match-define [list x-left side] square)
    (define left  (- x-left 1))
    (define right (+ left side))
    (define height-at (height-between scene left right))
    (add-height-to-all scene left right (+ height-at side)))

  #; {Scene Natural Natural -> Scene}
  ;; the max height of the scene on [left, right)
  (define (height-between scene left right)
    (apply max (for/list ([i (in-range left right 1)]) (vector-ref scene i))))

  #; {Scene Natural Natural Natural+ -> Scene}
  (define (add-height-to-all scene left right nu-height)
    (for/vector ([h (in-vector scene)] [i (in-naturals)])
      (if (or (< i left) (<= right i)) h nu-height)))

  #; {Scene -> Integer}
  (define [height scene] (apply max (vector->list scene))))

;; ---------------------------------------------------------------------------------------------------
(module% accumulator 

  (define from '[[base "accumulate max"]])
  (define rationale "keep track maximum height via srructural accumulator")

  (define/contract (falling-squares l) fs/c
    (for/fold ([scene [plain-scene]] [maxs '()] #:result (reverse maxs)) ([square l])
      (define scene++ (add-square scene square))
      (values scene++ (cons (scene-max scene++) maxs))))

  #; {type Scene = [scene [vector Natural] Natural]}
  ;; it records the height of the stack of resting squares at each index
  #; (vector-ref (scene-heights scene i)) ; is the height of the stack
  #; (scene-max scene) ; is the maximum height of all heights 

  [struct scene [heights max]]

  #; {-> Scene}
  (define [plain-scene] (scene (make-vector WIDTH 0) 0))

  #; {Scene Square -> Scene}
  ;; a `(list x side)` square increases the height between x-1 and x-1+side 
  (define (add-square scene0 square)
    (match-define [scene heights max-height] scene0)
    (match-define [list x-left side] square)
    (define left  (- x-left 1))
    (define right (+ left side))
    (define nu-height (+ side (height-between heights left right)))
    (scene (add-height-to-all heights left right nu-height) (max max-height nu-height)))

  #; {Scene Natural Natural -> Scene}
  ;; the max height of the scene on [left, right)
  (define (height-between heights left right)
    (apply max (for/list ([i (in-range left right 1)]) (vector-ref heights i))))

  #; {Scene Natural Natural Natural+ -> Scene}
  (define (add-height-to-all heights left right nu-height)
    (for/vector ([h (in-vector heights)] [i (in-naturals)])
      (if (or (< i left) (<= right i)) h nu-height))))

;; ---------------------------------------------------------------------------------------------------
(module% stateful-scene 

  (define from '[[base "imperative heights"]])
  (define rationale "notice single-threaded scene; make imperative to avoid duplication")

  (define/contract (falling-squares l) fs/c
    (for/fold ([scene [plain-scene]] [maxs '()] #:result (reverse maxs)) ([square l])
      (define scene++ (add-square scene square))
      (values scene++ (cons (height scene++) maxs))))

  #; {type Scene = [vector Natural]}
  ;; it records the height of the stack of resting squares at each index
  ;; `(vector-ref scene i)` is the height of the stack 

  #; {-> Scene}
  (define [plain-scene] (make-vector WIDTH 0))

  #; {Scene Square -> Scene}
  ;; EFFECT a `(list x side)` square increases the height between x-1 and x-1+side 
  (define (add-square scene square)
    (match-define [list x-left side] square)
    (define left  (- x-left 1))
    (define right (+ left side))
    (define height-at (height-between scene left right))
    (add-height-to-all! scene left right (+ side height-at)))

  #; {Scene Natural Natural -> Scene}
  ;; the max height of the scene on [left, right)
  (define (height-between scene left right)
    (apply max (for/list ([i (in-range left right 1)]) (vector-ref scene i))))

  #; {Scene Natural Natural Natural+ -> Scene}
  (define (add-height-to-all! scene left right nu-height)
    (for ([i (in-range left right 1)])
      (vector-set! scene i nu-height))
    scene)

  #; {Scene -> Integer}
  (define [height scene] (apply max (vector->list scene))))

;; ---------------------------------------------------------------------------------------------------
(module% state-accu 

  (define from '[[stateful-scene "accumulate max"] [accumulator "stateful heights"]])
  (define rationale "notice single-threaded scene; make imperative to avoid duplication")

  (define/contract (falling-squares l) fs/c
    (for/fold ([scene [plain-scene]] [maxs '()] #:result (reverse maxs)) ([square l])
      (define scene++ (add-square scene square))
      (values scene++ (cons (scene-max scene++) maxs))))

  #; {type Scene = [scene [vector Natural] Natural]}
  ;; it records the height of the stack of resting squares at each index
  #; (vector-ref (scene-heights scene i)) ; is the height of the stack
  #; (scene-max scene) ; is the maximum height of all heights 

  [struct scene [heights max]]

  #; {-> Scene}
  (define [plain-scene] (scene (make-vector WIDTH 0) 0))
  
  #; {Scene Square -> Scene}
  ;; EFFECT a `(list x side)` square increases the height between x-1 and x-1+side 
  (define (add-square scene0 square)
    (match-define [scene heights max-height] scene0)
    (match-define [list x-left side] square)
    (define left  (- x-left 1))
    (define right (+ left side))
    (define nu-height (+ side (height-between heights left right)))
    (scene (add-height-to-all! heights left right nu-height) (max max-height nu-height)))
  
  #; {Scene Natural Natural -> Scene}
  ;; the max height of the scene on [left, right)
  (define (height-between scene left right)
    (apply max (for/list ([i (in-range left right 1)]) (vector-ref scene i))))

  #; {Scene Natural Natural Natural+ -> Scene}
  (define (add-height-to-all! scene left right nu-height)
    (for ([i (in-range left right 1)])
      (vector-set! scene i nu-height))
    scene))

;; ---------------------------------------------------------------------------------------------------
(module% object

  (define from '[[state-accu "use class"]])
  (define rationale "turn struct and functions into a class and methods, instantiate once")

  (define/contract (falling-squares l) fs/c
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
          (vector-set! heights i nu-height))))))

;; ---------------------------------------------------------------------------------------------------
;; test them all 

(test falling-squares
      in
      base stateful-scene state-accu accumulator object
      [#:show-graph #true]
      with
      ;; my silly tests:
      (check-equal? (falling-squares '[ [1 106] ]) '(106) "simple test A")
      (check-equal? (falling-squares (build-list 1000 (λ (x) (list 1 1)))) (build-list 1000 add1) "B")

      ;; llms's tests:
      (check-equal? (falling-squares (list (list 1 2) (list 5 3) (list 3 1))) (list 2 3 3) "1")
      (check-equal? (falling-squares (list (list 1 2) (list 2 2) (list 3 2))) (list 2 4 6) "2")
      
      (check-equal? (falling-squares (list (list 1 2) (list 2 2) (list 2 1))) (list 2 4 5) "3")
      ;; corrected from faulty solution in ../llms/
      ;; scene 1:  [2 2]; scene 2: [2 4 4]; scene 3: [2 5 4]
  
      (check-equal? (falling-squares (list (list 1 1))) (list 1) "4")
      (check-equal? (falling-squares (list (list 1 2) (list 2 3) (list 6 1))) (list 2 5 5) "5")
      (check-equal? (falling-squares (list (list 1 2) (list 3 2) (list 5 2))) (list 2 2 2) "6")
      (check-equal? (falling-squares (list (list 1 2) (list 2 2) (list 1 2))) (list 2 4 6) "7")
      ;; corrected from faulty solution in ../llms/
      ;; scene 1: [2 2] scene 2: [2 4] scene 3 [6 4 6]
      ;; 1. ||
      ;; 2.  ||
      ;;    ||
      ;; 3. ||
      ;;     ||
      ;;    ||
      
      (check-exn exn:fail:contract? (λ () (falling-squares (list (list 100 100) (list 200 100))) "8"))
      ;; corrected from faulty solution in ../llms/ -- which created a test that violates constraints
      )

