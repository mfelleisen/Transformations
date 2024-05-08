#lang racket

(require "../testing-2.rkt")

;; ---------------------------------------------------------------------------------------------------
  (define LENGTH 1000)
  (define LIMIT  109)
  (define TARGET (* 2 LIMIT))

;; ---------------------------------------------------------------------------------------------------
;; MODULE backwards-base
  
(define (max-jump-backwards-base l0 target) ;; contract  max-jump-backwards-base/c
    (define okay? (good?-backwards-base? target))
    (first (max-jump-backwards-base-aux-backwards-base l0 okay?)))

  #; {[Listof Real] [Real -> Boolean] -> [Listof (U False N)]}
  ;; compute the list of maximal jumps from `l0` to its end 
(define (max-jump-backwards-base-aux-backwards-base l0 okay?)
    (match l0
      [(list _) '[0]]
      [(cons F R)
       (define M (max-jump-backwards-base-aux-backwards-base R okay?))
       (define best (find-max-for-backwards-base F R M okay?))
       (cons best M)]))

  #; {Real [Listof Real] [Listof N] [Real Real -> Boolean] -> (U False N)}
(define (find-max-for-backwards-base one R M okay?)
    (for*/first ([step (in-range 0 (length R))]
                 [x (in-value (and (okay? (list-ref R step) one) (list-ref M step)))] #:when x)
      (+ 1 x)))

  #; {[Real -> Boolean] -> Real Real -> Boolean}
(define ((good?-backwards-base? target) num@j num@i)
    (<= (abs (- num@j num@i)) target))

;; ---------------------------------------------------------------------------------------------------
;; MODULE no-listref
  
(define (max-jump-no-listref l0 target) ;; contract  max-jump-no-listref/c
    (define okay? (good?-no-listref? target))
    (first (max-jump-no-listref-aux-no-listref l0 okay?)))

  #; {[NEListof Real] [Real -> Boolean] -> [Listof (U False N)]}
  ;; compute the list of maximal jumps from `l0` to its end 
(define (max-jump-no-listref-aux-no-listref l0 okay?)
    (match l0
      [(list _) '[0]]
      [(cons F R)
       (define M (max-jump-no-listref-aux-no-listref R okay?))
       (define best (find-max-for-no-listref F R M okay?))
       (cons best M)]))

  #; {Real [Listof Real] [Listof N] [Real Real -> Boolean] -> (U False N)}
(define (find-max-for-no-listref one R M okay?)
    (for/first ([nxt (in-list R)] [steps (in-list M)] #:when (and steps (okay? nxt one)))
      (+ 1 steps)))
  
  #; {[Real -> Boolean] -> Real Real -> Boolean}
(define ((good?-no-listref? target) num@j num@i)
    (<= (abs (- num@j num@i)) target))

;; ---------------------------------------------------------------------------------------------------
;; MODULE let-loop
  
(define (max-jump-let-loop l0 target) ;; contract  max-jump-let-loop/c
    (define okay? (good?-let-loop? target))
    (first (max-jump-let-loop-aux-let-loop l0 okay?)))

  #; {[NEListof Real] [Real -> Boolean] -> [Listof (U False N)]}
  ;; compute the list of maximal jumps from `l0` to its end 
(define (max-jump-let-loop-aux-let-loop l0 okay?)
    (let max-jump-let-loop-aux-let-loop ([l l0])
      (match l
        [(list _) '[0]]
        [(cons F R)
         (define M (max-jump-let-loop-aux-let-loop R))
         (define best (find-max-for-let-loop F R M okay?))
         (cons best M)])))

  #; {Real [Listof Real] [Listof N] [Real Real -> Boolean] -> (U False N)}
(define (find-max-for-let-loop one R M okay?)
    (for/first ([nxt (in-list R)] [steps (in-list M)] #:when (and steps (okay? nxt one)))
      (+ 1 steps)))
  
  #; {[Real -> Boolean] -> Real Real -> Boolean}
(define ((good?-let-loop? target) num@j num@i)
    (<= (abs (- num@j num@i)) target))

;; ---------------------------------------------------------------------------------------------------
;; MODULE inline
  ;; turns out: accumulator brings nothing; no associativity law recognizable 
  
(define (max-jump-inline l0 target) ;; contract  max-jump-inline/c
(define (okay?-inline? num@j num@i) (<= (abs (- num@j num@i)) target))
    (define best*
      (let max-jump-inline-aux ([l l0])
        (match l
          [(list _) '[0]]
          [(cons F R)
           (define M (max-jump-inline-aux R))
           #;
           (define best (ormap (λ (nxt from-nxt) (and from-nxt (okay?-inline? nxt F) (+ 1 from-nxt))) R M))
           (define best
             (for/first ([nxt (in-list R)] [steps (in-list M)] #:when (and steps (okay?-inline? nxt F)))
               (+ 1 steps)))
           (cons best M)])))
    (first best*))

;; ---------------------------------------------------------------------------------------------------
;; MODULE forwards-base
  
(define (max-jump-forwards-base l0 target) ;; contract  max-jump-forwards-base/c
    (define N (length l0))
    (define dist-to-0 (apply vector (cons 0 (make-list (- N 1) (- N)))))

    (for ([x (in-list l0)] [l (in-suffix l0)] [i (in-naturals)])
      (for ([y (in-list (rest l))] [j (in-naturals (add1 i))] #:when (<= (abs (- y x)) target))
        (vector-set! dist-to-0 j (max (vector-ref dist-to-0 j) (add1 (vector-ref dist-to-0 i))))))

    (define r (vector-ref dist-to-0 (sub1 N)))
    (and (>= r 0) r))

;; ---------------------------------------------------------------------------------------------------
;; MODULE forwards-base-2

(define (max-jump-forwards-base-2 l0 target) ;; contract max-jump-forwards-base-2/c
    (struct node [weight {distance-to-0 #:mutable}] #:transparent)
    (define N (length l0))
    (define l (cons (node (first l0) 0) (map (λ (x) (node x (- N))) (rest l0))))

    (for ([x (in-list l)] [k (in-suffix l)])
      (for ([y (in-list (rest k))] #:when (<= (abs (- (node-weight y) (node-weight x))) target))
        (set-node-distance-to-0! y (max (node-distance-to-0 y) (add1 (node-distance-to-0 x))))))
    (define r (node-distance-to-0 (last l)))
    (and (>= r 0) r))

;; ---------------------------------------------------------------------------------------------------
;; MODULE graph

(struct node [name #; string color #;color-string])
(struct edge [from #; string label #;string to #;string])

(define (max-jump-graph l0 target) ;; contract  max-jump-graph/c
    (define G (graph-graph l0 target))

    #;
    (define-values [nodes edges] (graph-graph->nodes+edges G))
    #;
    (eprintf "~a\n" (draw-graph-graph nodes edges))
    
    (search-graph G 0 (sub1 (length l0))))

  #; {Graph -> (values [Listof Node] [Listof Edge])}
(define (graph-graph->nodes+edges G)
    (define nodes
      (for/list ([x (in-range (vector-length G))])
        (node (~a x) "red")))
    (define edges
      (for/fold ([r '()]) ([row G] [from (in-naturals)])
        (append r (map (λ (to) (edge (~a from) (~a to) "")) row))))
    (values nodes edges))

  #; {type [Graph n] = <[k l ... m] ... []> || where k < l < ... < m}

  #; {[Graph n] 0 n -> (U False N)}
(define (search-graph G from0 to)
    (let search-graph ([from from0])
      (cond
        [(= from to) 0]
        [(ormap search-graph (vector-ref G from)) => add1]
        [else #false])))
       
  #; {l : [Listof Real] Real -> [Graph (sub1 (length l))]}
(define (graph-graph l0 target)
    (for/vector ([x (in-list l0)] [l (in-suffix l0)] [i (in-naturals)])
      (for/fold ([r '()] #:result (reverse r)) ([y (in-list (rest l))] [j (in-naturals (add1 i))])
        (if (<= (abs (- y x)) target)
            (cons j r)
            r))))

;; ---------------------------------------------------------------------------------------------------
(test max-jump
      in
      graph

      backwards-base no-listref let-loop inline
      forwards-base forwards-base-2
      [
      #:show-graph #true
       ; #:measure 10000
       ]
      with

      (define LIMIT  109)
      (define osc1 (build-list LIMIT add1))
      (define osc2 (build-list LIMIT (λ (x) (- LIMIT x))))
      (define osc3 (build-list LIMIT (λ (x) (- (add1 x)))))
      (define osc4 (build-list LIMIT (λ (x) (- x LIMIT))))
      (define osc  (append osc1 osc2 osc3 osc4))
      
      (check-equal? (max-jump osc 2) (sub1 (* 4 LIMIT)))

      (check-equal? (max-jump (list 1 3 6 4 1 2) 3) 5)
      (check-equal? (max-jump (list 1 3 6 4 1 2) 0) #false)
      (check-equal? (max-jump (list 0 1) 0) #false)
      (check-equal? (max-jump (list 0 1) 1) 1)
      (check-equal? (max-jump (list 0 1) 2) 1)
      (check-equal? (max-jump (list 1 0) 0) #false)
      (check-equal? (max-jump (list 1 0) 1) 1)
      (check-equal? (max-jump (list 1 0) 2) 1)
      (check-equal? (max-jump (list 0 1 2) 0) #false)
      (check-equal? (max-jump (list 0 1 2) 1) 2)
      (check-equal? (max-jump (list 0 1 2) 2) 2)
      (check-equal? (max-jump (list 0 1 2) 3) 2)
      (check-equal? (max-jump (list 0 2 1) 0) #false)
      (check-equal? (max-jump (list 0 2 1) 1) 1)
      (check-equal? (max-jump (list 0 2 1) 2) 2)
      (check-equal? (max-jump (list 0 2 1) 3) 2)
      (check-equal? (max-jump (list 1 0 2) 0) #false)
      (check-equal? (max-jump (list 1 0 2) 1) 1)
      (check-equal? (max-jump (list 1 0 2) 2) 2)
      (check-equal? (max-jump (list 1 0 2) 3) 2)
      (check-equal? (max-jump (list 1 2 0) 0) #false)
      (check-equal? (max-jump (list 1 2 0) 1) 1)
      (check-equal? (max-jump (list 1 2 0) 2) 2)
      (check-equal? (max-jump (list 1 2 0) 3) 2)
      (check-equal? (max-jump (list 2 0 1) 0) #false)
      (check-equal? (max-jump (list 2 0 1) 1) 1)
      (check-equal? (max-jump (list 2 0 1) 2) 2)
      (check-equal? (max-jump (list 2 0 1) 3) 2)
      (check-equal? (max-jump (list 2 1 0) 0) #false)
      (check-equal? (max-jump (list 2 1 0) 1) 2)
      (check-equal? (max-jump (list 2 1 0) 2) 2)
      (check-equal? (max-jump (list 2 1 0) 3) 2)
      (check-equal? (max-jump (list 0 1 2 3) 0) #false)
      (check-equal? (max-jump (list 0 1 2 3) 1) 3)
      (check-equal? (max-jump (list 0 1 2 3) 2) 3)
      (check-equal? (max-jump (list 0 1 2 3) 3) 3)
      (check-equal? (max-jump (list 0 1 2 3) 4) 3)
      (check-equal? (max-jump (list 0 1 3 2) 0) #false)
      (check-equal? (max-jump (list 0 1 3 2) 1) 2)
      (check-equal? (max-jump (list 0 1 3 2) 2) 3)
      (check-equal? (max-jump (list 0 1 3 2) 3) 3)
      (check-equal? (max-jump (list 0 1 3 2) 4) 3)
      (check-equal? (max-jump (list 0 2 1 3) 0) #false)
      (check-equal? (max-jump (list 0 2 1 3) 1) #false)
      (check-equal? (max-jump (list 0 2 1 3) 2) 3)
      (check-equal? (max-jump (list 0 2 1 3) 3) 3)
      (check-equal? (max-jump (list 0 2 1 3) 4) 3)
      (check-equal? (max-jump (list 0 2 3 1) 0) #false)
      (check-equal? (max-jump (list 0 2 3 1) 1) 1)
      (check-equal? (max-jump (list 0 2 3 1) 2) 3)
      (check-equal? (max-jump (list 0 2 3 1) 3) 3)
      (check-equal? (max-jump (list 0 2 3 1) 4) 3)
      (check-equal? (max-jump (list 0 3 1 2) 0) #false)
      (check-equal? (max-jump (list 0 3 1 2) 1) 2)
      (check-equal? (max-jump (list 0 3 1 2) 2) 2)
      (check-equal? (max-jump (list 0 3 1 2) 3) 3)
      (check-equal? (max-jump (list 0 3 1 2) 4) 3)
      (check-equal? (max-jump (list 0 3 2 1) 0) #false)
      (check-equal? (max-jump (list 0 3 2 1) 1) 1)
      (check-equal? (max-jump (list 0 3 2 1) 2) 2)
      (check-equal? (max-jump (list 0 3 2 1) 3) 3)
      (check-equal? (max-jump (list 0 3 2 1) 4) 3)
      (check-equal? (max-jump (list 1 0 2 3) 0) #false)
      (check-equal? (max-jump (list 1 0 2 3) 1) 2)
      (check-equal? (max-jump (list 1 0 2 3) 2) 3)
      (check-equal? (max-jump (list 1 0 2 3) 3) 3)
      (check-equal? (max-jump (list 1 0 2 3) 4) 3)
      (check-equal? (max-jump (list 1 0 3 2) 0) #false)
      (check-equal? (max-jump (list 1 0 3 2) 1) 1)
      (check-equal? (max-jump (list 1 0 3 2) 2) 2)
      (check-equal? (max-jump (list 1 0 3 2) 3) 3)
      (check-equal? (max-jump (list 1 0 3 2) 4) 3)
      (check-equal? (max-jump (list 1 2 0 3) 0) #false)
      (check-equal? (max-jump (list 1 2 0 3) 1) 2)
      (check-equal? (max-jump (list 1 2 0 3) 2) 2)
      (check-equal? (max-jump (list 1 2 0 3) 3) 3)
      (check-equal? (max-jump (list 1 2 0 3) 4) 3)
      (check-equal? (max-jump (list 1 2 3 0) 0) #false)
      (check-equal? (max-jump (list 1 2 3 0) 1) 1)
      (check-equal? (max-jump (list 1 2 3 0) 2) 2)
      (check-equal? (max-jump (list 1 2 3 0) 3) 3)
      (check-equal? (max-jump (list 1 2 3 0) 4) 3)
      (check-equal? (max-jump (list 1 3 0 2) 0) #false)
      (check-equal? (max-jump (list 1 3 0 2) 1) 1)
      (check-equal? (max-jump (list 1 3 0 2) 2) 2)
      (check-equal? (max-jump (list 1 3 0 2) 3) 3)
      (check-equal? (max-jump (list 1 3 0 2) 4) 3)
      (check-equal? (max-jump (list 1 3 2 0) 0) #false)
      (check-equal? (max-jump (list 1 3 2 0) 1) 1)
      (check-equal? (max-jump (list 1 3 2 0) 2) 3)
      (check-equal? (max-jump (list 1 3 2 0) 3) 3)
      (check-equal? (max-jump (list 1 3 2 0) 4) 3)
      (check-equal? (max-jump (list 2 0 1 3) 0) #false)
      (check-equal? (max-jump (list 2 0 1 3) 1) 1)
      (check-equal? (max-jump (list 2 0 1 3) 2) 3)
      (check-equal? (max-jump (list 2 0 1 3) 3) 3)
      (check-equal? (max-jump (list 2 0 1 3) 4) 3)
      (check-equal? (max-jump (list 2 0 3 1) 0) #false)
      (check-equal? (max-jump (list 2 0 3 1) 1) 1))
