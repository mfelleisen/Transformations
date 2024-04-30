#lang racket

(require "../testing.rkt")

(module general racket ;; constraints collected from problem statememt 
  (provide LENGTH LIMIT max-jump/c in-suffix)
  (require "in-suffix.rkt")

  (define LENGTH 1000)
  (define LIMIT  109)
  (define TARGET (* 2 LIMIT))

  (define lon/c
    (and/c (flat-named-contract 'small (listof (integer-in (- LIMIT) (+ LIMIT))))
           (flat-named-contract 'short (compose (integer-in 1 LENGTH) length))))

  ;; You are given a 0-indexed array nums of n integers and an integer target.

  ;; You are initially positioned at index 0. In one step, you can jump from index i to any index j if
  ;;  * 0 <= i < j < n
  ;;  * -target <= nums[j] - nums[i] <= target

  ;; Return the maximum number of jumps you can make to reach index n - 1.
  ;; If there is no way to reach index n - 1, return #false.

  (define max-jump/c (-> lon/c (and/c natural? (integer-in 0 TARGET)) (or/c #false natural?))))

(def-module module% max-jump general)

;; ---------------------------------------------------------------------------------------------------
(module% backwards-base
  (define from '[])
  (define rationale "'graph traversal' via recursion")
  
  (define/contract [max-jump l0 target] max-jump/c
    (define okay? (good? target))
    (first (max-jump-aux l0 okay?)))

  #; {[Listof Real] [Real -> Boolean] -> [Listof (U False N)]}
  ;; compute the list of maximal jumps from `l0` to its end 
  (define (max-jump-aux l0 okay?)
    (cond
      [(empty? (rest l0)) '[0]]
      [else
       (define R (rest l0))
       (define M (max-jump-aux R okay?))
       (define best (find-max-for (first l0) R M okay?))
       (cons best M)]))

  #; {Real [Listof Real] [Listof N] [Real Real -> Boolean] -> (U False N)}
  (define (find-max-for one R M okay?)
    (for*/first ([step (in-range 0 (length R))]
                 [x (in-value (and (okay? (list-ref R step) one) (list-ref M step)))] #:when x)
      (+ 1 x)))

  #; {[Real -> Boolean] -> Real Real -> Boolean}
  (define ((good? target) num@j num@i)
    (<= (- target) (- num@j num@i) (+ target))))

;; ---------------------------------------------------------------------------------------------------
(module% no-listref
  (define from `[[backwards-base ,NO-ALLOC]])
  (define rationale "brute force dynamic programming")
  
  (define/contract [max-jump l0 target] max-jump/c
    (define okay? (good? target))
    (first (max-jump-aux l0 okay?)))

  #; {[NEListof Real] [Real -> Boolean] -> [Listof (U False N)]}
  ;; compute the list of maximal jumps from `l0` to its end 
  (define (max-jump-aux l0 okay?)
    (cond
      [(empty? (rest l0)) '[0]]
      [else
       (define R (rest l0))
       (define M (max-jump-aux R okay?))
       (define best (find-max-for (first l0) R M okay?))
       (cons best M)]))

  #; {Real [Listof Real] [Listof N] [Real Real -> Boolean] -> (U False N)}
  (define (find-max-for one R M okay?)
    (for/first ([nxt R] [max-steps-from-nxt M] #:when (and max-steps-from-nxt (okay? nxt one)))
      (+ 1 max-steps-from-nxt)))
  
  #; {[Real -> Boolean] -> Real Real -> Boolean}
  (define ((good? target) num@j num@i)
    (<= (- target) (- num@j num@i) (+ target))))

;; ---------------------------------------------------------------------------------------------------
(module% let-loop
  (define from `[[no-listref ,LETLOOP]])
  (define rationale "a micro step before I inline all functions")
  
  (define/contract [max-jump l0 target] max-jump/c
    (define okay? (good? target))
    (first (max-jump-aux l0 okay?)))

  #; {[NEListof Real] [Real -> Boolean] -> [Listof (U False N)]}
  ;; compute the list of maximal jumps from `l0` to its end 
  (define (max-jump-aux l0 okay?)
    (let max-jump-aux ([l l0])
      (cond
        [(empty? (rest l)) '[0]]
        [else
         (define R (rest l))
         (define M (max-jump-aux R))
         (define best (find-max-for (first l) R M okay?))
         (cons best M)])))

  #; {Real [Listof Real] [Listof N] [Real Real -> Boolean] -> (U False N)}
  (define (find-max-for one R M okay?)
    (for/first ([nxt R] [max-steps-from-nxt M] #:when (and max-steps-from-nxt (okay? nxt one)))
      (+ 1 max-steps-from-nxt)))
  
  #; {[Real -> Boolean] -> Real Real -> Boolean}
  (define ((good? target) num@j num@i)
    (<= (- target) (- num@j num@i) (+ target))))

;; ---------------------------------------------------------------------------------------------------
(module% inline
  (define from `[[let-loop ,INLINE]])
  (define rationale "inline all functions")
  
  (define/contract [max-jump l0 target] max-jump/c
    (define (okay? num@j num@i) (<= (- target) (- num@j num@i) (+ target)))
    (define best*
      (let max-jump-aux ([l l0])
        (cond
          [(empty? (rest l)) '[0]]
          [else
           (define R (rest l))
           (define F (first l))
           (define M (max-jump-aux R))
           (define best
             (for/first ([nxt R] [max-steps-from-nxt M] #:when (and max-steps-from-nxt (okay? nxt F)))
               (+ 1 max-steps-from-nxt)))
           (cons best M)])))
    (first best*)))

;; ---------------------------------------------------------------------------------------------------
(module% forwards-base
  (define from `[])
  (define rationale "calculate distance to 0 (forward), not backward (distance from end)")
  
  (define/contract (max-jump l0 target) max-jump/c
    (define N (length l0))
    (define dist-to-0 (apply vector (cons 0 (make-list (- N 1) (- N)))))

    (for ([x l0] [l (in-suffix l0)] [i (in-naturals)])
      (for ([y (rest l)] [j (in-naturals (add1 i))] #:when (<= (abs (- y x)) target))
        (vector-set! dist-to-0 j (max (vector-ref dist-to-0 j) (add1 (vector-ref dist-to-0 i))))))

    (define r (vector-ref dist-to-0 (sub1 N)))
    (and (>= r 0) r)))

;; ---------------------------------------------------------------------------------------------------
(module% forwards-base-2
  (define from `[[forwards-base ,NO-ALLOC]])
  (define rationale "replace parallel datastructure (vectors) with proper data representation")

  (struct node [weight {distance-to-0 #:mutable}] #:transparent)

  (define/contract (max-jump l0 target) max-jump/c
    (define N (length l0))
    (define l (cons (node (first l0) 0) (map (λ (x) (node x (- N))) (rest l0))))

    (for ([x l] [k (in-suffix l)])
      (for ([y (rest k)] #:when (<= (abs (- (node-weight y) (node-weight x))) target))
        (set-node-distance-to-0! y (max (node-distance-to-0 y) (add1 (node-distance-to-0 x))))))
    (define r (node-distance-to-0 (last l)))
    (and (>= r 0) r)))

;; ---------------------------------------------------------------------------------------------------
(test max-jump
      in
      backwards-base no-listref let-loop inline
      forwards-base forwards-base-2
      [#:show-graph #true #:measure 1]
      with
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
