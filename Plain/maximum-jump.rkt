#lang racket

(require "../testing-2.rkt")

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai8

(define (max-jump-ai8 nums target)
  ;; Get the length of the nums list
  (define n (length nums))
  ;; Initialize the dp list with negative infinity for all indices except the first one set to 0
  (define dp (cons 0 (make-list (sub1 n) -inf.0)))
  
  ;; Define a helper function to update dp values based on the current index i
  (define (update-dp i dp)
    (when (not (= (list-ref dp i) -inf.0))  ;; Only proceed if the current index is reachable
      ;; Iterate over all possible jumps from index i
      (for ([j (in-range (add1 i) n)])
        (when (<= (abs (- (list-ref nums j) (list-ref nums i))) target)
          ;; Update the dp value at index j if a better jump count is found
          (set! dp (list-set dp j (max (list-ref dp j) (add1 (list-ref dp i))))))))
    dp)
  
  ;; Process each index to update dp values
  (for ([i (in-range n)])
    (set! dp (update-dp i dp)))
  
  ;; Check the value at the last index to determine if the end is reachable
  (define final-value (list-ref dp (sub1 n)))
  (if (= final-value -inf.0) #false (inexact->exact final-value)))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai7

(define (max-jump-ai7 nums target)
  (let ([n (length nums)])
    ;; Initialize a list to keep track of the maximum jumps to each index, starting with negative infinity (represented by #f)
    ;; except for the first index which is 0 (no jumps needed to reach itself).
    (define dp (make-vector n #f))
    (vector-set! dp 0 0)  ; Start position, no jumps needed
    
    ;; Iterate over each index in nums
    (for ([i (in-range n)])
      (when (vector-ref dp i)  ; Only consider this index if it is reachable
        ;; Check possible jumps from index i to all indices j where i < j
        (for ([j (in-range (add1 i) n)])
          (when (<= (abs (- (list-ref nums j) (list-ref nums i))) target)
            ;; Update the maximum jumps to index j
            (vector-set! dp j (max (or (vector-ref dp j) -inf.0) (add1 (vector-ref dp i))))))))
    
    ;; Check the value for the last index; return -1 if it's still false (#f), meaning it's unreachable
    (let ([last (vector-ref dp (sub1 n))])
      (if last (inexact->exact last) #false))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai6

(define (max-jump-ai6 nums target)
  ;; Get the length of the nums list
  (define n (length nums))
  ;; Initialize the dp list with negative infinity, except for the first element which is 0
  (define dp (cons 0 (make-list (sub1 n) -inf.0)))
  
  ;; Define a helper function to update dp based on the current index i
  (define (update-dp i dp)
    (if (= (list-ref dp i) -inf.0)
        dp  ;; If current dp[i] is -inf, skip updating
        ;; Else, update dp for all j > i within bounds and where nums[j] - nums[i] is within [-target, target]
        (for/fold ([dp dp]) ([j (in-range (add1 i) n)])
          (if (<= (abs (- (list-ref nums j) (list-ref nums i))) target)
              (list-set dp j (max (list-ref dp j) (add1 (list-ref dp i))))
              dp))))
  
  ;; Update dp for each index using a fold
  (define final-dp (for/fold ([dp dp]) ([i (in-range n)])
                     (update-dp i dp)))
  
  ;; Check the last element of dp to determine the result
  (define result (last final-dp))
  (if (= result -inf.0) #false (inexact->exact result)))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai5

(define (max-jump-ai5 nums target)
  (let ([n (length nums)])
    ;; Initialize a list to store the maximum jumps to each index,
    ;; starting with negative infinity for all except the first index which is 0
    (define dp (make-vector n -inf.0))
    (vector-set! dp 0 0)  ; No jump needed for the first element
    
    ;; Iterate over each index as the starting point
    (for ([i (in-range n)])
      (when (not (= (vector-ref dp i) -inf.0))  ; Only consider reachable indices
        ;; Check possible jumps from index i
        (for ([j (in-range (add1 i) n)])
          (when (<= (abs (- (list-ref nums j) (list-ref nums i))) target)
            ;; Update the dp vector with the maximum jumps to index j
            (vector-set! dp j (max (vector-ref dp j) (add1 (vector-ref dp i))))))))
    
    ;; Check the value at the last index to determine if it's reachable
    (let ([final-jumps (vector-ref dp (sub1 n))])
      (if (= final-jumps -inf.0)
          #false
          (inexact->exact final-jumps)))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai4

(define (max-jump-ai4 nums target)
  (let ([n (length nums)])
    ;; Initialize DP table with negative infinity for unreachable and 0 for the start position.
    (define dp (make-vector n -inf.0))
    (vector-set! dp 0 0)
    
    ;; Iterate over each index as the starting point for jumps.
    (for ([i (in-range n)])
      (when (not (= (vector-ref dp i) -inf.0))  ; Only consider reachable positions.
        ;; Check possible jumps from index i to index j.
        (for ([j (in-range (add1 i) n)])
          (when (<= (abs (- (list-ref nums j) (list-ref nums i))) target)
            ;; Update dp[j] to the maximum of its current value or dp[i] + 1.
            (vector-set! dp j (max (vector-ref dp j) (add1 (vector-ref dp i))))))))
    
    ;; Check the value for the last position; return -1 if it's still negative infinity.
    (let ([last-jump (vector-ref dp (sub1 n))])
      (if (= last-jump -inf.0)
          #false
          (inexact->exact last-jump)))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai3

(define (max-jump-ai3 nums target)
  (let ([n (length nums)])  ; Get the length of the list
    ;; Initialize the dp list with negative infinity except the first element set to 0
    (define dp (cons 0 (make-list (- n 1) -inf.0)))
    ;; Define a helper function to update dp based on the current index i
    (define (update-dp i dp)
      (if (= (list-ref dp i) -inf.0)
          dp  ; If dp[i] is -inf, return dp unchanged
          (foldl (lambda (j dp-acc)  ; Fold over the range from i+1 to n
                   (if (<= (abs (- (list-ref nums j) (list-ref nums i))) target)
                       (list-set dp-acc j (max (list-ref dp-acc j) (+ 1 (list-ref dp-acc i))))
                       dp-acc))
                 dp
                 (range (+ i 1) n))))
    ;; Fold over the range from 0 to n to update dp for each index
    (define final-dp (foldl update-dp dp (range n)))
    ;; Check the last element of dp; if it's still -inf, return -1, otherwise return the value
    (if (= (last final-dp) -inf.0)
        #false
        (inexact->exact (last final-dp)))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai2

(define (max-jump-ai2 nums target)
  ;; Initialize the length of nums and the DP array.
  (define n (length nums))
  (define dp (make-vector n -inf.0)) ; Create a vector of size n initialized with negative infinity
  (vector-set! dp 0 0) ; Start position, no jumps needed
  
  ;; Loop over each position in nums
  (for ([i (in-range n)])
    (when (not (= (vector-ref dp i) -inf.0)) ; Skip if this index is not reachable
      ;; Check possible jumps from position i
      (for ([j (in-range (add1 i) n)])
        (when (<= (abs (- (list-ref nums j) (list-ref nums i))) target)
          ;; Update the DP value for position j
          (vector-set! dp j (max (vector-ref dp j) (add1 (vector-ref dp i))))))))
  
  ;; Check if the last position is reachable
  (if (= (vector-ref dp (sub1 n)) -inf.0)
      #false
      (inexact->exact (vector-ref dp (sub1 n)))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai1

(define (max-jump-ai1 nums target)
  ;; Get the length of nums
  (define n (length nums))
  ;; Initialize a list to store the maximum jumps to each index, starting with negative infinity
  (define dp (make-vector n -inf.0))
  ;; The start position requires 0 jumps
  (vector-set! dp 0 0)
  
  ;; Iterate over each index in nums
  (for ([i (in-range n)])
    ;; Continue only if current index is reachable
    (when (not (= (vector-ref dp i) -inf.0))
      ;; Check potential jumps from index i to j
      (for ([j (in-range (add1 i) n)])
        (when (<= (abs (- (list-ref nums j) (list-ref nums i))) target)
          ;; Update the dp vector with the maximum of current value or jumps from i + 1
          (vector-set! dp j (max (vector-ref dp j) (add1 (vector-ref dp i))))))))
  
  ;; Check the value at the last index to determine if it's reachable
  (define result (vector-ref dp (sub1 n)))
  (if (= result -inf.0)
      #false
      (inexact->exact result)))

;; ---------------------------------------------------------------------------------------------------
;; MODULE ai0

(define (max-jump-ai0 nums target)
  ;; Define the length of the nums list
  (define n (length nums))
  ;; Initialize the DP list with negative infinity, except the first element which is 0
  (define dp (cons 0 (make-list (sub1 n) -inf.0)))
  
  ;; Define a helper function to update the dp list based on the current index i and target
  (define (update-dp i dp)
    (if (= (list-ref dp i) -inf.0)
        dp  ;; If current dp[i] is -inf, no update is needed
        ;; Otherwise, update dp for each j from i+1 to n-1
        (for/fold ([dp dp]) ([j (in-range (add1 i) n)])
          (if (<= (abs (- (list-ref nums j) (list-ref nums i))) target)
              (list-set dp j (max (list-ref dp j) (add1 (list-ref dp i))))
              dp))))
  
  ;; Update dp for each index from 0 to n-1
  (define final-dp (for/fold ([dp dp]) ([i (in-range n)])
                    (update-dp i dp)))
  
  ;; Check the last element of dp; if it's still -inf, return -1, otherwise return its value
  (if (= (last final-dp) -inf.0) #false (inexact->exact (last final-dp))))

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
         (define best
           (ormap (λ (nxt from-nxt) (and from-nxt (okay?-inline? nxt F) (+ 1 from-nxt))) R M))
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

      ai8 ai7 ai6 ai5 ai4 ai3 ai2 ai1 ai0

      backwards-base no-listref let-loop inline forwards-base forwards-base-2
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
