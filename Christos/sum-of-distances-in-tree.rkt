#lang racket

(require "../testing.rkt")

;; ---------------------------------------------------------------------------------------------------
(module general racket ;; constraints collected from problem statememt 

  (provide MIN-NODES MAX-NODES sdt/c)
  
  (define MIN-NODES 1)
  (define MAX-NODES (* 3 104))

  (define number-of-nodes/c
    (integer-in MIN-NODES MAX-NODES))

  (define (node-index/c n)
    (and/c
     natural?
     (<=/c (- n 1))))

  (define (number-of-edges/c n)
    (flat-named-contract
     'number-of-edges 
     (compose (<=/c (- n 1)) length)))


  (define (edge/c n)
    (and/c
     (list/c (node-index/c n) (node-index/c n))
     (flat-named-contract
      'distinct-ends 
      (λ (e)
        (not (= (first e) (second e)))))))

  (define (edge-list/c n)
    (and/c
     (number-of-edges/c n)
     (listof (edge/c n))))

  (define (distances/c es n)
    (flat-named-contract
     'one-per-node 
     (compose  (=/c n) length)))

  
     

  (define sdt/c
    (->i ([n number-of-nodes/c]
          [es (n) (edge-list/c n)])
         [ds (n es) (distances/c es n)])))

  
(def-module module% sum-of-distances-in-tree general)

;; ---------------------------------------------------------------------------------------------------
(module% base

 (define from '[])
 (define rationale "functional with comprehensions and limited recursion")

 (define (find-children current es)
   (for/list ([e es]
              #:when (member current e))
     (match-define (list a b) e)
     (if (= current a) b a)))
 
 (define (find-path-length start end es seen)
   (cond [(= start end) 0]
         [else
          (define maybe-path-length
            (for/or ([child (find-children start es)]
                     #:unless (member child seen))
              (find-path-length child end es (cons start seen))))
          (and maybe-path-length (+ 1 maybe-path-length))])) 
           


 (define/contract (sum-of-distances-in-tree n es) sdt/c
   (for/list ([i (in-range n)])
     (for/sum ([j (in-range n)])
       (find-path-length i j es empty)))))

;; ---------------------------------------------------------------------------------------------------
(module% accumulator

 (define from `[[base ,ACCUMULATOR]])
 (define rationale "functional with comprehensions and accumulators")

 (define (find-children current es)
   (for/fold ([children empty]              
              #:result children)
             ([e es]
              #:when (member current e))
     (match-define (list a b) e)
     (values (cons (if (= current a) b a) children))))
 
 (define (find-path-length start end es seen path-length)
   (cond [(= start end) path-length]
         [else
          (for/fold ([found #f]
                     #:result found)
                    ([child (find-children start es)]
                     #:unless (member child seen))
            (values
             (or found
                 (find-path-length
                  child end es
                  (cons start seen)
                  (+ 1 path-length)))))]))
   

 
 (define/contract (sum-of-distances-in-tree n es) sdt/c
   (for/fold ([sums empty]
              #:result sums)
             ([i (in-range n)])
     (define one-more-sum
       (for/fold ([sum 0]
                  #:result sum)
                 ([j (in-range n)])
         (values (+ sum (find-path-length i j es empty 0)))))
     (values (append sums (list one-more-sum))))))

;; ---------------------------------------------------------------------------------------------------
(module% assembly

  (define from `[])
  (define rationale "imperative code in o(n)")

  (define/contract (sum-of-distances-in-tree n es) sdt/c

    (define children (make-hash))

    (for ([e es])
      (hash-update! children
	(first e)
	(λ (v) (cons (second e) v))
	empty)
      (hash-update! children
	(second e)
	(λ (v) (cons (first e) v))
	empty)) 

   
    (define distances-descendents (make-hash))
    (define number-of-descendents (make-hash))
   
    (define parents (make-hash))
   
    (define distances-total (make-hash))


    (define seen empty)
    (define current 0)
    (define to-do (list current))
    (define child-d-d 0)
    (define child-n-of-d 0)     
    (define cs empty)
   
    (define (pull-info current seen)
      (define cs (hash-ref children current empty))
      (for ([child cs]
	    #:unless (member child seen))
	(pull-info child (cons current seen))
	(set! child-d-d (hash-ref distances-descendents child 0))
	(set! child-n-of-d (hash-ref number-of-descendents child 0))
	(hash-update! distances-descendents
	  current
	  (λ (s) (+ s 1 child-n-of-d child-d-d ))
	  0)
	(hash-update! number-of-descendents
	  current
	  (λ (s) (+ s 1 child-n-of-d))
	  0)
	(hash-set! parents child current)))

    (pull-info 0 '())   

    (set! seen empty)
    (set! current 0)
    (set! to-do (list current))
    (define maybe-parent #f)
    (define d-d 0)
    (define n-of-d 0)
    (define t-d 0)
    (set! cs empty)
  
    (do () [(empty? to-do)]

      (set! current (first to-do))
     
      (if (member current seen)
         
	  (set! to-do (rest to-do))

	  (begin
	    (set! seen (cons current seen))
	    (set! maybe-parent
	      (hash-ref parents current #f))
	    (set! d-d
	      (hash-ref distances-descendents current 0))
	    (set! n-of-d
	      (hash-ref number-of-descendents current 0))
	    (set! t-d
	      (if maybe-parent
		  (+ d-d
		     (- (hash-ref distances-total maybe-parent)
			(+ 1 n-of-d d-d))
		     (- n 1 n-of-d))
		  d-d))
	    (hash-set! distances-total current t-d)
	    (set! cs (hash-ref children current empty))
	    (set! to-do (append cs (rest to-do))))))
   

    (for/list ([i (in-range n)])
      (hash-ref distances-total i))))

;; ---------------------------------------------------------------------------------------------------
(module% c

  (define from `[])
  (define rationale "imperative code in o(n)")

  (define/contract (sum-of-distances-in-tree n es) sdt/c

    (define children (make-hash))

    (for ([e es])
      (hash-update! children
                    (first e)
                    (λ (v) (cons (second e) v))
                    empty)
      (hash-update! children
                    (second e)
                    (λ (v) (cons (first e) v))
                    empty)) 

   
    (define distances-descendents (make-hash))
    (define number-of-descendents (make-hash))
   
    (define parents (make-hash))
   
    (define distances-total (make-hash))

    (define (pull-info current seen)
      (define cs (hash-ref children current empty))
      (for ([child cs]
            #:unless (member child seen))
        (pull-info child (cons current seen))
        (define child-d-d (hash-ref distances-descendents child 0))
        (define child-n-of-d (hash-ref number-of-descendents child 0))
        (hash-update! distances-descendents
                      current
                      (λ (s) (+ s 1 child-n-of-d child-d-d ))
                      0)
        (hash-update! number-of-descendents
                      current
                      (λ (s) (+ s 1 child-n-of-d))
                      0)
        (hash-set! parents child current)))

    (define (push-info current seen)
      (define maybe-parent
        (hash-ref parents current #f))
      (define d-d
        (hash-ref distances-descendents current 0))
      (define n-of-d
        (hash-ref number-of-descendents current 0))
      (define t-d
        (if maybe-parent
            (+ d-d
               (- (hash-ref distances-total maybe-parent)
                  (+ 1 n-of-d d-d))
               (- n 1 n-of-d))
            d-d))
      (hash-set! distances-total current t-d)
      (define cs (hash-ref children current empty))
      (for ([child cs]
            #:unless (member child seen))
        (push-info child (cons current seen))))
 
 
     
    (pull-info 0 '())    
    (push-info 0 '())
   

    (for/list ([i (in-range n)])
      (hash-ref distances-total i))))
  
;; ---------------------------------------------------------------------------------------------------
(module% functional-recursive

  (define from `[[base ,FUNCTIONAL-VANILLA]])
  (define rationale "turning comprehensions to vanilla recursion")

  (define (find-children current es)
    (cond [(empty? es) empty]
          [(member current (first es))
           (define e (first es))
           (define a (first e))
           (define b (second e))
           (define child
             (if (= current a) b a))
           (cons child (find-children current (rest es)))]
          [else (find-children current (rest es))]))
 
  (define (find-path-length start end es seen)
    (cond [(= start end) 0]
          [(member start seen) #f]
          [else
           (define children (find-children start es))
           (ormap 
            (λ (child)
              (define maybe-path
                (find-path-length child end es (cons start seen)))
              (and maybe-path (+ 1 maybe-path)))
            children)]))
           
 
  (define/contract (sum-of-distances-in-tree nodes es) sdt/c
    (define (inner source n)
      (if (= n 0)
          0
          (+ (find-path-length source (- n 1) es empty)
             (inner source (- n 1)))))
    (define (outter n)
      (if (= n 0)
          empty
          (cons (inner (- n 1) nodes) (outter (- n 1)))))
    (reverse (outter nodes))))

;; ---------------------------------------------------------------------------------------------------
(module% accumulator-recursive

  (define from `[[functional-recursive ,ACCUMULATOR-VANILLA]])
  (define rationale "turning vanilla to accumulator-based")

  (define (find-children current es children)
    (cond [(empty? es) children]
          [(member current (first es))
           (define e (first es))
           (define a (first e))
           (define b (second e))
           (define child
             (if (= current a) b a))
           (find-children current (rest es) (cons child children))]
          [else (find-children current (rest es) children)]))
 
  (define (find-path-length start end es seen path-length)
    (cond [(= start end) path-length]
          [(member start seen) #f]
          [else
           (define children (find-children start es empty))
           (ormap 
            (λ (child)
              (find-path-length child end es (cons start seen) (+ 1 path-length)))
            children)]))

 
 
  (define/contract (sum-of-distances-in-tree nodes es) sdt/c
    (define (inner source n)
      (if (= n 0)
          0
          (+ (find-path-length source (- n 1) es empty 0)
             (inner source (- n 1)))))
    (define (outter n)
      (if (= n 0)
          empty
          (cons (inner (- n 1) nodes) (outter (- n 1)))))
    (reverse (outter nodes))))

;; ---------------------------------------------------------------------------------------------------
(test
  sum-of-distances-in-tree
  in
  base
  accumulator
  functional-recursive
  accumulator-recursive
  c
  assembly
  [#:show-graph #true]
  with
    
  ;; llms's tests:
  (check-equal? (sum-of-distances-in-tree 3 (list (list 0 1) (list 0 2))) (list 2 3 3))
  (check-equal? (sum-of-distances-in-tree 1 empty) (list 0))
  (check-equal? (sum-of-distances-in-tree 2 (list (list 1 0))) (list 1 1))
  (check-equal? (sum-of-distances-in-tree 6 (list (list 0 1) (list 0 2) (list 2 3) (list 2 4) (list 2 5))) (list 8 12 6 10 10 10))

  (check-equal? (sum-of-distances-in-tree 5 (list (list 0 1) (list 0 2) (list 0 3) (list 0 4))) (list 4 7 7 7 7))
  (check-equal? (sum-of-distances-in-tree 5 (list (list 0 1) (list 1 2) (list 2 3) (list 3 4) )) (list 10 7 6 7 10))
  (check-equal? (sum-of-distances-in-tree 7 (list (list 0 1) (list 0 2) (list 1 3) (list 1 4) (list 2 5) (list 2 6)))
                (list 10 11 11 16 16 16 16))
  )


 
