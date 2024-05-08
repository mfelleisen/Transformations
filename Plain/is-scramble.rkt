#lang racket

(require "../testing-2.rkt")

;; ---------------------------------------------------------------------------------------------------
;; MODULE base
  
(define (is-base s t) ;; contract  is/c
  (eprintf "~a vs ~a\n" s t)
  (define k (string->list s))
  (define l (string->list t))
  (cond
    [(equal? k l)            #true]
    [(equal? (reverse k) l)  #true]
    [(different-letters-base k l) #false]
    [else (stream-member? l (all-of-base k))]))

#; {[NEListof Char] -> [Streamof [NEListof Char]]}
(define (all-of-base k)
  (cond
    [(empty? (rest k)) (stream k)]
    [else
     (let all-pivots ([i (- (length k) 1)])
       (cond
         [(zero? i) (stream)]
         [else
          (define front* (all-of-base (take k i)))
          (define back*  (all-of-base (drop k i)))
          (x-product* front* back* (λ () (all-pivots (sub1 i))))]))]))

#; {[Streamof word] [Streamof Word] [-> [Streamof Word]] -> [Streamof Word]}
(define (x-product* front0 back0 f)
  (let outer ([front* front0] [back* back0])
    (cond
      [(stream-empty? front*) (f)]
      [else 
       (define w (stream-first front*))
       (let inner ([q back*])
         (cond
           [(stream-empty? q) (outer (stream-rest front*) back*)]
           [else
            (define u (stream-first q))
            (stream* (append u w) (append w u) (inner (stream-rest q)))]))])))
  
#; {X [Streamof X] -> Boolean}
(define (stream-member? x s)
  (for/first ([y (in-stream s)] #:when (and (eq? (first x) (first y)) (equal? x y))) #true))
  
#; {[Listof Char] [Listof Char] -> Boolean}
(define (different-letters-base k l)
  (not (equal? (sort k char<?) (sort l char<?))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE base2

(define (is-base2 s t) ;; contract  is/c
  (define k (string->list s))
  (define l (string->list t))
  (cond
    [(equal? k l)            #true]
    [(equal? (reverse k) l)  #true]
    [(different-letters-base2 k l) #false]
    [else #; (not (empty? k)) 
          (compare-in-parallel-base2 k l)]))
  
#; {[Listof Char] [Listof Char] -> Boolean}
;; ASSUME |k| == |l|, |k| > 0
(define (compare-in-parallel-base2 k l)
  (cond
    [(empty? (rest k)) (equal? k l)]
    [else
     (define L (length k))
     (for/or ([i (in-range 1 L)])
       (or
        (and (compare-in-parallel-base2 (take k i) (take l i))
             (compare-in-parallel-base2 (drop k i) (drop l i)))
        (and (compare-in-parallel-base2 (take k i) (drop l (- L i)))
             (compare-in-parallel-base2 (drop k i) (take l (- L i))))))]))
  
#; {[Listof Char] [Listof Char] -> Boolean}
(define (different-letters-base2 k l)
  (not (equal? (sort k char<?) (sort l char<?))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE inline
  
(define (is-inline s t) ;; contract  is/c
  (define k (string->list s))
  (define l (string->list t))
  (cond
    [(equal? k l)            #true]
    [(equal? (reverse k) l)  #true]
    [(not (equal? (sort k char<?) (sort l char<?))) #false]
    [else #; (not (empty? k))
          (let compare-in-parallel ([k k] [l l] [L (length k)])
            (cond
              [(empty? (rest k)) (equal? k l)]
              [else
               (for/or ([i (in-range 1 L)])
                 (or
                  (and (compare-in-parallel (take k i) (take l i) i)
                       (compare-in-parallel (drop k i) (drop l i) (- L i)))
                  (and (compare-in-parallel (take k i) (drop l (- L i)) i)
                       (compare-in-parallel (drop k i) (take l (- L i)) (- L i)))))]))]))

;; ---------------------------------------------------------------------------------------------------
;; MODULE accumulator
  
(define (is-accumulator s t) ;; contract  is/c
  (define k (string->list s))
  (define l (string->list t))
  (cond
    [(equal? k l)            #true]
    [(equal? (reverse k) l)  #true]
    [(different-letters-accumulator k l) #false]
    [else #; (not (empty? k)) 
          (compare-in-parallel-accumulator k l (length k))]))
  
#; {[Listof Char] [Listof Char] -> Boolean}
;; ASSUME |k| == |l|, |k| > 0, |k| = L 
(define (compare-in-parallel-accumulator k l L)
  (cond
    [(empty? (rest k)) (equal? k l)]
    [else
     (for/or ([i (in-range 1 L)])
       (or
        (and (compare-in-parallel-accumulator (take k i) (take l i) i)
             (compare-in-parallel-accumulator (drop k i) (drop l i) (- L i)))
        (and (compare-in-parallel-accumulator (take k i) (drop l (- L i)) i)
             (compare-in-parallel-accumulator (drop k i) (take l (- L i)) (- L i)))))]))
  
#; {[Listof Char] [Listof Char] -> Boolean}
(define (different-letters-accumulator k l)
  (not (equal? (sort k char<?) (sort l char<?))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE cps
  
(define (is-cps s t) ;; contract  is/c
  (define k (string->list s))
  (define l (string->list t))
  (cond
    [(equal? k l)            #true]
    [(equal? (reverse k) l)  #true]
    [(different-letters-cps k l) #false]
    [else #; (not (empty? k)) 
          (compare-in-parallel-cps k l (length k))]))
  
#; {[Listof Char] [Listof Char] -> Boolean}
;; ASSUME |k| == |l|, |k| > 0, |k| = L
(define (compare-in-parallel-cps k l L)
  (let/ec return 
    (let compare-in-parallel ([k k] [l l] [L L] [cont return])
      (cond
        [(empty? (rest k)) (cont (equal? k l))]
        [else
         (let 1success ([i 1] [inner cont])
           (cond
             [(>= i L) (inner #false)]
             [else
              (define k-front-i (take k i))
              (define l-front-i (take l i))
              (define k-back-i  (drop k i))
              (define l-back-i  (drop l i))
              (define l-switch-back (drop l (- L i)))
              (define l-switch-front (take l (- L i)))

              (define and1 ;; first and expression in cps
                (λ (x)
                  (if x
                      (compare-in-parallel k-back-i  l-back-i  (- L i)
                                           (λ (y)
                                             (if y
                                                 (inner #true)
                                                 (fail1 'hukairs))))
                      (fail1 'hukairs))))

              (define fail1 ;; failure of first and expression 
                (λ _ (compare-in-parallel k-front-i l-switch-back i and2)))

              (define and2 ;; second and expression in cps 
                (λ (x)
                  (if x
                      (compare-in-parallel k-back-i  l-switch-front (- L i)
                                           (λ (y)
                                             (if y
                                                 (inner #true)
                                                 (fail2 'hukairs))))
                      (fail2 'hukair))))
                
              (define fail2 ;; failure of second annd expression 
                (λ _ (1success (add1 i) inner)))

              (compare-in-parallel k-front-i l-front-i i and1)]))]))))
  
  
#; {[Listof Char] [Listof Char] -> Boolean}
(define (different-letters-cps k l)
  (not (equal? (sort k char<?) (sort l char<?))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE cps-ll
  
(define (is-cps-ll s t) ;; contract  is/c
  (define k (string->list s))
  (define l (string->list t))
  (cond
    [(equal? k l)            #true]
    [(equal? (reverse k) l)  #true]
    [(different-letters-cps-ll k l) #false]
    [else #; (not (empty? k)) 
          (compare-in-parallel-cps-ll k l (length k))]))

#; {[Listof Char] [Listof Char] -> Boolean}
;; ASSUME |k| == |l|, |k| > 0, |k| = L
(define (compare-in-parallel-cps-ll k l L)
  (let/ec return
    (cip-cps-ll k l L return)))

#; {[Listof Char] [Listof Char] Cont -> Boolean}
(define (cip-cps-ll k l L cont)
  (cond
    [(empty? (rest k)) (cont (equal? k l))]
    [else (1success-cps-ll 1 k l L cont)]))

#; {Natural [Listof Char] [Listof Char] Cont -> Boolean}
(define (1success-cps-ll i k l L inner)
  (cond
    [(>= i L) (inner #false)]
    [else
     (define k-front-i (take k i))
     (define l-front-i (take l i))
     (define k-back-i  (drop k i))
     (define l-back-i  (drop l i))
     (define l-switch-back (drop l (- L i)))
     (define l-switch-front (take l (- L i)))

     (define and1 ;; first and expression in cps
       (λ (x)
         (if x
             (cip-cps-ll k-back-i  l-back-i  (- L i)
                         (λ (y)
                           (if y
                               (inner #true)
                               (fail1 'hukairs))))
             (fail1 'hukairs))))

     (define fail1 ;; failure of first and expression 
       (λ _ (cip-cps-ll k-front-i l-switch-back i and2)))

     (define and2 ;; second and expression in cps 
       (λ (x)
         (if x
             (cip-cps-ll k-back-i  l-switch-front (- L i)
                         (λ (y)
                           (if y
                               (inner #true)
                               (fail2 'hukairs))))
             (fail2 'hukair))))
                
     (define fail2 ;; failure of second annd expression 
       (λ _ (1success-cps-ll (add1 i) k l L inner)))

     (cip-cps-ll k-front-i l-front-i i and1)]))
  
#; {[Listof Char] [Listof Char] -> Boolean}
(define (different-letters-cps-ll k l)
  (not (equal? (sort k char<?) (sort l char<?))))

;; ---------------------------------------------------------------------------------------------------
;; MODULE cps-data
  
(define (is-cps-data s t) ;; contract  is/c
  (define k (string->list s))
  (define l (string->list t))
  (cond
    [(equal? k l)            #true]
    [(equal? (reverse k) l)  #true]
    [(different-letters-cps-data k l) #false]
    [else #; (not (empty? k)) 
          (compare-in-parallel-cps-data k l (length k))]))

#; {[Listof Char] [Listof Char] -> Boolean}
;; ASSUME |k| == |l|, |k| > 0, |k| = L
(define (compare-in-parallel-cps-data k l L)
  (cip-cps-data k l L 'return))

#; {[Listof Char] [Listof Char] Cont -> Boolean}
(define (cip-cps-data k l L cont)
  (cond
    [(empty? (rest k)) (apply-k-cps-data cont (equal? k l))]
    [else (1success-cps-data 1 k l L cont)]))

#; {Natural [Listof Char] [Listof Char] Cont -> Boolean}
(define (1success-cps-data i k l L inner)
  (cond
    [(>= i L) (apply-k-cps-data inner #false)]
    [else
     (define k-front-i (take k i))
     (define l-front-i (take l i))
     (define k-back-i  (drop k i))
     (define l-back-i  (drop l i))
     (define l-switch-back (drop l (- L i)))
     (define l-switch-front (take l (- L i)))

     (define fail2 `[fail2 ,(add1 i) ,k ,l ,L ,inner])
     (define and2 `[and ,k-back-i ,l-switch-front ,(- L i) ,inner ,fail2])
     (define fail1 `[fail1 ,k-front-i ,l-switch-back ,i ,and2])
     (define and1 `[and ,k-back-i ,l-back-i ,(- L i) ,inner ,fail1])
       
     (cip-cps-data k-front-i l-front-i i and1)]))

(define (apply-k-cps-data k x)
  (match k
    ['return x]
    [`[fail2 ,i+1 ,kk ,ll ,L ,inner]
     (1success-cps-data i+1 kk ll L inner)]
    [`[fail1      ,kk ,ll ,i ,and2]
     (cip-cps-data kk ll i and2)]
    [`[and ,kk ,ll ,L-i ,inner ,fail]
     (if x (cip-cps-data kk  ll L-i `[and-decide ,inner ,fail]) (apply-k-cps-data fail 'hukairs))]
    [`[and-decide ,inner ,fail]
     (define y x)
     (if y (apply-k-cps-data inner #true) (apply-k-cps-data fail 'hukairs))]))
  
#; {[Listof Char] [Listof Char] -> Boolean}
(define (different-letters-cps-data k l)
  (not (equal? (sort k char<?) (sort l char<?))))

;; ---------------------------------------------------------------------------------------------------
(test is
      in
      ; base ; is too inefficient to get thru "datastructure"
      base2 accumulator cps cps-ll cps-data inline
      [#:show-graph #true]
      with
      
      #; (check-exn #px"same length" (λ () (is "aaaa" "aaa")))
      
      (check-true (is "abb" "bba"))
      (check-true (is "web" "bwe"))
      (check-true (is "bac" "bca"))
      (check-true (is "abcde" "ebcda"))
      (check-true (is "datastructure" "tastructureda"))
      
      (check-true (is "rat" "tar"))
      (check-true (is "t" "t"))
      (check-true (is "asd" "dsa"))
      (check-true (is "abbc" "acbb"))
      (check-true (is "documentation" "entationdocum"))
      #; (check-exn #px"same length"  (λ () (is "aabbc" "aabc")))
      (check-true (is "abcdefghijklmnopqrstuvwxyz" "zxywvutsrqponmlkjihgfedcba"))
      (check-true (is "language" "uagelagn"))
      (check-true (is "performance" "rmanceperfo"))
      (check-true (is "dbs" "sdb"))
      #; (check-exn #px"same length" (λ () (is "" "a")))
      (check-true (is "algorithm" "gorithmal"))
      (check-true (is "ab" "ab"))
      (check-true (is "optimization" "izationoptim"))
      (check-true (is "abcd" "cbad"))
      (check-true (is "great" "rgeat"))
      (check-false (is "abcdefghij" "efghijcadb"))
      (check-true (is "javascript" "tjavascrip"))
      (check-false (is "abcde" "caebd"))
      (check-true (is "hello" "ohlel"))
      (check-true (is "internet" "terninet"))
      (check-true (is "anagram" "nagaram"))
      (check-true (is "tpg" "pgt"))
      (check-true (is "abc" "cba"))
      (check-true (is "integration" "grationinte"))
      #; (check-exn #px"same length" (λ () (is "bcdefg" "fcbegda")))
      (check-true (is "testing" "gintest"))
      #; (check-exn #px"same length" (λ () (is "tt" "ttt")))
      (check-false (is "aaa" "aab"))
      (check-true (is "asd" "das"))
      (check-true (is "database" "basedata"))
      (check-true (is "babb" "bbab"))
      (check-true (is "javascript" "riptjavasc"))
      (check-false (is "a" "b"))
      #; (check-exn #px"same length" (λ () (is "a" "")))
      (check-true (is "computer" "putercom"))
      (check-false (is "xyxyxyzz" "xxyyzzzx"))
      (check-true (is "design" "sgined"))
      (check-false (is "physics" "fysicep"))
      (check-true (is "deployment" "mentdeploy"))
      (check-true (is "specification" "cationfispeci"))
      (check-true (is "bca" "bca"))
      (check-true (is "python" "ythonp"))
      (check-true (is "tt" "tt"))
      (check-false (is "aacde" "deeca"))
      #; (check-exn #px"same length" (λ () (is "javascript" "jvascripted")))
      (check-true (is "security" "uritysec"))
      (check-true (is "maintenance" "enancemaint"))
      (check-true (is "implementation" "ationimplement"))
      (check-true (is "world" "rldwo"))
      (check-true (is "abcd" "abcd"))
      #; (check-exn #px"same length" (λ () (is "tt" "t")))
      (check-true (is "debugging" "uggingdeb"))
      (check-true (is "abcd" "dacb"))
      (check-true (is "network" "worknet"))
      (check-true (is "a" "a"))
      #; (check-exn #px"short string" (λ () (is "" "")))
      (check-true (is "banana" "ananab"))
      (check-false (is "asd" "dab"))
      (check-true (is "abcd" "badc"))
      (check-false (is "lk" "ll"))
      (check-false (is "javascript" "pseudojack"))
      (check-true (is "javascript" "javascript"))
      (check-true (is "abcd" "dcba"))
      (check-true (is "abcd" "abdc"))
      (check-true (is "programming" "mingprogram"))
      (check-true (is "visualization" "tionvisualiza"))
      (check-true (is "abcd" "cdba"))
      #; (check-exn #px"same length" (λ () (is "ttt" "tt")))
      (check-true (is "asd" "ads"))
      #; (check-exn #px"same length" (λ () (is "javascript" "javasript")))
      #; (check-exn #px"same length" (λ () (is "abc" "ac")))
      (check-true (is "ab" "ba"))
      (check-true (is "development" "opmentdevel"))
      (check-false (is "abcdefg" "fbegcda"))
      (check-true (is "abcdd" "abddc")))
