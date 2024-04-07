#lang racket

(require "../testing.rkt")

(module general racket ;; constraints collected from problem statememt 
  (provide LENGTH is/c)

  (define LENGTH 30)
  
  (define is/c (->i ([s string?]
                     [t string?])
                    #:pre/name (s t) "same length" (= (string-length s) (string-length t))
                    #:pre/name (s) "short string" (<= 1 (string-length s) LENGTH)
                    #:pre/name (s) "lower case s" (andmap char-lower-case? (string->list s))
                    #:pre/name (t) "lower case t" (andmap char-lower-case? (string->list t))
                    (r boolean?))))

(def-module module% is general)

;; ---------------------------------------------------------------------------------------------------
(module% base
  (define from '[])
  (define rationale "INEFFICIENT (build stream of all scrambles, check membership)")
  
  (define/contract (is s t) is/c
    (eprintf "~a vs ~a\n" s t)
    (define k (string->list s))
    (define l (string->list t))
    (cond
      [(equal? k l)            #true]
      [(equal? (reverse k) l)  #true]
      [(different-letters k l) #false]
      [else (stream-member? l (all-of k))]))

  #; {[NEListof Char] -> [Streamof [NEListof Char]]}
  (define (all-of k)
    (cond
      [(empty? (rest k)) (stream k)]
      [else
       (let all-pivots ([i (- (length k) 1)])
         (cond
           [(zero? i) (stream)]
           [else
            (define front* (all-of (take k i)))
            (define back*  (all-of (drop k i)))
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
  (define (different-letters k l)
    (not (equal? (sort k char<?) (sort l char<?)))))

;; ---------------------------------------------------------------------------------------------------
(module% base2
  (define from '[])
  (define rationale "compare the two strings (as lists) in parallel")
  
  (define/contract (is s t) is/c
    (define k (string->list s))
    (define l (string->list t))
    (cond
      [(equal? k l)            #true]
      [(equal? (reverse k) l)  #true]
      [(different-letters k l) #false]
      [else #; (not (empty? k)) 
            (compare-in-parallel k l (length k))]))
  
  #; {[Listof Char] [Listof Char] -> Boolean}
  ;; ASSUME |k| == |l|, |k| > 0
  (define (compare-in-parallel k l L) 
    (cond
      [(empty? (rest k)) (equal? k l)]
      [else
       (for/or ([i (in-range 1 L)])
         (or
          (and (compare-in-parallel (take k i) (take l i) i)
               (compare-in-parallel (drop k i) (drop l i) (- L i)))
          (and (compare-in-parallel (take k i) (drop l (- L i)) i)
               (compare-in-parallel (drop k i) (take l (- L i)) (- L i)))))]))
  
  #; {[Listof Char] [Listof Char] -> Boolean}
  (define (different-letters k l)
    (not (equal? (sort k char<?) (sort l char<?)))))

;; ---------------------------------------------------------------------------------------------------
(module% inline
  (define from `[[base2 ,LETLOOP]])
  (define rationale "compare the two strings (as lists) in parallel")
  
  (define/contract (is s t) is/c
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
                         (compare-in-parallel (drop k i) (take l (- L i)) (- L i)))))]))])))

;; ---------------------------------------------------------------------------------------------------
(module% accumulator
  (define from `[[base2 ,ACCUMULATOR]])
  (define rationale "the accumulator records the length of list to compare")
  
  (define/contract (is s t) is/c
    (define k (string->list s))
    (define l (string->list t))
    (cond
      [(equal? k l)            #true]
      [(equal? (reverse k) l)  #true]
      [(different-letters k l) #false]
      [else #; (not (empty? k)) 
            (compare-in-parallel k l)]))
  
  #; {[Listof Char] [Listof Char] -> Boolean}
  ;; ASSUME |k| == |l|, |k| > 0
  (define (compare-in-parallel k l) 
    (cond
      [(empty? (rest k)) (equal? k l)]
      [else
       (define L (length k))
       (for/or ([i (in-range 1 L)])
         (or
          (and (compare-in-parallel (take k i) (take l i))
               (compare-in-parallel (drop k i) (drop l i)))
          (and (compare-in-parallel (take k i) (drop l (- L i)))
               (compare-in-parallel (drop k i) (take l (- L i))))))]))
  
  #; {[Listof Char] [Listof Char] -> Boolean}
  (define (different-letters k l)
    (not (equal? (sort k char<?) (sort l char<?)))))

;; ---------------------------------------------------------------------------------------------------
(test is
      in
      ; base ; is too inefficient to get thru "datastructure"
      base2 accumulator inline
      [#:show-graph #true]
      with
      
      (check-exn #px"same length" (λ () (is "aaaa" "aaa")))
      
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
      (check-exn #px"same length"  (λ () (is "aabbc" "aabc")))
      (check-true (is "abcdefghijklmnopqrstuvwxyz" "zxywvutsrqponmlkjihgfedcba"))
      (check-true (is "language" "uagelagn"))
      (check-true (is "performance" "rmanceperfo"))
      (check-true (is "dbs" "sdb"))
      (check-exn #px"same length" (λ () (is "" "a")))
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
      (check-exn #px"same length" (λ () (is "bcdefg" "fcbegda")))
      (check-true (is "testing" "gintest"))
      (check-exn #px"same length" (λ () (is "tt" "ttt")))
      (check-false (is "aaa" "aab"))
      (check-true (is "asd" "das"))
      (check-true (is "database" "basedata"))
      (check-true (is "babb" "bbab"))
      (check-true (is "javascript" "riptjavasc"))
      (check-false (is "a" "b"))
      (check-exn #px"same length" (λ () (is "a" "")))
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
      (check-exn #px"same length" (λ () (is "javascript" "jvascripted")))
      (check-true (is "security" "uritysec"))
      (check-true (is "maintenance" "enancemaint"))
      (check-true (is "implementation" "ationimplement"))
      (check-true (is "world" "rldwo"))
      (check-true (is "abcd" "abcd"))
      (check-exn #px"same length" (λ () (is "tt" "t")))
      (check-true (is "debugging" "uggingdeb"))
      (check-true (is "abcd" "dacb"))
      (check-true (is "network" "worknet"))
      (check-true (is "a" "a"))
      (check-exn #px"short string" (λ () (is "" "")))
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
      (check-exn #px"same length" (λ () (is "ttt" "tt")))
      (check-true (is "asd" "ads"))
      (check-exn #px"same length" (λ () (is "javascript" "javasript")))
      (check-exn #px"same length" (λ () (is "abc" "ac")))
      (check-true (is "ab" "ba"))
      (check-true (is "development" "opmentdevel"))
      (check-false (is "abcdefg" "fbegcda"))
      (check-true (is "abcdd" "abddc")))
