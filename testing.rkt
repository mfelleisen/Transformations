#lang racket

(module vocabulary racket

  (provide FUNCTIONAL FUNCTIONAL-VANILLA ACCUMULATOR-VANILLA INLINE VECTOR IMPERATIVE ACCUMULATOR CLASS LETLOOP CPS LambdaLift)

  (define FUNCTIONAL  "plain functional")
  (define FUNCTIONAL-VANILLA  "vanilla recursive functions")
  (define ACCUMULATOR-VANILLA  "vanilla recursive functions with accumulators")
  (define INLINE      "inline all functions")
  (define VECTOR      "use mutable vectors")
  (define IMPERATIVE  "imperative everything")
  (define ACCUMULATOR "use accumulator")
  (define CLASS       "introduce class")
  (define LETLOOP     "local let-loop")
  (define CPS         "cps")
  (define LambdaLift  "ll"))

;; ---------------------------------------------------------------------------------------------------
(provide
 ;; SYNTAX 
 #; (def-module module-kind exported-function imported-sub-modules ...)
 ;; creates a new "submodule" form that
 ;; - always exports `exported-function`
 ;; - always requires `imported-modules` ...
 #; (def-module module% f a b c)
 ;; means programmers can now write 
 #; (module% (define f ...) ...)
 ;; and `f` is exported and defined in the context of `a` `b` and `c`
 def-module)

(require (for-syntax syntax/parse))

(define-syntax (def-module stx)
  (syntax-parse stx
    [(_ module% exported keywords ...)
     #`(define-syntax module% (make-def-module-transformer #'exported #'(keywords ...)))]))

(define-for-syntax ((make-def-module-transformer exported0 lib0) stx)
  (define/syntax-parse exported exported0)
  (define/syntax-parse (lib ...) lib0)
  (syntax-parse stx
    [(_ m def ...)

     #:with from (datum->syntax stx 'from)
     #:with rationale (datum->syntax stx 'rationale)

     #:with racket (datum->syntax stx 'racket)
     #:with vocab  (datum->syntax stx '(require (submod "../testing.rkt" vocabulary)))
     #:with provide (datum->syntax stx 'provide)
     #:with (rr ...) (map (λ (k) (datum->syntax stx `(require (submod ".." ,k)))) (attribute lib))

     #`(module m racket
         (provide exported from rationale)
         vocab
         rr ...
         def ...)]))

;; ---------------------------------------------------------------------------------------------------
;; test them all

(provide
 ;; SYNTAX
 #; (test f in mod-name ... with test-case ...)
 ;; tests the definitions of `f` in sub-modules `mod-name` ... with the specified cases
 test)

(require (for-syntax syntax/parse))
(require (for-syntax racket/format))

; (module+ test (require rackunit))

(define-syntax (test stx)
  (syntax-parse stx
    [(_ exported (~datum in) name ...
        [(~optional (~seq #:show-graph sg #;boolean) #:defaults ([sg #'#false]))]
        (~datum with) test-case ...
        )
     #:do [(define n* (map syntax-e (syntax->list #'(name ...))))
           (define p* (map (λ (n) (~a (symbol->string n) ":")) n*))]
     #:with (rr ...) (map (require-with-prefix stx) p* n*)
     #:with (ss ...) (map (with-prefix stx #'exported) p*)

     #:with (rationale ...) (map (with-prefix stx #'rationale) p*)
     #:with (from ...) (map (with-prefix stx #'from) p*)
     #:with show-graph
     (if (not (syntax-e #'sg)) #'(void)
         #'(let ()
             (define nodes [list (node (~a 'name) "white") ...])
             (define edges
               (append
                (let ([local-edges from])
                  (map (λ (x) (edge (~a (first x)) (~a 'name) (second x))) local-edges))
                ...))
             [main nodes edges]))
                           
     #`(module+ test
         #,(datum->syntax stx '(require rackunit))
         rr ...
         ;; add more test cases here
         (let ([exported ss])
           (eprintf "testing ~a\n" 'name)
           test-case
           ...)
         ...

         show-graph)]))

;; compile time for `test`
#; {Syntax -> [String Symbol -> Syntax]}
(define-for-syntax ((require-with-prefix stx) pre mod-name)
  (datum->syntax stx `(require (prefix-in ,(string->symbol pre) (submod ".." ,mod-name)))))

#; {Syntax -> [Symbol -> Syntax]}
(define-for-syntax ((with-prefix stx syntax-name) pre)
  (define name (syntax-e syntax-name))
  (datum->syntax stx (string->symbol (~a pre name))))

;; run-time for `test`
(require racket/gui/base)

;; an internal representation of the nodes and edges, to communicate from the macro to the run-time
(struct node [name #; string color #;color-string])
(struct edge [from #; string label #;string to #;string])

;; an external lib representation for nodes and edges, to get a graph drawn 
(require circular-layout)

;; translate from internal to external 
#; {Node -> CLNode}
(define (node->clnode n)
  (match-define [node name color] n)
  (clnode (~a name) 50 (~a name) color))

#; {Edge -> CLEdge}
(define (edge->cledge e)
  (match-define [edge from label to] e)
  (cledge from label 3 to #t))

#; {[Listof node] [Listof edge] -> ImageSnip}
(define [main nodes0 edges0]
  (define width  600)
  (define height 800)
  (define center `[,(quotient width 2) . ,(quotient height 2)])
  (define target (make-bitmap width height))
  (define dc (new bitmap-dc% [bitmap target]))
  (define is (make-object image-snip% target))
  (define nodes (map node->clnode nodes0))
  (define edges (map edge->cledge edges0))
  (draw-circular-layout #:dc dc #:nodes nodes #:edges edges #:center center #:scale 10)
  is)

;; ---------------------------------------------------------------------------------------------------

