
## Ideas on Transforming Functions Systematically

### Files

- `the-311-project.rkt` ~~ this is transforming a simple sum-of-numbers function a la EoPL 
- `profit.rkt` ~~ this is a semantic transformation a la Darlington (from FP to Imperative) 

- `testing.rkt` ~~ macros to make life simpler in the other files 

###

- `Matthias` is a directory for the examples assigned below
  - one file describes the problem (original text)
  - one file contains the various solutions 

Each code file has this structure:

```
#lang racket

(module general racket ;; collects constraints and contract for problem 
  ...)

(def-module module% f general) ;; module% can be chosen to be something different

(module% name1
   (define from .. association list of module names and rationale strings ..)
   (define rationale "some arbitrary comment for now")
   
   (define/contract (f ...) f/c ...)
   ...)

...

(module% nameN
   (define from .. association list of module names and rationale strings ..)
   (define rationale "some arbitrary comment for now")
   
   (define/contract (f ...) f/c ...)
   ...)

(test f
      in
      name1 ... nameN ;; any order works, affects graph 
      [#:show-graph Boolean]
      with
      (check-...) ;; test cases for f 
      ...)
```


### TODO

- Orange:
  - fallingSquares  MF  -- wrong at source  -- done 
  - sumOfDistanceInTree CD

- Green:
  - maximumGap MF -- done 
  - minimumEffort CD 

- Blue:
  - isScramble MF -- source too slow (hmph) -- 
  - shortest_common_supersequence CD
