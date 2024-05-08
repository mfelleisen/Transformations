
## Ideas on Transforming Functions Systematically

### Files

- `the-311-project.rkt` ~~ this is transforming a simple sum-of-numbers function a la EoPL 
- `profit.rkt` ~~ this is a semantic transformation a la Darlington (from FP to Imperative) 

- `testing.rkt`
  - macros to make life simpler in the Matthias/ Christos/ files
  - top of the file is a "vocabulary" submodule for from justifications 

- `testing-2.rkt`
  - macros to make testing simpler in the Plain/ files

###

- `Plain` is a directory in which each file contains both the AI solutions and ours
  - ai0 .. aiN for all solutions
  - followed by ours 

*Note*

The AI solutions are _adapted_ from the ones in the directories. Here are the changes I made: 

1. I fixed the bugs that the AI injected into two of the solutions. 
2. I replaced `-1` returns with `#false` returns. (It is _dumb_ to use sentinel values in 2024. It makes AI stand for Average Intelligence at best.) 
3. I injected `inexact->exact` instead of going back to the old test cases with `check-within`.   
		Inexact is an artifact of the implementation (using `-inf.0`) tricks, and again, implementation details should not leak in 2024. 




- `Matthias` is a directory for the examples assigned below
  - one file describes the problem (original text)
  - one file contains the various solutions 

- `Christos` is a directory for the examples assigned below
  - one file describes the problem (original text)
  - one file contains the various solutions 

Each code file has this structure:

```
#lang racket

(module general racket ;; collects constraints and contract for problem 
  ...)

(def-module module% f general) ;; module% can be chosen to be something different

(module% name1
   (define from .. association list of module names and justification strings ..)
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

### DONE

- Matthias
  - find-peaks -- done 
  - count-test-devices -- done 
  - sum-counts -- done 

 |    problem: Matthias/   | solutions: Matthias/       | originals: llms/old/		                       |
 | ----------------------- | -------------------------- | ---------------------------------------------------- |
 |  falling-squares.txt	   |  falling-squares.rkt      	| leetcode_hard_processed/LeetCode_505_fallingSquares/ |
 |  is-scramble.txt	   |  is-scramble.rkt          	| leetcode_hard_processed/LeetCode_77_isScramble/      |
 |  maximum-gap.txt	   |  gap.rkt          		| leetcode_hard_processed/LeetCode_124_maximumGap/     |

 |    problem: Matthias/   | solutions: Matthias/       | originals: llms/v2\_processed/	               |
 | ----------------------- | -------------------------- | ---------------------------------------------------- |
 |  maximum-jump.txt	   |  maximum-jump.rkt         	| LeetCodeContests_2855_maximumJumps/     	       |
 |  sum-counts.txt	   |  sum-counts.rkt           	| LeetCodeContests_3163_sumCounts/	       	       |
 |  find-peaks.txt	   |  find-peaks.rkt           	| LeetCodeContests_3221_findPeaks/	       	       |
 |  count-test-devices.txt |  count-test-devices.rkt    | LeetCodeContests_3220_countTestedDevices/   	       |

- Orange:
  - fallingSquares  MF  -- wrong at source  -- done 
  - sumOfDistanceInTree CD

- Green:
  - maximumGap MF -- done 
  - minimumEffort CD 

- Blue:
  - isScramble MF -- source too slow (hmph) -- 
  - shortest_common_supersequence CD
