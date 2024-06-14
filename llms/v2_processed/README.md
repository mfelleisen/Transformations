
## Allocation of Inspection

### Protocol

We will score each program as follows: 

| score | [would] produce a versionthat is HIGHly modern  | ... |
| ----- | ----------------------------------------------- | --- |
|   4   | HIGHLY readable program as is                   | ... |
|   3   | minor changes produce a HIGHLY readable program | ... | 
|   2   | easy rewrites of functions into `for`           | ... |
|       | obvious merger of functions                     | ... |
|   1   | sophisticated merger of functions               | ... | 
|       | sophisticated recognition of eliminating indexing with `for` over lists | ... | 
|   0   | none of the above helps                         | ... | 


Examples 

1. AI fails to recognize when a tail-recursive function with accumulators 
is just a `for/fold` in modern Racket. See minIncrementOperations/




### Christos

| done | name        | -> highest  | --> directory | --> file | score | 
| ---- | ----------- | ----------- | ------------- | -------- | ----- | 
|  0   | minimumSum/ | [highest](LeetCodeContests_2811_minimumSum/highest_heuristic.txt) |  [dir](LeetCodeContests_2811_minimumSum/) |  [path-to-selected-high](LeetCodeContests_2811_minimumSum/1_refactored/refactor_6.rkt) | 5 |
|   4   | theMaximumAchievableX/ | [highest](LeetCodeContests_2812_theMaximumAchievableX/highest_heuristic.txt) |  [dir](LeetCodeContests_2812_theMaximumAchievableX) |  [path-to-selected-high](LeetCodeContests_2812_theMaximumAchievableX/10_refactored/refactor_14.rkt) | 2 |
|  2   | longestEqualSubarray/ | [highest](LeetCodeContests_2832_longestEqualSubarray/highest_heuristic.txt) |  [dir](LeetCodeContests_2832_longestEqualSubarray) |  [path-to-selected-high](LeetCodeContests_2832_longestEqualSubarray/1_refactored/refactor_95.rkt) | 0 |
|  2  | sumOfSquares/ | [highest](LeetCodeContests_2844_sumOfSquares/highest_heuristic.txt) |  [dir](LeetCodeContests_2844_sumOfSquares) |  [path-to-selected-high](LeetCodeContests_2844_sumOfSquares/10_refactored/refactor_45.rkt) | 4 |
|  2   | maximumJumps/ | [highest](LeetCodeContests_2855_maximumJumps/highest_heuristic.txt) |  [dir](LeetCodeContests_2855_maximumJumps) |  [path-to-selected-high](LeetCodeContests_2855_maximumJumps/3_refactored/refactor_43.rkt) | 1 |
|  2   | longestAlternatingSubarray/ | [highest](LeetCodeContests_2866_longestAlternatingSubarray/highest_heuristic.txt) |  [dir](LeetCodeContests_2866_longestAlternatingSubarray) |  [path-to-selected-high](LeetCodeContests_2866_longestAlternatingSubarray/0_refactored/refactor_79.rkt) | 6 |
|  3  | maxNonDecreasingLength/ | [highest](LeetCodeContests_2869_maxNonDecreasingLength/highest_heuristic.txt) |  [dir](LeetCodeContests_2869_maxNonDecreasingLength) |  [path-to-selected-high](LeetCodeContests_2869_maxNonDecreasingLength/0_refactored/refactor_42.rkt) | 2 |
|  2  | maxArrayValue/ | [highest](LeetCodeContests_2872_maxArrayValue/highest_heuristic.txt) |  [dir](LeetCodeContests_2872_maxArrayValue) |  [path-to-selected-high](LeetCodeContests_2872_maxArrayValue/2_refactored/refactor_10.rkt) | 7 |
|  2   | findPrimePairs/ | [highest](LeetCodeContests_2873_findPrimePairs/highest_heuristic.txt) |  [dir](LeetCodeContests_2873_findPrimePairs) |  [path-to-selected-high](LeetCodeContests_2873_findPrimePairs/4_refactored/refactor_45.rkt) | 7 |
|  2  | numberOfEmployeesWhoMetTarget/ | [highest](LeetCodeContests_2876_numberOfEmployeesWhoMetTarget/highest_heuristic.txt) |  [dir](LeetCodeContests_2876_numberOfEmployeesWhoMetTarget) |  [path-to-selected-high](LeetCodeContests_2876_numberOfEmployeesWhoMetTarget/9_refactored/refactor_68.rkt) | 3 |
|     | numberOfWays/ | [highest](LeetCodeContests_2882_numberOfWays/highest_heuristic.txt) |  [dir](LeetCodeContests_2882_numberOfWays) |  [path-to-selected-high](LeetCodeContests_2882_numberOfWays/0_refactored/refactor_18.rkt) | 3 |
|     | countBlackBlocks/ | [highest](LeetCodeContests_2889_countBlackBlocks/highest_heuristic.txt) |  [dir](LeetCodeContests_2889_countBlackBlocks) |  [path-to-selected-high](LeetCodeContests_2889_countBlackBlocks/0_refactored/refactor_42.rkt) | 5 |
|     | maximumBeauty/ | [highest](LeetCodeContests_2891_maximumBeauty/highest_heuristic.txt) |  [dir](LeetCodeContests_2891_maximumBeauty) |  [path-to-selected-high](LeetCodeContests_2891_maximumBeauty/10_refactored/refactor_76.rkt) | 7 |
|     | isGood/ | [highest](LeetCodeContests_2892_isGood/highest_heuristic.txt) |  [dir](LeetCodeContests_2892_isGood) |  [path-to-selected-high](LeetCodeContests_2892_isGood/9_refactored/refactor_32.rkt) | 4 |
|     | maxScore/ | [highest](LeetCodeContests_2893_maxScore/highest_heuristic.txt) |  [dir](LeetCodeContests_2893_maxScore) |  [path-to-selected-high](LeetCodeContests_2893_maxScore/1_refactored/refactor_35.rkt) | 1 |
|     | maxSum/ | [highest](LeetCodeContests_2902_maxSum/highest_heuristic.txt) |  [dir](LeetCodeContests_2902_maxSum) |  [path-to-selected-high](LeetCodeContests_2902_maxSum/0_refactored/refactor_23.rkt) | 5 |
|     | countPairs/ | [highest](LeetCodeContests_2917_countPairs/highest_heuristic.txt) |  [dir](LeetCodeContests_2917_countPairs) |  [path-to-selected-high](LeetCodeContests_2917_countPairs/3_refactored/refactor_50.rkt) | 3 |
|     | maxIncreasingGroups/ | [highest](LeetCodeContests_2919_maxIncreasingGroups/highest_heuristic.txt) |  [dir](LeetCodeContests_2919_maxIncreasingGroups) |  [path-to-selected-high](LeetCodeContests_2919_maxIncreasingGroups/1_refactored/refactor_4.rkt) | 3 |
|     | countPairs/ | [highest](LeetCodeContests_2953_countPairs/highest_heuristic.txt) |  [dir](LeetCodeContests_2953_countPairs) |  [path-to-selected-high](LeetCodeContests_2953_countPairs/10_refactored/refactor_30.rkt) | 4 |
|     | maxSum/ | [highest](LeetCodeContests_2954_maxSum/highest_heuristic.txt) |  [dir](LeetCodeContests_2954_maxSum) |  [path-to-selected-high](LeetCodeContests_2954_maxSum/1_refactored/refactor_85.rkt) | 9 |
|     | accountBalanceAfterPurchase/ | [highest](LeetCodeContests_2955_accountBalanceAfterPurchase/highest_heuristic.txt) |  [dir](LeetCodeContests_2955_accountBalanceAfterPurchase) |  [path-to-selected-high](LeetCodeContests_2955_accountBalanceAfterPurchase/1_refactored/refactor_8.rkt) | 3 |
|     | maximizeTheProfit/ | [highest](LeetCodeContests_2979_maximizeTheProfit/highest_heuristic.txt) |  [dir](LeetCodeContests_2979_maximizeTheProfit) |  [path-to-selected-high](LeetCodeContests_2979_maximizeTheProfit/1_refactored/refactor_65.rkt) | -7 |
|     | countSymmetricIntegers/ | [highest](LeetCodeContests_2998_countSymmetricIntegers/highest_heuristic.txt) |  [dir](LeetCodeContests_2998_countSymmetricIntegers) |  [path-to-selected-high](LeetCodeContests_2998_countSymmetricIntegers/0_refactored/refactor_90.rkt) | 5 |
|     | minAbsoluteDifference/ | [highest](LeetCodeContests_3000_minAbsoluteDifference/highest_heuristic.txt) |  [dir](LeetCodeContests_3000_minAbsoluteDifference) |  [path-to-selected-high](LeetCodeContests_3000_minAbsoluteDifference/8_refactored/refactor_18.rkt) | 3 |
|     | minimumPossibleSum/ | [highest](LeetCodeContests_3026_minimumPossibleSum/highest_heuristic.txt) |  [dir](LeetCodeContests_3026_minimumPossibleSum) |  [path-to-selected-high](LeetCodeContests_3026_minimumPossibleSum/9_refactored/refactor_4.rkt) | 6 |
|     | getMaxFunctionValue/ | [highest](LeetCodeContests_3032_getMaxFunctionValue/highest_heuristic.txt) |  [dir](LeetCodeContests_3032_getMaxFunctionValue) |  [path-to-selected-high](LeetCodeContests_3032_getMaxFunctionValue/0_refactored/refactor_3.rkt) | 0 |
|     | numberOfPoints/ | [highest](LeetCodeContests_3034_numberOfPoints/highest_heuristic.txt) |  [dir](LeetCodeContests_3034_numberOfPoints) |  [path-to-selected-high](LeetCodeContests_3034_numberOfPoints/0_refactored/refactor_69.rkt) | 5 |
|     | minimumRightShifts/ | [highest](LeetCodeContests_3045_minimumRightShifts/highest_heuristic.txt) |  [dir](LeetCodeContests_3045_minimumRightShifts) |  [path-to-selected-high](LeetCodeContests_3045_minimumRightShifts/1_refactored/refactor_37.rkt) | 2 |
|     | minOperations/ | [highest](LeetCodeContests_3094_minOperations/highest_heuristic.txt) |  [dir](LeetCodeContests_3094_minOperations) |  [path-to-selected-high](LeetCodeContests_3094_minOperations/2_refactored/refactor_23.rkt) | 8 |
|     | maxNumberOfAlloys/ | [highest](LeetCodeContests_3095_maxNumberOfAlloys/highest_heuristic.txt) |  [dir](LeetCodeContests_3095_maxNumberOfAlloys) |  [path-to-selected-high](LeetCodeContests_3095_maxNumberOfAlloys/0_refactored/refactor_3.rkt) | 1 |
|     | lengthOfLongestSubsequence/ | [highest](LeetCodeContests_3106_lengthOfLongestSubsequence/highest_heuristic.txt) |  [dir](LeetCodeContests_3106_lengthOfLongestSubsequence) |  [path-to-selected-high](LeetCodeContests_3106_lengthOfLongestSubsequence/0_refactored/refactor_54.rkt) | -1 |
|     | maximumSumOfHeights/ | [highest](LeetCodeContests_3114_maximumSumOfHeights/highest_heuristic.txt) |  [dir](LeetCodeContests_3114_maximumSumOfHeights) |  [path-to-selected-high](LeetCodeContests_3114_maximumSumOfHeights/3_refactored/refactor_47.rkt) | 6 |

### Matthias

(a) AI re-invents `in-suffixes` (but in an expensive way). 

(b) AI re-invests `list->set` (not realizing that lists can be treated as sets).  (I initially placed this remark with the wrong function. Argh.) 

(c) AI writes ";; The function uses higher-order functions and idiomatic constructs such as `for/list` to avoid
;; explicit loops." All I can say is "huh"? 

(d) I copied the `in-suffixes` over and then a good modern code fell into place.

(e) "AI" creates a list of 1 number, matches it, and never does anything else with match. Worst code I have ever seen. Not even my F 1 students could do this badly. 

(f) when I worked on this I came up with a much compact solution. But doing so requires insight into the arithmetic of the problem, not a mere structural transformation. 

(g) Only because it is somewhat natural, once one stupid thing is removed ... but it can be done with a `for/fold` that is even better in modern Racket 

(h) but this helped me find a bug in Racket's for/fold stop-after 


| done | name        | -> highest  | --> directory | --> file | score | 
| ---- | ----------- | ----------- | ------------- | -------- | ----- | 
| 2 | getWordsInLongestSubsequence/ | [highest](LeetCodeContests_3143_getWordsInLongestSubsequence/highest_heuristic.txt) |  [dir](LeetCodeContests_3143_getWordsInLongestSubsequence) |  [path-to-selected-high](LeetCodeContests_3143_getWordsInLongestSubsequence/1_refactored/refactor_86.rkt) | 6 |
| 1 | maximumTripletValue/ | [highest](LeetCodeContests_3154_maximumTripletValue/highest_heuristic.txt) |  [dir](LeetCodeContests_3154_maximumTripletValue) |  [path-to-selected-high](LeetCodeContests_3154_maximumTripletValue/10_refactored/refactor_11.rkt) | 1 |
| 1 (a) | sumCounts/ | [highest](LeetCodeContests_3163_sumCounts/highest_heuristic.txt) |  [dir](LeetCodeContests_3163_sumCounts) |  [path-to-selected-high](LeetCodeContests_3163_sumCounts/0_refactored/refactor_68.rkt) | 5 |
| 1  | findIndices/ | [highest](LeetCodeContests_3165_findIndices/highest_heuristic.txt) |  [dir](LeetCodeContests_3165_findIndices) |  [path-to-selected-high](LeetCodeContests_3165_findIndices/0_refactored/refactor_1.rkt) | 0 |
| 3 | minimumSum/ | [highest](LeetCodeContests_3176_minimumSum/highest_heuristic.txt) |  [dir](LeetCodeContests_3176_minimumSum) |  [path-to-selected-high](LeetCodeContests_3176_minimumSum/0_refactored/refactor_54.rkt) | 6 |
| 1 | minIncrementOperations/ | [highest](LeetCodeContests_3178_minIncrementOperations/highest_heuristic.txt) |  [dir](LeetCodeContests_3178_minIncrementOperations) |  [path-to-selected-high](LeetCodeContests_3178_minIncrementOperations/0_refactored/refactor_53.rkt) | 3 |
|  3   | findKOr/ | [highest](LeetCodeContests_3183_findKOr/highest_heuristic.txt) |  [dir](LeetCodeContests_3183_findKOr) |  [path-to-selected-high](LeetCodeContests_3183_findKOr/1_refactored/refactor_36.rkt) | 8 |
|  2  | findChampion/ | [highest](LeetCodeContests_3188_findChampion/highest_heuristic.txt) |  [dir](LeetCodeContests_3188_findChampion) |  [path-to-selected-high](LeetCodeContests_3188_findChampion/4_refactored/refactor_91.rkt) | 4 |
|  3   | findChampion/ | [highest](LeetCodeContests_3189_findChampion/highest_heuristic.txt) |  [dir](LeetCodeContests_3189_findChampion) |  [path-to-selected-high](LeetCodeContests_3189_findChampion/0_refactored/refactor_51.rkt) | 3 |
|  2 (c)  | maximumXorProduct/ | [highest](LeetCodeContests_3192_maximumXorProduct/highest_heuristic.txt) |  [dir](LeetCodeContests_3192_maximumXorProduct) |  [path-to-selected-high](LeetCodeContests_3192_maximumXorProduct/8_refactored/refactor_98.rkt) | 5 |
|  2  | maximumStrongPairXor/ | [highest](LeetCodeContests_3193_maximumStrongPairXor/highest_heuristic.txt) |  [dir](LeetCodeContests_3193_maximumStrongPairXor) |  [path-to-selected-high](LeetCodeContests_3193_maximumStrongPairXor/4_refactored/refactor_72.rkt) | 9 |
|  3  | distributeCandies/ | [highest](LeetCodeContests_3199_distributeCandies/highest_heuristic.txt) |  [dir](LeetCodeContests_3199_distributeCandies) |  [path-to-selected-high](LeetCodeContests_3199_distributeCandies/1_refactored/refactor_66.rkt) | 4 |
|  0 (d)| findHighAccessEmployees/ | [highest](LeetCodeContests_3202_findHighAccessEmployees/highest_heuristic.txt) |  [dir](LeetCodeContests_3202_findHighAccessEmployees) |  [path-to-selected-high](LeetCodeContests_3202_findHighAccessEmployees/0_refactored/refactor_95.rkt) | 6 |
|   0  | findIntersectionValues/ | [highest](LeetCodeContests_3206_findIntersectionValues/highest_heuristic.txt) |  [dir](LeetCodeContests_3206_findIntersectionValues) |  [path-to-selected-high](LeetCodeContests_3206_findIntersectionValues/1_refactored/refactor_22.rkt) | 6 |
|   0 (e) | countSubarrays/ | [highest](LeetCodeContests_3213_countSubarrays/highest_heuristic.txt) |  [dir](LeetCodeContests_3213_countSubarrays) |  [path-to-selected-high](LeetCodeContests_3213_countSubarrays/2_refactored/refactor_65.rkt) | 4 |
|   3  | areSimilar/ | [highest](LeetCodeContests_3215_areSimilar/highest_heuristic.txt) |  [dir](LeetCodeContests_3215_areSimilar) |  [path-to-selected-high](LeetCodeContests_3215_areSimilar/1_refactored/refactor_9.rkt) | 5 |
|   3 (f)  | countTestedDevices/ | [highest](LeetCodeContests_3220_countTestedDevices/highest_heuristic.txt) |  [dir](LeetCodeContests_3220_countTestedDevices) |  [path-to-selected-high](LeetCodeContests_3220_countTestedDevices/0_refactored/refactor_3.rkt) | 7 |
|   2 (g)  | findPeaks/ | [highest](LeetCodeContests_3221_findPeaks/highest_heuristic.txt) |  [dir](LeetCodeContests_3221_findPeaks) |  [path-to-selected-high](LeetCodeContests_3221_findPeaks/1_refactored/refactor_64.rkt) | 4 |
|  3   | numberGame/ | [highest](LeetCodeContests_3226_numberGame/highest_heuristic.txt) |  [dir](LeetCodeContests_3226_numberGame) |  [path-to-selected-high](LeetCodeContests_3226_numberGame/8_refactored/refactor_26.rkt) | 7 |
|  3  | findMissingAndRepeatedValues/ | [highest](LeetCodeContests_3227_findMissingAndRepeatedValues/highest_heuristic.txt) |  [dir](LeetCodeContests_3227_findMissingAndRepeatedValues) |  [path-to-selected-high](LeetCodeContests_3227_findMissingAndRepeatedValues/0_refactored/refactor_9.rkt) | 1 |
|  0 (h)  | missingInteger/ | [highest](LeetCodeContests_3236_missingInteger/highest_heuristic.txt) |  [dir](LeetCodeContests_3236_missingInteger) |  [path-to-selected-high](LeetCodeContests_3236_missingInteger/8_refactored/refactor_52.rkt) | 7 |
|  3  | minimumOperationsToMakeEqual/ | [highest](LeetCodeContests_3239_minimumOperationsToMakeEqual/highest_heuristic.txt) |  [dir](LeetCodeContests_3239_minimumOperationsToMakeEqual) |  [path-to-selected-high](LeetCodeContests_3239_minimumOperationsToMakeEqual/0_refactored/refactor_59.rkt) | 6 |
|  2   | findMaximumNumber/ | [highest](LeetCodeContests_3240_findMaximumNumber/highest_heuristic.txt) |  [dir](LeetCodeContests_3240_findMaximumNumber) |  [path-to-selected-high](LeetCodeContests_3240_findMaximumNumber/1_refactored/refactor_94.rkt) | 4 |
|   3  | divideArray/ | [highest](LeetCodeContests_3241_divideArray/highest_heuristic.txt) |  [dir](LeetCodeContests_3241_divideArray) |  [path-to-selected-high](LeetCodeContests_3241_divideArray/0_refactored/refactor_11.rkt) | 6 |
|   2 | maxFrequencyElements/ | [highest](LeetCodeContests_3242_maxFrequencyElements/highest_heuristic.txt) |  [dir](LeetCodeContests_3242_maxFrequencyElements) |  [path-to-selected-high](LeetCodeContests_3242_maxFrequencyElements/4_refactored/refactor_23.rkt) | 5 |
|   3  | hasTrailingZeros/ | [highest](LeetCodeContests_3246_hasTrailingZeros/highest_heuristic.txt) |  [dir](LeetCodeContests_3246_hasTrailingZeros) |  [path-to-selected-high](LeetCodeContests_3246_hasTrailingZeros/0_refactored/refactor_19.rkt) | 9 |
|   3  | minOperations/ | [highest](LeetCodeContests_3249_minOperations/highest_heuristic.txt) |  [dir](LeetCodeContests_3249_minOperations) |  [path-to-selected-high](LeetCodeContests_3249_minOperations/17_refactored/refactor_58.rkt) | 5 |
|  3   | areaOfMaxDiagonal/ | [highest](LeetCodeContests_3251_areaOfMaxDiagonal/highest_heuristic.txt) |  [dir](LeetCodeContests_3251_areaOfMaxDiagonal) |  [path-to-selected-high](LeetCodeContests_3251_areaOfMaxDiagonal/0_refactored/refactor_35.rkt) | 11 |
| 2  | incremovableSubarrayCount/ | [highest](LeetCodeContests_3252_incremovableSubarrayCount/highest_heuristic.txt) |  [dir](LeetCodeContests_3252_incremovableSubarrayCount) |  [path-to-selected-high](LeetCodeContests_3252_incremovableSubarrayCount/0_refactored/refactor_98.rkt) | 4 |
|  1  | minimumCost/ | [highest](LeetCodeContests_3260_minimumCost/highest_heuristic.txt) |  [dir](LeetCodeContests_3260_minimumCost) |  [path-to-selected-high](LeetCodeContests_3260_minimumCost/1_refactored/refactor_18.rkt) | 8 |
|  4   | minimumCost/ | [highest](LeetCodeContests_3263_minimumCost/highest_heuristic.txt) |  [dir](LeetCodeContests_3263_minimumCost) |  [path-to-selected-high](LeetCodeContests_3263_minimumCost/3_refactored/refactor_95.rkt) | 7 |
