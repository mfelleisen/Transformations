#lang racket

;; Function: getWordsInLongestSubsequence
;; Description: Returns the longest subsequence of words such that consecutive words are from different groups.
;; You are given an integer n, a 0-indexed string array words, and a 0-indexed binary array groups, both arrays having length n.
;; You need to select the longest subsequence from an array of indices [0, 1, ..., n - 1], such that for the subsequence denoted as [i0, i1, ..., ik - 1] having length k, groups[ij] != groups[ij + 1], for each j where 0 < j + 1 < k.
;; Return a string array containing the words corresponding to the indices (in order) in the selected subsequence. If there are multiple answers, return any of them.
;; A subsequence of an array is a new array that is formed from the original array by deleting some (possibly none) of the elements without disturbing the relative positions of the remaining elements.
;; Note: strings in words may be unequal in length.
;; Example 1:
;; Input: n = 3, words = ["e","a","b"], groups = [0,0,1]
;; Output: ["e","b"]
;; Explanation: A subsequence that can be selected is [0,2] because groups[0] != groups[2].
;; So, a valid answer is [words[0],words[2]] = ["e","b"].
;; Another subsequence that can be selected is [1,2] because groups[1] != groups[2].
;; This results in [words[1],words[2]] = ["a","b"].
;; It is also a valid answer.
;; It can be shown that the length of the longest subsequence of indices that satisfies the condition is 2.
;; Example 2:
;; Input: n = 4, words = ["a","b","c","d"], groups = [1,0,1,1]
;; Output: ["a","b","c"]
;; Explanation: A subsequence that can be selected is [0,1,2] because groups[0] != groups[1] and groups[1] != groups[2].
;; So, a valid answer is [words[0],words[1],words[2]] = ["a","b","c"].
;; Another subsequence that can be selected is [0,1,3] because groups[0] != groups[1] and groups[1] != groups[3].
;; This results in [words[0],words[1],words[3]] = ["a","b","d"].
;; It is also a valid answer.
;; It can be shown that the length of the longest subsequence of indices that satisfies the condition is 3.
;; Constraints:
;;  * 1 <= n == words.length == groups.length <= 100
;;  * 1 <= words[i].length <= 10
;;  * 0 <= groups[i] < 2
;;  * words consists of distinct strings.
;;  * words[i] consists of lowercase English letters.
(define (getWordsInLongestSubsequence n words groups)
  ;; Helper function to recursively build the longest subsequence
  (define (build-subsequence idx last-group subsequence)
    (if (>= idx n)
        (reverse subsequence) ; Return reversed subsequence when all elements are processed
        (let ([current-group (list-ref groups idx)]
              [current-word (list-ref words idx)])
          (if (not (= current-group last-group))
              (build-subsequence (add1 idx) current-group (cons current-word subsequence)) ; Cons current word
              (build-subsequence (add1 idx) last-group subsequence))))) ; Continue with the same subsequence

  (if (= n 0)
      '() ; Return empty list if n is 0
      (build-subsequence 1 (first groups) (list (first words))))) ; Initialize with the first element

;; Example usage:
(define n 4)
(define words '("a" "b" "c" "d"))
(define groups '(1 0 1 1))
(getWordsInLongestSubsequence n words groups) ; Output: '("a" "b" "c")

(require rackunit)


(define (test-humaneval) 

  (let (( candidate getWordsInLongestSubsequence))
    (check-within (candidate 3 (list "e" "a" "b") (list 0 0 1)) (list "e" "b") 0.001)
    (check-within (candidate 4 (list "a" "b" "c" "d") (list 1 0 1 1)) (list "a" "b" "c") 0.001)
    (check-within (candidate 1 (list "c") (list 0)) (list "c") 0.001)
    (check-within (candidate 1 (list "d") (list 1)) (list "d") 0.001)
    (check-within (candidate 1 (list "e") (list 0)) (list "e") 0.001)
    (check-within (candidate 1 (list "fe") (list 0)) (list "fe") 0.001)
    (check-within (candidate 1 (list "frl") (list 1)) (list "frl") 0.001)
    (check-within (candidate 1 (list "ha") (list 1)) (list "ha") 0.001)
    (check-within (candidate 1 (list "l") (list 0)) (list "l") 0.001)
    (check-within (candidate 1 (list "n") (list 1)) (list "n") 0.001)
    (check-within (candidate 1 (list "s") (list 1)) (list "s") 0.001)
    (check-within (candidate 2 (list "d" "g") (list 0 1)) (list "d" "g") 0.001)
    (check-within (candidate 2 (list "lr" "h") (list 0 0)) (list "lr") 0.001)
    (check-within (candidate 2 (list "wx" "h") (list 0 1)) (list "wx" "h") 0.001)
    (check-within (candidate 2 (list "yw" "n") (list 0 1)) (list "yw" "n") 0.001)
    (check-within (candidate 2 (list "z" "n") (list 0 0)) (list "z") 0.001)
    (check-within (candidate 2 (list "zr" "a") (list 0 0)) (list "zr") 0.001)
    (check-within (candidate 3 (list "h" "vv" "kp") (list 0 1 0)) (list "h" "vv" "kp") 0.001)
    (check-within (candidate 3 (list "m" "v" "y") (list 0 1 0)) (list "m" "v" "y") 0.001)
    (check-within (candidate 3 (list "o" "cfy" "en") (list 1 0 0)) (list "o" "cfy") 0.001)
    (check-within (candidate 3 (list "tu" "rv" "bn") (list 0 0 0)) (list "tu") 0.001)
    (check-within (candidate 4 (list "c" "f" "y" "i") (list 1 0 1 1)) (list "c" "f" "y") 0.001)
    (check-within (candidate 4 (list "c" "w" "h" "s") (list 0 0 0 1)) (list "c" "s") 0.001)
    (check-within (candidate 4 (list "d" "a" "v" "b") (list 1 0 0 1)) (list "d" "a" "b") 0.001)
    (check-within (candidate 4 (list "hh" "svj" "a" "nr") (list 1 1 1 1)) (list "hh") 0.001)
    (check-within (candidate 4 (list "im" "j" "xq" "cjs") (list 1 0 0 1)) (list "im" "j" "cjs") 0.001)
    (check-within (candidate 4 (list "m" "dkg" "r" "h") (list 1 1 1 0)) (list "m" "h") 0.001)
    (check-within (candidate 4 (list "ow" "qay" "r" "j") (list 1 1 1 1)) (list "ow") 0.001)
    (check-within (candidate 4 (list "r" "k" "pb" "x") (list 0 0 1 0)) (list "r" "pb" "x") 0.001)
    (check-within (candidate 4 (list "sq" "do" "bcj" "nm") (list 0 1 1 0)) (list "sq" "do" "nm") 0.001)
    (check-within (candidate 4 (list "sz" "mq" "j" "u") (list 0 0 1 0)) (list "sz" "j" "u") 0.001)
    (check-within (candidate 4 (list "x" "nf" "p" "asn") (list 1 1 1 1)) (list "x") 0.001)
    (check-within (candidate 4 (list "z" "tkt" "x" "swy") (list 1 0 1 1)) (list "z" "tkt" "x") 0.001)
    (check-within (candidate 5 (list "ht" "lw" "ax" "vi" "fo") (list 0 0 1 0 0)) (list "ht" "ax" "vi") 0.001)
    (check-within (candidate 5 (list "mc" "kh" "x" "q" "z") (list 0 0 1 1 0)) (list "mc" "x" "z") 0.001)
    (check-within (candidate 5 (list "n" "fg" "fy" "tv" "gv") (list 1 1 1 1 1)) (list "n") 0.001)
    (check-within (candidate 5 (list "n" "l" "e" "d" "m") (list 1 1 0 1 1)) (list "n" "e" "d") 0.001)
    (check-within (candidate 5 (list "n" "m" "g" "b" "d") (list 0 0 1 0 0)) (list "n" "g" "b") 0.001)
    (check-within (candidate 5 (list "nz" "zwt" "hig" "s" "jze") (list 1 1 1 0 1)) (list "nz" "s" "jze") 0.001)
    (check-within (candidate 5 (list "o" "i" "b" "k" "kz") (list 0 0 1 1 1)) (list "o" "b") 0.001)
    (check-within (candidate 5 (list "r" "o" "k" "d" "f") (list 0 0 0 1 1)) (list "r" "d") 0.001)
    (check-within (candidate 5 (list "sfh" "exd" "j" "w" "gc") (list 1 0 1 1 1)) (list "sfh" "exd" "j") 0.001)
    (check-within (candidate 5 (list "v" "f" "k" "l" "p") (list 0 0 1 0 0)) (list "v" "k" "l") 0.001)
    (check-within (candidate 5 (list "vbd" "ua" "muo" "mu" "qi") (list 0 0 0 1 0)) (list "vbd" "mu" "qi") 0.001)
    (check-within (candidate 5 (list "we" "ch" "tl" "yx" "utx") (list 1 0 0 1 1)) (list "we" "ch" "yx") 0.001)
    (check-within (candidate 5 (list "x" "vlk" "tds" "dfn" "kr") (list 0 0 1 1 0)) (list "x" "tds" "kr") 0.001)
    (check-within (candidate 5 (list "y" "j" "u" "r" "f") (list 0 0 1 1 0)) (list "y" "u" "f") 0.001)
    (check-within (candidate 5 (list "y" "r" "z" "x" "q") (list 0 1 0 1 1)) (list "y" "r" "z" "x") 0.001)
    (check-within (candidate 5 (list "yc" "fgq" "gg" "og" "tca") (list 0 1 1 1 0)) (list "yc" "fgq" "tca") 0.001)
    (check-within (candidate 5 (list "z" "d" "p" "c" "m") (list 0 0 0 0 0)) (list "z") 0.001)
    (check-within (candidate 6 (list "c" "i" "to" "kv" "op" "u") (list 0 0 1 0 0 0)) (list "c" "to" "kv") 0.001)
    (check-within (candidate 6 (list "d" "h" "e" "k" "j" "r") (list 0 1 1 0 1 0)) (list "d" "h" "k" "j" "r") 0.001)
    (check-within (candidate 6 (list "l" "f" "v" "b" "w" "k") (list 1 0 1 1 0 0)) (list "l" "f" "v" "w") 0.001)
    (check-within (candidate 6 (list "lj" "vf" "pa" "w" "z" "q") (list 0 0 1 0 0 0)) (list "lj" "pa" "w") 0.001)
    (check-within (candidate 7 (list "cd" "oki" "ho" "oi" "m" "yvy" "i") (list 1 1 0 1 1 1 1)) (list "cd" "ho" "oi") 0.001)
    (check-within (candidate 7 (list "exb" "c" "oq" "lq" "xh" "zmo" "aug") (list 1 1 0 1 1 0 0)) (list "exb" "oq" "lq" "zmo") 0.001)
    (check-within (candidate 7 (list "f" "r" "k" "h" "m" "v" "p") (list 1 0 0 0 1 0 0)) (list "f" "r" "m" "v") 0.001)
    (check-within (candidate 7 (list "fd" "fc" "jm" "z" "lg" "kl" "ux") (list 0 1 0 1 0 1 0)) (list "fd" "fc" "jm" "z" "lg" "kl" "ux") 0.001)
    (check-within (candidate 7 (list "ft" "iw" "m" "v" "gx" "d" "pm") (list 1 1 1 0 1 1 1)) (list "ft" "v" "gx") 0.001)
    (check-within (candidate 7 (list "lma" "i" "rt" "xar" "bfx" "np" "x") (list 1 1 1 1 1 0 1)) (list "lma" "np" "x") 0.001)
    (check-within (candidate 7 (list "nsv" "r" "o" "qo" "pb" "xqv" "clb") (list 1 1 0 0 0 0 0)) (list "nsv" "o") 0.001)
    (check-within (candidate 7 (list "p" "qdb" "zcd" "l" "tv" "ln" "ogb") (list 1 1 0 1 0 0 1)) (list "p" "zcd" "l" "tv" "ogb") 0.001)
    (check-within (candidate 7 (list "z" "cee" "j" "jqu" "w" "ljr" "k") (list 1 0 1 1 0 0 1)) (list "z" "cee" "j" "w" "k") 0.001)
    (check-within (candidate 8 (list "h" "p" "q" "t" "j" "a" "c" "n") (list 0 1 1 1 0 0 1 1)) (list "h" "p" "j" "c") 0.001)
    (check-within (candidate 8 (list "r" "v" "c" "t" "d" "a" "x" "o") (list 1 1 0 1 1 0 0 1)) (list "r" "c" "t" "a" "o") 0.001)
    (check-within (candidate 8 (list "u" "l" "a" "y" "j" "s" "h" "q") (list 0 0 0 0 0 1 0 0)) (list "u" "s" "h") 0.001)
    (check-within (candidate 8 (list "x" "mr" "yyf" "l" "z" "q" "zvj" "zqt") (list 0 1 1 1 0 1 1 0)) (list "x" "mr" "z" "q" "zqt") 0.001)
    (check-within (candidate 8 (list "y" "x" "i" "xtm" "ze" "n" "cma" "dgk") (list 0 1 0 0 1 1 0 0)) (list "y" "x" "i" "ze" "cma") 0.001)
    (check-within (candidate 8 (list "yun" "x" "zpp" "bpr" "ii" "ezg" "dn" "k") (list 0 1 1 1 1 0 1 0)) (list "yun" "x" "ezg" "dn" "k") 0.001)
    (check-within (candidate 9 (list "ckr" "iz" "top" "of" "sb" "wv" "hb" "da" "wd") (list 1 1 0 1 1 0 0 0 1)) (list "ckr" "top" "of" "wv" "wd") 0.001)
    (check-within (candidate 9 (list "g" "h" "u" "n" "w" "o" "f" "p" "m") (list 1 0 0 1 1 0 0 1 0)) (list "g" "h" "n" "o" "p" "m") 0.001)
    (check-within (candidate 9 (list "ilw" "t" "dyy" "irz" "oxy" "k" "rfj" "hi" "zxe") (list 1 1 1 0 1 1 0 1 1)) (list "ilw" "irz" "oxy" "rfj" "hi") 0.001)
    (check-within (candidate 9 (list "l" "iuz" "d" "tfw" "mu" "a" "rp" "mrb" "wnl") (list 1 1 1 1 1 0 1 1 0)) (list "l" "a" "rp" "wnl") 0.001)
    (check-within (candidate 9 (list "mc" "b" "yr" "cj" "zk" "wi" "esm" "yu" "cw") (list 0 0 1 1 0 0 1 1 0)) (list "mc" "yr" "zk" "esm" "cw") 0.001)
    (check-within (candidate 9 (list "nw" "hx" "ygc" "vjo" "jmv" "p" "juv" "b" "y") (list 0 1 0 0 1 0 0 1 0)) (list "nw" "hx" "ygc" "jmv" "p" "b" "y") 0.001)
    (check-within (candidate 9 (list "osq" "qiw" "h" "tc" "xg" "tvt" "fqp" "zq" "b") (list 0 0 1 0 1 1 0 1 1)) (list "osq" "h" "tc" "xg" "fqp" "zq") 0.001)
    (check-within (candidate 9 (list "vr" "lw" "e" "g" "dz" "kf" "qe" "h" "p") (list 1 0 0 1 1 0 0 0 0)) (list "vr" "lw" "g" "kf") 0.001)
    (check-within (candidate 10 (list "gy" "nd" "l" "hr" "i" "qf" "zz" "nq" "e" "oa") (list 0 1 0 0 1 0 1 1 1 0)) (list "gy" "nd" "l" "i" "qf" "zz" "oa") 0.001)
    (check-within (candidate 10 (list "j" "r" "h" "t" "z" "b" "a" "s" "v" "q") (list 1 0 1 1 1 1 0 0 0 1)) (list "j" "r" "h" "a" "q") 0.001)
    (check-within (candidate 10 (list "k" "f" "u" "h" "x" "w" "c" "e" "l" "p") (list 0 1 1 1 1 1 1 1 1 0)) (list "k" "f" "p") 0.001)
    (check-within (candidate 10 (list "lj" "huy" "lg" "h" "o" "b" "ava" "ay" "r" "us") (list 1 1 1 1 0 0 1 1 1 1)) (list "lj" "o" "ava") 0.001)
    (check-within (candidate 10 (list "m" "d" "xv" "dp" "nq" "xi" "e" "g" "n" "qw") (list 1 0 1 1 1 1 0 1 0 1)) (list "m" "d" "xv" "e" "g" "n" "qw") 0.001)
    (check-within (candidate 10 (list "n" "c" "y" "h" "w" "m" "g" "t" "x" "v") (list 1 1 1 0 0 1 0 0 0 1)) (list "n" "h" "m" "g" "v") 0.001)
    (check-within (candidate 10 (list "o" "w" "l" "g" "m" "x" "f" "q" "c" "v") (list 1 1 1 0 1 1 1 0 0 1)) (list "o" "g" "m" "q" "v") 0.001)
    (check-within (candidate 10 (list "p" "mw" "m" "xld" "j" "jv" "n" "so" "pkd" "rwt") (list 0 0 1 0 1 1 0 0 1 1)) (list "p" "m" "xld" "j" "n" "pkd") 0.001)
    (check-within (candidate 10 (list "vyv" "msl" "d" "bu" "ubl" "bgk" "sz" "njv" "pf" "s") (list 1 0 1 1 0 0 1 0 1 0)) (list "vyv" "msl" "d" "ubl" "sz" "njv" "pf" "s") 0.001)
    (check-within (candidate 10 (list "y" "mz" "lt" "ur" "o" "m" "djh" "tb" "w" "j") (list 0 0 1 0 1 1 0 1 0 0)) (list "y" "lt" "ur" "o" "djh" "tb" "w") 0.001)
    (check-within (candidate 10 (list "y" "s" "i" "v" "a" "w" "l" "q" "k" "t") (list 0 1 1 1 0 1 1 1 0 0)) (list "y" "s" "a" "w" "k") 0.001)
    (check-within (candidate 11 (list "a" "tea" "ldt" "ybm" "zkw" "r" "d" "dms" "le" "u" "ze") (list 1 1 0 0 0 1 1 1 1 0 1)) (list "a" "ldt" "r" "u" "ze") 0.001)
    (check-within (candidate 11 (list "c" "o" "e" "r" "x" "w" "b" "d" "h" "y" "z") (list 1 0 1 0 1 0 1 1 1 0 1)) (list "c" "o" "e" "r" "x" "w" "b" "y" "z") 0.001)
    (check-within (candidate 11 (list "chu" "a" "qdx" "fgd" "qe" "bqc" "x" "kbx" "sv" "ly" "br") (list 1 0 0 0 0 0 1 0 1 1 0)) (list "chu" "a" "x" "kbx" "sv" "br") 0.001)
    (check-within (candidate 11 (list "ec" "jdf" "b" "wa" "kjd" "bb" "ty" "yi" "ybw" "ilj" "cv") (list 0 1 0 1 1 1 1 1 1 0 1)) (list "ec" "jdf" "b" "wa" "ilj" "cv") 0.001)
    (check-within (candidate 11 (list "ew" "isn" "fl" "mg" "pdg" "d" "p" "hh" "e" "y" "whm") (list 0 0 1 1 0 0 0 0 0 1 1)) (list "ew" "fl" "pdg" "y") 0.001)
    (check-within (candidate 11 (list "h" "o" "d" "y" "r" "c" "p" "b" "g" "j" "k") (list 1 1 0 1 1 0 1 1 0 0 0)) (list "h" "d" "y" "c" "p" "g") 0.001)
    (check-within (candidate 11 (list "ipr" "l" "zy" "j" "h" "hdt" "m" "d" "pd" "nv" "wy") (list 1 1 1 1 0 1 0 1 1 1 1)) (list "ipr" "h" "hdt" "m" "d") 0.001)
    (check-within (candidate 11 (list "j" "g" "go" "a" "f" "bg" "o" "l" "ze" "kq" "w") (list 0 0 1 0 1 0 0 0 1 0 1)) (list "j" "go" "a" "f" "bg" "ze" "kq" "w") 0.001)
    (check-within (candidate 11 (list "j" "r" "a" "g" "x" "b" "y" "v" "k" "i" "c") (list 0 1 0 0 0 0 1 1 0 0 0)) (list "j" "r" "a" "y" "k") 0.001)
    (check-within (candidate 11 (list "kgo" "han" "nlu" "tv" "us" "pk" "xw" "cxc" "eml" "v" "msz") (list 1 0 0 1 0 0 1 0 1 1 0)) (list "kgo" "han" "tv" "us" "xw" "cxc" "eml" "msz") 0.001)
    (check-within (candidate 11 (list "kh" "op" "ij" "te" "hk" "pmt" "v" "ne" "en" "b" "zuj") (list 0 0 1 1 1 0 1 1 0 1 1)) (list "kh" "ij" "pmt" "v" "en" "b") 0.001)
    (check-within (candidate 11 (list "ms" "t" "oz" "x" "pw" "ik" "d" "gj" "z" "ps" "i") (list 1 1 0 1 0 0 1 1 0 0 1)) (list "ms" "oz" "x" "pw" "d" "z" "i") 0.001)
))

(test-humaneval)