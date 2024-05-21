#lang racket

;; You are given a 2D 0-indexed array of strings, access_times, with size n. For each i where 0 <= i <= n - 1, access_times[i][0] represents the name of an employee, and access_times[i][1] represents the access time of that employee. All entries in access_times are within the same day.
;; The access time is represented as four digits using a 24-hour time format, for example, "0800" or "2250".
;; An employee is said to be high-access if he has accessed the system three or more times within a one-hour period.
;; Times with exactly one hour of difference are not considered part of the same one-hour period. For example, "0815" and "0915" are not part of the same one-hour period.
;; Access times at the start and end of the day are not counted within the same one-hour period. For example, "0005" and "2350" are not part of the same one-hour period.
;; Return a list that contains the names of high-access employees with any order you want.
;; Example 1:
;; Input: access_times = [["a","0549"],["b","0457"],["a","0532"],["a","0621"],["b","0540"]]
;; Output: ["a"]
;; Explanation: "a" has three access times in the one-hour period of [05:32, 06:31] which are 05:32, 05:49, and 06:21.
;; But "b" does not have more than two access times at all.
;; So the answer is ["a"].
;; Example 2:
;; Input: access_times = [["d","0002"],["c","0808"],["c","0829"],["e","0215"],["d","1508"],["d","1444"],["d","1410"],["c","0809"]]
;; Output: ["c","d"]
;; Explanation: "c" has three access times in the one-hour period of [08:08, 09:07] which are 08:08, 08:09, and 08:29.
;; "d" has also three access times in the one-hour period of [14:10, 15:09] which are 14:10, 14:44, and 15:08.
;; However, "e" has just one access time, so it can not be in the answer and the final answer is ["c","d"].
;; Example 3:
;; Input: access_times = [["cd","1025"],["ab","1025"],["cd","1046"],["cd","1055"],["ab","1124"],["ab","1120"]]
;; Output: ["ab","cd"]
;; Explanation: "ab" has three access times in the one-hour period of [10:25, 11:24] which are 10:25, 11:20, and 11:24.
;; "cd" has also three access times in the one-hour period of [10:25, 11:24] which are 10:25, 10:46, and 10:55.
;; So the answer is ["ab","cd"].
;; Constraints:
;; 1 <= access_times.length <= 100
;; access_times[i].length == 2
;; 1 <= access_times[i][0].length <= 10
;; access_times[i][0] consists only of English small letters.
;; access_times[i][1].length == 4
;; access_times[i][1] is in 24-hour time format.
;; access_times[i][1] consists only of '0' to '9'.
(define (time-to-minutes time)
  (+ (* 60 (string->number (substring time 0 2)))
     (string->number (substring time 2 4))))

;; Helper function to check if two times are within one hour
(define (within-one-hour? start end)
  (let ([start-min (time-to-minutes start)]
        [end-min (time-to-minutes end)])
    (and (>= end-min start-min)
         (< (- end-min start-min) 60))))

;; Group access times by employee
(define (group-by-employee access_times)
  (foldl (lambda (entry acc)
           (hash-update acc (first entry)
                        (lambda (lst) (cons (second entry) lst))
                        '()))
         (make-hash)
         access_times))

;; Check if any employee has high access within any one-hour period
(define (has-high-access? times)
  (let loop ([remaining times])
    (cond
      [(< (length remaining) 3) #f]
      [else
       (let ([start (first remaining)])
         (let ([count (length (takef (cdr remaining)
                                     (lambda (time) (within-one-hour? start time))))])
           (or (>= (+ count 1) 3)
               (loop (cdr remaining)))))])))

;; Filter and collect high access employees
(define (find-high-access-employees grouped-times)
  (for/list ([pair (in-hash grouped-times)]
             #:when (has-high-access? (sort (cdr pair) string<?)))
    (car pair)))

;; Main function to find high access employees
(define (findHighAccessEmployees access_times)
  (find-high-access-employees (group-by-employee access_times)))

;; Example usage:
(findHighAccessEmployees '(("a" "0549") ("b" "0457") ("a" "0532") ("a" "0621") ("b" "0540")))
(findHighAccessEmployees '(("d" "0002") ("c" "0808") ("c" "0829") ("e" "0215") ("d" "1508") ("d" "1444") ("d" "1410") ("c" "0809")))
(findHighAccessEmployees '(("cd" "1025") ("ab" "1025") ("cd" "1046") ("cd" "1055") ("ab" "1124") ("ab" "1120")))


(require rackunit)


(define (test-humaneval) 

  (let (( candidate findHighAccessEmployees))
    (check-within (candidate (list (list "a" "0549") (list "b" "0457") (list "a" "0532") (list "a" "0621") (list "b" "0540"))) (list "a") 0.001)
    (check-within (candidate (list (list "d" "0002") (list "c" "0808") (list "c" "0829") (list "e" "0215") (list "d" "1508") (list "d" "1444") (list "d" "1410") (list "c" "0809"))) (list "c" "d") 0.001)
    (check-within (candidate (list (list "cd" "1025") (list "ab" "1025") (list "cd" "1046") (list "cd" "1055") (list "ab" "1124") (list "ab" "1120"))) (list "ab" "cd") 0.001)
    (check-within (candidate (list (list "baipstt" "1456"))) (list ) 0.001)
    (check-within (candidate (list (list "bouo" "1126"))) (list ) 0.001)
    (check-within (candidate (list (list "cavfbqg" "2304"))) (list ) 0.001)
    (check-within (candidate (list (list "cenjcq" "1007"))) (list ) 0.001)
    (check-within (candidate (list (list "cqotrwqcaq" "0131"))) (list ) 0.001)
    (check-within (candidate (list (list "downbuk" "1951"))) (list ) 0.001)
    (check-within (candidate (list (list "dqsoiyz" "2204"))) (list ) 0.001)
    (check-within (candidate (list (list "duzeyrov" "0243"))) (list ) 0.001)
    (check-within (candidate (list (list "erfg" "1223"))) (list ) 0.001)
    (check-within (candidate (list (list "fwhefd" "2026"))) (list ) 0.001)
    (check-within (candidate (list (list "gbefbne" "0911"))) (list ) 0.001)
    (check-within (candidate (list (list "gp" "1540"))) (list ) 0.001)
    (check-within (candidate (list (list "ht" "1319"))) (list ) 0.001)
    (check-within (candidate (list (list "inahnsjdqz" "1750"))) (list ) 0.001)
    (check-within (candidate (list (list "jwxvijxo" "0851"))) (list ) 0.001)
    (check-within (candidate (list (list "kibwwvjuez" "0716"))) (list ) 0.001)
    (check-within (candidate (list (list "lvry" "0706"))) (list ) 0.001)
    (check-within (candidate (list (list "mbsyxxfzjf" "0114"))) (list ) 0.001)
    (check-within (candidate (list (list "mlehvzqb" "1620"))) (list ) 0.001)
    (check-within (candidate (list (list "mmgat" "0516"))) (list ) 0.001)
    (check-within (candidate (list (list "mxatapbs" "2240"))) (list ) 0.001)
    (check-within (candidate (list (list "mzxbgtfc" "1531"))) (list ) 0.001)
    (check-within (candidate (list (list "nnhh" "1445"))) (list ) 0.001)
    (check-within (candidate (list (list "o" "1414"))) (list ) 0.001)
    (check-within (candidate (list (list "qaxqifxxww" "1557"))) (list ) 0.001)
    (check-within (candidate (list (list "rjy" "0200"))) (list ) 0.001)
    (check-within (candidate (list (list "sgpgh" "0539"))) (list ) 0.001)
    (check-within (candidate (list (list "sxx" "0325"))) (list ) 0.001)
    (check-within (candidate (list (list "tkvgcf" "1645"))) (list ) 0.001)
    (check-within (candidate (list (list "ttk" "0304"))) (list ) 0.001)
    (check-within (candidate (list (list "un" "0833"))) (list ) 0.001)
    (check-within (candidate (list (list "vlifcdn" "0731"))) (list ) 0.001)
    (check-within (candidate (list (list "w" "2224"))) (list ) 0.001)
    (check-within (candidate (list (list "wkmehwsg" "2023"))) (list ) 0.001)
    (check-within (candidate (list (list "y" "1005"))) (list ) 0.001)
    (check-within (candidate (list (list "ynnale" "1331"))) (list ) 0.001)
    (check-within (candidate (list (list "yt" "0900"))) (list ) 0.001)
    (check-within (candidate (list (list "zbgzk" "0527"))) (list ) 0.001)
    (check-within (candidate (list (list "a" "0039") (list "a" "0042"))) (list ) 0.001)
    (check-within (candidate (list (list "ajhzcltqse" "0605") (list "ajhzcltqse" "0558"))) (list ) 0.001)
    (check-within (candidate (list (list "cbaqsymoi" "0001") (list "cbaqsymoi" "0004"))) (list ) 0.001)
    (check-within (candidate (list (list "df" "1958") (list "df" "2002"))) (list ) 0.001)
    (check-within (candidate (list (list "dhmnhvou" "0529") (list "dhmnhvou" "0531"))) (list ) 0.001)
    (check-within (candidate (list (list "epghzrog" "0333") (list "epghzrog" "0333"))) (list ) 0.001)
    (check-within (candidate (list (list "gda" "1529") (list "gda" "1534"))) (list ) 0.001)
    (check-within (candidate (list (list "gjhtgm" "2207") (list "gjhtgm" "2156"))) (list ) 0.001)
    (check-within (candidate (list (list "gsd" "2030") (list "gsd" "2046"))) (list ) 0.001)
    (check-within (candidate (list (list "gsstuktwm" "1403") (list "gsstuktwm" "1357"))) (list ) 0.001)
    (check-within (candidate (list (list "h" "2159") (list "h" "2203"))) (list ) 0.001)
    (check-within (candidate (list (list "hxrdffk" "1736") (list "hxrdffk" "1724"))) (list ) 0.001)
    (check-within (candidate (list (list "iaxsnenx" "2037") (list "iaxsnenx" "2050"))) (list ) 0.001)
    (check-within (candidate (list (list "ikwjvflxq" "0055") (list "ikwjvflxq" "0056"))) (list ) 0.001)
    (check-within (candidate (list (list "jkgjmku" "0743") (list "jkgjmku" "0754"))) (list ) 0.001)
    (check-within (candidate (list (list "jkw" "0241") (list "jkw" "0235"))) (list ) 0.001)
    (check-within (candidate (list (list "jykugiprxf" "1633") (list "jykugiprxf" "1641"))) (list ) 0.001)
    (check-within (candidate (list (list "kdxw" "1338") (list "kdxw" "1336"))) (list ) 0.001)
    (check-within (candidate (list (list "kenltmrg" "0932") (list "kenltmrg" "0941"))) (list ) 0.001)
    (check-within (candidate (list (list "kptjrr" "1356") (list "kptjrr" "1349"))) (list ) 0.001)
    (check-within (candidate (list (list "mcd" "1333") (list "mcd" "1325"))) (list ) 0.001)
    (check-within (candidate (list (list "mhkizga" "1552") (list "mhkizga" "1551"))) (list ) 0.001)
    (check-within (candidate (list (list "monxm" "1748") (list "monxm" "1742"))) (list ) 0.001)
    (check-within (candidate (list (list "msjydtinfy" "1301") (list "msjydtinfy" "1245"))) (list ) 0.001)
    (check-within (candidate (list (list "myhdmu" "1407") (list "myhdmu" "1419"))) (list ) 0.001)
    (check-within (candidate (list (list "nyoezc" "1050") (list "nyoezc" "1041"))) (list ) 0.001)
    (check-within (candidate (list (list "oksvrskxch" "0053") (list "oksvrskxch" "0111"))) (list ) 0.001)
    (check-within (candidate (list (list "pxc" "1915") (list "pxc" "1910"))) (list ) 0.001)
    (check-within (candidate (list (list "qedxyj" "0609") (list "qedxyj" "0614"))) (list ) 0.001)
    (check-within (candidate (list (list "qmslkyxnph" "0946") (list "qmslkyxnph" "0958"))) (list ) 0.001)
    (check-within (candidate (list (list "r" "0206") (list "r" "0202"))) (list ) 0.001)
    (check-within (candidate (list (list "r" "2041") (list "r" "2052"))) (list ) 0.001)
    (check-within (candidate (list (list "rf" "2205") (list "rf" "2203"))) (list ) 0.001)
    (check-within (candidate (list (list "rswegeuhqd" "0235") (list "rswegeuhqd" "0238"))) (list ) 0.001)
    (check-within (candidate (list (list "skfgl" "0718") (list "skfgl" "0712"))) (list ) 0.001)
    (check-within (candidate (list (list "smnnl" "2329") (list "smnnl" "2340"))) (list ) 0.001)
    (check-within (candidate (list (list "tpbbxpx" "0409") (list "tpbbxpx" "0408"))) (list ) 0.001)
    (check-within (candidate (list (list "uiqxqp" "0515") (list "uiqxqp" "0516"))) (list ) 0.001)
    (check-within (candidate (list (list "uyuz" "1530") (list "uyuz" "1543"))) (list ) 0.001)
    (check-within (candidate (list (list "vfeunkee" "1500") (list "vfeunkee" "1508"))) (list ) 0.001)
    (check-within (candidate (list (list "wbyd" "1848") (list "wbyd" "1839"))) (list ) 0.001)
    (check-within (candidate (list (list "x" "0522") (list "x" "0506"))) (list ) 0.001)
    (check-within (candidate (list (list "xhrhdy" "1455") (list "xhrhdy" "1454"))) (list ) 0.001)
    (check-within (candidate (list (list "xmsypay" "1605") (list "xmsypay" "1612"))) (list ) 0.001)
    (check-within (candidate (list (list "xy" "0015") (list "xy" "0021"))) (list ) 0.001)
    (check-within (candidate (list (list "ydtnnpzw" "0516") (list "ydtnnpzw" "0520"))) (list ) 0.001)
    (check-within (candidate (list (list "zh" "2348") (list "zh" "2334"))) (list ) 0.001)
    (check-within (candidate (list (list "zinywjn" "0017") (list "zinywjn" "0019"))) (list ) 0.001)
    (check-within (candidate (list (list "aczdfmsd" "0317") (list "aczdfmsd" "0314") (list "aczdfmsd" "0320"))) (list "aczdfmsd") 0.001)
    (check-within (candidate (list (list "bsluadumi" "1518") (list "bsluadumi" "1516") (list "bsluadumi" "1510"))) (list "bsluadumi") 0.001)
    (check-within (candidate (list (list "ckrdpxq" "1122") (list "ckrdpxq" "1125") (list "ckrdpxq" "1121"))) (list "ckrdpxq") 0.001)
    (check-within (candidate (list (list "fe" "1320") (list "fe" "1326") (list "fe" "1331"))) (list "fe") 0.001)
    (check-within (candidate (list (list "ff" "1508") (list "ff" "1508") (list "ff" "1516"))) (list "ff") 0.001)
    (check-within (candidate (list (list "fnlmbcedu" "0052") (list "fnlmbcedu" "0103") (list "fnlmbcedu" "0055"))) (list "fnlmbcedu") 0.001)
    (check-within (candidate (list (list "hffgwjjve" "0159") (list "hffgwjjve" "0152") (list "hffgwjjve" "0159"))) (list "hffgwjjve") 0.001)
    (check-within (candidate (list (list "ivlvfgwsx" "0122") (list "ivlvfgwsx" "0135") (list "ivlvfgwsx" "0139"))) (list "ivlvfgwsx") 0.001)
    (check-within (candidate (list (list "jlfnksqlt" "0304") (list "jlfnksqlt" "0252") (list "jlfnksqlt" "0304"))) (list "jlfnksqlt") 0.001)
    (check-within (candidate (list (list "jy" "0647") (list "jy" "0652") (list "jy" "0704"))) (list "jy") 0.001)
    (check-within (candidate (list (list "kchzzdso" "2329") (list "kchzzdso" "2326") (list "kchzzdso" "2329"))) (list "kchzzdso") 0.001)
))

(test-humaneval)