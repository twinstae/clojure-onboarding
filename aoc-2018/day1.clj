(ns day1
  (:require
   [clojure.java.io :as io]))


(comment
  ;; part 1 
  (def input (with-open [rdr (io/reader "resources/day1/input1.txt")]
               (map parse-long (line-seq rdr))))

  (prn (reduce + input)) ; Execution error (IOException) at java.io.BufferedReader/ensureOpen (BufferedReader.java:121).
  ; Stream closed


  (with-open [rdr (io/reader "resources/day1/input1.txt")]
    (let [input (map parse-long (line-seq rdr))]
      (reduce + input)))
  )

  ;; part2
(with-open [rdr (io/reader "resources/day1/input1.txt")]
  (let [input (map parse-long (line-seq rdr))]
   (->> (cycle input)
        (reduce
         (fn [[seen current] change]
           (let [next (+ current change)]
             (if (contains? seen next)
               (reduced next)
               [(conj seen next) next])))
         [#{0} 0]))))

