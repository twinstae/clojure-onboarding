#_{:clj-kondo/ignore [:namespace-name-mismatch]}
(ns aoc-2018.day1
  (:require
   [clojure.string :as str]))

(def data-file (slurp "resources/day1/input1.txt"))
(def input-lines (str/split-lines data-file))

(defn sum [s] (reduce + s))

(comment
  ;; part 1
  (map parse-long input-lines)
  (sum (map parse-long input-lines)))

  ;; part2
(->> (cycle (map parse-long input-lines))
     (reduce
      (fn [[seen current] change]
        (let [next (+ current change)]
          (if (contains? seen next)
            (reduced next)
            [(conj seen next) next])))
      [#{0} 0]))
