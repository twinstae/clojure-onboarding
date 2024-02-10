(ns day1
  (:require
   [clojure.string :as str]))

(def data-file (slurp "resources/day1/input1.txt"))
(def input-lines (str/split-lines data-file))

(defn sum [s] (reduce + s))


(def input (map parse-long input-lines))
(comment
  ;; part 1

  (sum input))

  ;; part2
(->> (cycle input)
     (reduce
      (fn [[seen current] change]
        (let [next (+ current change)]
          (if (contains? seen next)
            (reduced next)
            [(conj seen next) next])))
      [#{0} 0]))
