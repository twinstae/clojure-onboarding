#_{:clj-kondo/ignore [:namespace-name-mismatch]}
(ns aoc-2018.day1
  (:require
   [clojure.string :as str]))


(def root "/home/amikojhk/github/clojure-onboarding/")
(def data-file (slurp (str root "aoc-2018/day1/input1.txt")))
(def input-lines (str/split-lines data-file))

(defn sum [s] (reduce + s))

(defn changes [input] (map (fn [n] (Integer/parseInt n)) input))

(comment
  ;; part 1
  (changes input-lines)
  (sum (changes input-lines)))
