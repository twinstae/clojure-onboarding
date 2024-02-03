#_{:clj-kondo/ignore [:namespace-name-mismatch]}
(ns aoc-2018.day2
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [is]]))

(def root "/home/amikojhk/github/clojure-onboarding/")
(def data-file (slurp (str root "aoc-2018/day2/input1.txt")))
(def input-lines (str/split-lines data-file))


(defn check
  "받은 id에 같은 문자가 2번 혹은 3번 포함되었는지를 각각 boolean으로 반환합니다"
  [id]
  (let [dict (set/map-invert (frequencies id))]
    [(contains? dict 2) (contains? dict 3)]))

(is (= (check "abcdef") [false false]))
(is (= (check "bababc") [true true]))
(is (= (check "abbcde") [true false]))
(is (= (check "abcccd") [false true]))
(is (= (check "aabcdd") [true false]))
(is (= (check "abcdee") [true false]))
(is (= (check "ababab") [false true]))

(defn count-if
  [predicate seq]
  (count (filter predicate seq)))

(defn check-sum
  [input-lines]
  (let [result (map check input-lines)]
    (* (count-if #(get %1 0) result)
       (count-if #(get %1 1) result))))

(comment
  (check-sum input-lines))
