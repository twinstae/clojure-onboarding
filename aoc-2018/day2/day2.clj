#_{:clj-kondo/ignore [:namespace-name-mismatch]}
(ns aoc-2018.day2
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def data-file (slurp "resources/day2/input1.txt"))
(def input-lines (str/split-lines data-file))


(defn check
  "받은 id에 같은 문자가 2번 혹은 3번 포함되었는지를 각각 boolean으로 반환합니다"
  [id]
  (let [dict (set/map-invert (frequencies id))]
    [(contains? dict 2) (contains? dict 3)]))

(deftest check-test
  (testing "중복 없음"
    (is (= (check "abcdef") [false false])))
  (testing "2번만 있음"
    (is (= (check "abbcde") [true false]))
    (is (= (check "aabcdd") [true false]))
    (is (= (check "abcdee") [true false])))
  (testing "3번만 있음"
    (is (= (check "abcccd") [false true]))
    (is (= (check "ababab") [false true])))
  (testing "2번 3번 둘 다 있음"
    (is (= (check "bababc") [true true]))))


(defn check-sum
  [input-lines]
  (let [result (map check input-lines)]
    (* (count (filter #(get %1 0) result))
       (count (filter #(get %1 1) result)))))

(comment
  (check-sum input-lines))

;; part 2
(defn zip
  [a b]
  (map vector a b))

(defn compare-str
  [[a b]]
  (reduce
   (fn [[diff common] [x y]]
     (if (= x y)
       [diff (conj common x)]
       [(inc diff) common]))
   [0, []]
   (zip a b)))

(def t
  ["abcde" "fghij" "fguij"])

(defn combinations-2
  [seq] ;; seq에는 중복이 없다고 가정합니다
  (->>
   (mapcat (fn [a] (map (fn [b] [a b]) seq)) seq) ;; flatMap + map
   (filter (fn [[a b]] (not= a b))))) ;; 자기 자신과의 조합은 걸러줍니다

(defn answer-2
  [input]
  (->> input
       (combinations-2) ;; 비교하기 위해 조합을 구합니다
       (map compare-str) ;; 비교해서 다른 글자수와 공통 부분들을 구합니다
       (filter (fn [[diff]] (= diff 1))) ;; 1글자만 다른 것 중에
       (distinct) ;; 중복을 제거하고
       (first) ;; 첫 번째 항목에서
       (second) ;; 공통부분의 문자 sequence를 꺼내서
       (str/join) ;; 문자열로 합칩니다
       ))

;; 사실 답이 단 하나 뿐이라 가정하면 중복을 제거하지 않고 점프해도 됩니다

(comment
  (answer-2 t)
  (answer-2 input-lines))
