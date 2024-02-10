(ns day2
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def input-lines (str/split-lines (slurp "resources/day2/input1.txt")))


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

(defn compare-str
  "a와 b 사이에 글자수 차이와 공통 부분을 튜플로 반환합니다"
  [[a b]]
  (reduce
   (fn [[diff common] [x y]]
     (if (= x y)
       [diff (conj common x)]
       [(inc diff) common]))
   [0, []]
   (map vector a b)))

((deftest compare-str-test
   (testing "똑같은 경우"
     (is (= (compare-str ["abc" "abc"])
            [0 [\a \b \c]])))
   (testing "1글자만 다른 경우"
     (is (= (compare-str ["abc" "abi"])
            [1 [\a \b]])))))


(def t
  ["abcde" "fghij" "fguij"])

(defn permutation-2
  "items 중에 중복 없이 2개를 고른 순열을 반환합니다."
  [items] ;; items에는 중복이 없다고 가정합니다
  (->>
   (mapcat (fn [a] (map (fn [b] [a b]) items)) items) ;; flatMap + map
   (filter (fn [[a b]] (not= a b))) ;; 자기 자신과의 짝은 걸러줍니다
)) ;; 순서만 다른 값들을 걸러줍니다

((deftest permutation-2-test
   (is (= (permutation-2 ["a" "b" "c"])
          '(["a" "b"] ["a" "c"] ["b" "a"] ["b" "c"] ["c" "a"] ["c" "b"])))))

(defn answer-2
  [input]
  (->> input
       (permutation-2) ;; 비교하기 위해 조합을 구합니다
       (map compare-str) ;; 비교해서 다른 글자수와 공통 부분들을 구합니다
       (filter (fn [[diff]] (= diff 1))) ;; 1글자만 다른 것 중에
       (distinct)
       (first) ;; 첫 번째 항목에서
       (second) ;; 공통부분의 문자 sequence를 꺼내서
       (str/join) ;; 문자열로 합칩니다
       ))

;; 사실 답이 단 하나 뿐이라 가정하면 중복을 제거하지 않고 점프해도 됩니다

(comment
  (answer-2 t)
  (answer-2 input-lines))
