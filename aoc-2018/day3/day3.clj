#_{:clj-kondo/ignore [:namespace-name-mismatch]}
(ns aoc-2018.day3
  (:require
   [clojure.string :as str]))

(def t
  ["#1 @ 1,3: 4x4"
   "#2 @ 3,1: 4x4"
   "#3 @ 5,5: 2x2"])

(def root "/home/amikojhk/github/clojure-onboarding/")
(def data-file (slurp (str root "aoc-2018/day3/input1.txt")))
(def input-lines (str/split-lines data-file))

(defn parse-line
  [line]
  (let [[id, data] (str/split line #" @ ")
        [start size] (#(str/split data #": "))]

    [(map #(Integer/parseInt %) (str/split start #","))
     (map #(Integer/parseInt %) (str/split size #"x"))
     id]))

(comment
  (parse-line "#1 @ 1,3: 4x4")
  (map parse-line t))

(defn points-of
  "시작점과 크기를 받아서, 사각형 안의 모든 점들의 좌표를 반환합니다"
  [[[start-x start-y] [size-x size-y]]]
  (mapcat (fn [x] (map (fn [y] [x y]) (range start-y (+ start-y size-y)))) (range start-x (+ start-x size-x))))

(comment
  (points-of ['(1 3) '(4 4)]))

(defn answer-1
  [input]
  (->> input
       (map parse-line) ;; 입력을 예쁘게 시작점과 크기로 읽어서
       (mapcat points-of) ;; 점들로 변환하고
       (frequencies) ;; 점의 개수를 세어서
       (seq) ;; 각 점마다
       (filter (fn [[_point count]] (>= count 2))) ;; 2개 이상인 것만 남기고
       (count) ;; 그 숫자를 셉니다
       ))

(comment
  (answer-1 t)
  (answer-1 input-lines))
