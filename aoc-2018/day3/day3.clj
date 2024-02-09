#_{:clj-kondo/ignore [:namespace-name-mismatch]}
(ns aoc-2018.day3
  (:require
   [clojure.string :as str]))

(def t
  ["#1 @ 1,3: 4x4"
   "#2 @ 3,1: 4x4"
   "#3 @ 5,5: 2x2"])

(def input-lines (str/split-lines (slurp "resources/day3/input1.txt")))

(defn parse-line
  [line]
  (let [[_ id x y width height] (re-matches #"#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)" line)]

    {:start {:x (parse-long x) :y (parse-long y)}
     :size  {:width (parse-long width) :height (parse-long height)}
     :id       id}))

(comment
  (parse-line "#1 @ 1,3: 4x4")
  (map parse-line t))

(defn points-of
  "시작점과 크기를 받아서, 사각형 안의 모든 점들의 좌표를 반환합니다"
  [{{start-x :x start-y :y} :start {width :width height :height} :size}]
  (mapcat (fn [x] (map (fn [y] [x y]) (range start-y (+ start-y height)))) (range start-x (+ start-x width))))

(comment
  (points-of {:start {:x 1, :y 3}, :size {:width 4, :height 4}, :id "1"}))

(defn answer-1
  [input]
  (->> input
       (map parse-line) ;; 입력을 예쁘게 시작점과 크기로 읽어서
       (mapcat points-of) ;; 점들로 변환하고
       (frequencies) ;; 점의 개수를 세어서
       (filter (fn [[_point count]] (>= count 2))) ;; 2개 이상인 것만 남기고
       (count) ;; 그 숫자를 셉니다
       ))

(comment
  (answer-1 t)
  (answer-1 input-lines))
