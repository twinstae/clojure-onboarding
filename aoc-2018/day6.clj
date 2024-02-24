(ns day6
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))


(defn parse-coordinate
  [line]
  (let [[_ x y] (re-matches #"([0-9]+), ([0-9]+)" line)]
    [(parse-long x) (parse-long y)]))

(def test-input (->> (slurp "resources/day6/input0.txt")
                     (str/split-lines)
                     (map parse-coordinate)))
(def input (->> (slurp "resources/day6/input1.txt")
                (str/split-lines)
                (map parse-coordinate)))

(defn rect
  [width height]
  (mapcat (fn [x] (map (fn [y] [x y]) (range (inc height)))) (range (inc width))))

(deftest rect-test
  (testing "주어진 크기의 사각형 안의 모든 점을 반환합니다"
    (is (= (rect 1 2) '([0 0] [0 1] [0 2] [1 0] [1 1] [1 2])))))

(defn manhattan-distance
  [[from-x from-y] [to-x to-y]]
  (+ (abs (- to-x from-x)) (abs (- to-y from-y))))

(deftest manhattan-distance-test
  (testing "x좌표의 차이와 y좌표 차이의 합을 반환합니다"
    (is (= (manhattan-distance [2 2] [0 0]) 4))
    (is (= (manhattan-distance [1 1] [2 2]) 2))
    (is (= (manhattan-distance [0 1] [2 3]) 4))
    (is (= (manhattan-distance [1 0] [3 2]) 4))))

(defn closest
  [from coordinates]
  (let [[a b] (->> coordinates
                   (map (fn [to] {:to to :distance (manhattan-distance from to)}))
                   (sort-by :distance)
                   (take 2))]
    (if (= (a :distance) (b :distance)) nil (a :to))))

(deftest closest-test
  (testing "가장 가까운 점의 좌표를 반환합니다"
    (is (= (closest [0 0] test-input) [1 1])))
  (testing "가장 가까운 거리가 같은 점이 2개 이상이면 nil을 반환합니다"
    (is (= (closest [0 4] test-input) nil))))

(defn area-of
  [coordinate area]
  (count (filter #(= coordinate %) area)))

(defn answer-1
  [coordinates]
  (let [width (apply max (map first coordinates))
        height (apply max (map last coordinates))
        grid (rect width height)
        border (filter (fn [[x y]] (or (zero? x) (zero? y) (= (inc width) x) (= (inc width) y))) grid)
        border-area (map #(closest % coordinates) border)
        area (map #(closest % coordinates) grid)
        infinite-coordinates (->> border-area
                                  (remove nil?)
                                  (distinct))
        finite-coordinates (remove (set infinite-coordinates) coordinates)]

    (->> finite-coordinates
         (map #(area-of % area))
         (apply max))))

(comment
  (answer-1 test-input)
  (answer-1 input))
