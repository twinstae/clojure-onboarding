#_{:clj-kondo/ignore [:namespace-name-mismatch]}
(ns aoc-2018.day4
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]))

(def test-input-lines (str/split-lines (slurp "resources/day4/input0.txt")))
(def input-lines (str/split-lines (slurp "resources/day4/input1.txt")))


(defn parse-id
  [message]
  (->> message
       (re-matches #"Guard #([0-9]+) begins shift")
       (last)
       (parse-long)))

(deftest parse-id-test
  (is (= (parse-id "Guard #10 begins shift") 10))
  (is (= (parse-id "Guard #99 begins shift") 99)))

(defn parse-input-lines
  [input-lines]
  (->> input-lines
       (sort)
       (reduce
        (fn [acc line]
          (let [[_ date-str message]  (re-matches #"\[[0-9\-]+ [0-9]+:([0-9]+)\] (.+)" line)
                date (parse-long date-str)]
            (case message
              "falls asleep" (update acc :result #(conj % {:id (acc :current) :start date}))
              "wakes up" (update acc :result #(conj (pop %) (assoc (last %) :end date)))
              (assoc acc :current (parse-id message)))))
        {:current nil :result []})
       (#(% :result))
       (map (fn [sleep] (assoc sleep :duration (- (sleep :end) (sleep :start)))))
       (group-by :id)))

(deftest parse-input-lines-test
  (is (= (parse-input-lines test-input-lines)
         {10
          [{:id 10, :start 5, :end 25, :duration 20}
           {:id 10, :start 30, :end 55, :duration 25}
           {:id 10, :start 24, :end 29, :duration 5}],
          99
          [{:id 99, :start 40, :end 50, :duration 10}
           {:id 99, :start 36, :end 46, :duration 10}
           {:id 99, :start 45, :end 55, :duration 10}]})))

(def test-input (parse-input-lines test-input-lines))
(def input (parse-input-lines input-lines))

(defn top-id
  [sleep-by-id]
  (->> sleep-by-id
       (map (fn [[id sleep-list]] {:id id :total (reduce + (map #(% :duration) sleep-list))}))
       (sort-by :total)
       (last)
       (#(get % :id))))

(comment (top-id input))

(defn most-sleep-minute
  [id sleep-list]
  (->> sleep-list
       (mapcat (fn [sleep] (set (range (sleep :start) (sleep :end)))))
       (frequencies)
       (sort-by last)
       (last)
       ((fn [[minute times]] {:id id :minute minute :times times}))))

(deftest overlap-minute-test
  (is (= (let [id (top-id test-input)]
           (most-sleep-minute id (test-input id))) 24)))

(defn answer-1
  [sleep-by-id]
  (let [id (top-id sleep-by-id)
        result (most-sleep-minute id (sleep-by-id id))]
    (* (result :id) (result :minute))))


(comment
  (answer-1 test-input)
  (answer-1 input))

