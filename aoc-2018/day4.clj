(ns day4
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def test-input-lines (str/split-lines (slurp "resources/day4/input0.txt")))
(def input-lines (str/split-lines (slurp "resources/day4/input1.txt")))

(defn parse-id
  [message]
  (->> message
       (re-matches #"Guard #([0-9]+) begins shift")
       (last)
       (parse-long)))

(deftest parse-id-test
  (testing "메시지에서 id만 뽑아서 반환합니다"
    (is (= (parse-id "Guard #10 begins shift") 10))
    (is (= (parse-id "Guard #99 begins shift") 99))))

(defn parse-input-lines
  [input-lines]
  (->> input-lines
       (sort)
       (reduce
        (fn [acc line]
          (let [{:keys [current result start]} acc
                [_ date-str message]  (re-matches #"\[[0-9\-]+ [0-9]+:([0-9]+)\] (.+)" line)
                date (parse-long date-str)]
            (case message
              "falls asleep" (assoc acc :start date)
              "wakes up" {:current current :result (conj result {:id current :start start :end date})}
              (assoc acc :current (parse-id message)))))
        {:current nil :result [] :start nil})
       (#(% :result))
       (group-by :id)))

(deftest parse-input-lines-test
  (testing ""
    (is (= (parse-input-lines test-input-lines)
           {10
            [{:id 1, :start 5 :end 25}
             {:id 10 :start 30 :end 55}
             {:id 10 :start 24 :end 29}],
            99
            [{:id 99 :start 40 :end 50}
             {:id 99 :start 36 :end 46}
             {:id 99 :start 45 :end 55}]}))))

(def test-input (parse-input-lines test-input-lines))
(def input (parse-input-lines input-lines))

(defn duration
  [{:keys [start end]}]
  (- end start))

(defn top-id
  [sleep-by-id]
  (->> sleep-by-id
       (map (fn [[id sleep-list]] {:id id :total (reduce + (map duration sleep-list))}))
       (apply max-key :total)
       (#(% :id))))

(deftest top-id-test
  (is (= (top-id test-input) 10)))

(comment (top-id input))

(defn most-sleep-minute
  [id sleep-list]
  (->> sleep-list
       (mapcat (fn [{:keys [start end]}] (set (range start end))))
       (frequencies)
       (apply max-key last)
       ((fn [[minute times]] {:id id :minute minute :times times}))))

(deftest most-sleep-minute-test
  (is (= (let [id (top-id test-input)]
           (most-sleep-minute id (test-input id)))
         {:id 10, :minute 24, :times 2})))

(defn answer-1
  [sleep-by-id]
  (let [id (top-id sleep-by-id)
        result (most-sleep-minute id (sleep-by-id id))]
    (* (result :id) (result :minute))))


(comment
  (answer-1 test-input)
  (answer-1 input))

(defn most-sleep-guard
  [sleep-by-id]
  (->> sleep-by-id
       (map #(apply most-sleep-minute %))
       (sort-by :times)
       (last)))

(deftest most-sleep-guard-test
  (is (= (most-sleep-guard test-input)
         {:id 99, :minute 45, :times 3})))

(defn answer-2
  [sleep-by-id]
  (let [result (most-sleep-guard sleep-by-id)]
    (* (result :id) (result :minute))))

(comment
  (answer-2 (parse-input-lines test-input-lines))
  (answer-2 (parse-input-lines input-lines)))
