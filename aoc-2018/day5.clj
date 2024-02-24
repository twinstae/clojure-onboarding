(ns day5
  (:require [clojure.test :refer [deftest is testing]]))

(defn react?
  [a b]
  (= (abs (- (int a) (int b))) 32))

(deftest react?-test
  (testing "반응한다"
    (is (react? \a \A))
    (is (react? \A \a))
    (is (react? \b \B)))
  (testing "반응하지 않는다"
    (is (false? (react? \a \B)))
    (is (false? (react? \A \A)))
    (is (false? (react? \a \a)))))

(defn reaction
  [polymer]
  (->> polymer
       (reduce
        (fn [acc unit]
          (if (and (seq acc) (react? unit (first acc)))
            (rest acc)
            (cons unit acc)))
        '())
       (reverse)
       (apply str)))

(deftest reaction-test
  (testing "반응해서 사라진다"
    (is (= (reaction "aA") ""))
    (is (= (reaction "Aa") "")))
  (testing "연쇄 반응으로 사라진다"
    (is (= (reaction "abBA") ""))
    (is (= (reaction "aBbA") "")))
  (testing "같은 종류가 아니면 반응이 일어나지 않는다"
    (is (= (reaction "abAB") "abAB"))
    (is (= (reaction "aBAb") "aBAb")))
  (testing "같은 종류여도 극성이 같으면 반응이 일어나지 않는다"
    (is (= (reaction "aabAAB") "aabAAB"))
    (is (= (reaction "bbAbaBB") "bbAbaBB")))
  (testing "복잡한 예시"
    (is (= (reaction "dabAcCaCBAcCcaDA") "dabCBAcaDA"))))

(def input (slurp "resources/day5/input1.txt"))

(comment
  (->> input
       (reaction)
       (count)))

;; part 2

(defn removing-all-x
  [x polymer]
  (let [X (Character/toUpperCase x)]
    (remove #(or (= % x) (= % X)) polymer)))

(def test-polymer "dabAcCaCBAcCcaDA")

(defn get-result
  [x]
  (apply str (removing-all-x x test-polymer)))

(deftest removing-all-x-test
  (testing "제거"
    (is (= (get-result \a) "dbcCCBcCcD"))
    (is (= (get-result \b) "daAcCaCAcCcaDA"))
    (is (= (get-result \c) "dabAaBAaDA"))
    (is (= (get-result \d) "abAcCaCBAcCcaA"))))

(def a-to-z "abcdefghijklmnopqrstuvwxyz")

(defn answer-2
  [polymer]
  (->> a-to-z
       (map #(->> polymer
                  (removing-all-x %)
                  (reaction)
                  (count)))
       (apply min)))

(comment
  (answer-2 test-polymer)
  (answer-2 input))