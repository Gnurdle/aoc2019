(ns aoc2019.day1
  (:require [cljs-node-io.core :as io]
            [clojure.string :as cstr]
            [cljs.test :as ctest]))

(defn- day1-data []
  (->> "resources/day1.txt"
       (io/slurp)
       (cstr/split-lines)
       (map js/parseInt)))

(defn ^:private fuel-of
  [mass]
  (max 0 (- (quot mass 3) 2)))

(defn ^:private day1a
  []
  (let [d (day1-data)]
    (reduce + (map fuel-of d))))

(defn ^:private decaying-fuel
  [fuel]
  (loop [total-fuel fuel remaining-fuel fuel]
    (let [more-fuel (fuel-of remaining-fuel)]
      (if (pos? more-fuel)
        (recur (+ total-fuel more-fuel) more-fuel)
        total-fuel))))

(defn ^:private fuel-of-and-decaying
  [mass]
  (decaying-fuel (fuel-of mass)))

(defn ^:private day1b
  []
  (let [d (day1-data)]
    (reduce + (map fuel-of-and-decaying d))))

(comment
  (ctest/is (= (day1a) 3382136))
  (ctest/is (= (day1b) 5070314))
  )

