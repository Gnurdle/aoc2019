(ns aoc2019.day3
  (:require [cljs-node-io.core :as io]
            [clojure.string :as cstr]
            [clojure.set :as cset]
            [cljs.test :as ctest]))

(defn ^:private parse-move
  [ss]
  (let [dd (keyword (first ss))
        nn (js/parseInt (subs ss 1))]
    [dd nn]))

(defn- day3-data []
  (let [fix-line (fn [x]
                   (-> x
                       (cstr/split #",")
                       (as-> d (mapv parse-move d))))]
    (-> "resources/day3.txt"
        (io/slurp)
        (cstr/split-lines)
        (as-> x (mapv fix-line x)))))

(defn ^:private points-traversed-by-move
  "given starting point and move, return the points traversed by dragging"
  [start move]
  (let [[direction count] move
        stepper (case direction
                  :L (fn [[x y]] [(dec x) y])
                  :R (fn [[x y]] [(inc x) y])
                  :U (fn [[x y]] [x (inc y)])
                  :D (fn [[x y]] [x (dec y)]))]
    (reduce (fn [pts _]
              (conj pts (stepper (last pts)))) [start] (range count))))

(defn ^:private points-traversed-by-path
  "return the set of all points traversed by the moves in path"
  [path]
  (loop [seen #{} curr [0 0] moves path]
    (if-not (seq moves)
      seen
      (let [more-points (points-traversed-by-move curr (first moves))
            new-curr (last more-points)]
        (recur (cset/union seen (into #{} more-points)) new-curr (rest moves))))))

(defn ^:private points-intersections
  "yield the path crossings, excepting the origin"
  [points1 points2]
  (disj (cset/intersection (set points1) (set points2)) [0 0]))

(defn ^:private abs [x]
  (max x (- x)))

(defn ^:private manhattan-dist-origin
  [[x y]]
  (+ (abs x) (abs y)))

(defn ^:private day3a []
  (let [[path1 path2] (day3-data)
        points1 (points-traversed-by-path path1)
        points2 (points-traversed-by-path path2)
        intersections (points-intersections points1 points2)
        nearest-point (apply min-key manhattan-dist-origin intersections)]
    (manhattan-dist-origin nearest-point)))

(defn ^:private day3b []
  (let [[path1 path2] (day3-data)
        points1 (points-traversed-by-path path1)
        points2 (points-traversed-by-path path2)
        intersections (points-intersections points1 points2)]

    {:points1 points1
     :points2 points2
     :intersections intersections}))

(defn ^:private point-steps-to
  [point points]
  (loop [n 0 points points]
    (when (seq points)
      (let [first-point (first points)]
        (if (= point first-point)
          n
          (recur (inc n) (rest points)))))))

(comment
  (def $d (day3b))
  (keys $d)
  (def $points1 (:points1 $d))
  (def $points2 (:points2 $d))
  (def $intersections (:intersections $d))
  (first $intersections)
  (point-steps-to (first $intersections) $points1)
  (point-steps-to (first $intersections) $points2)
  (def $steps (map (fn [pt])))
  (count $intersections))

(comment
  (ctest/is (= (day3a) 1285))
  )

