(ns aoc2019.core
  (:require [cljs-node-io.core :as io]
            [clojure.string :as cstr]
            [clojure.set :as cset]
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


(defn- day2-data []
  (as-> "resources/day2.txt" x
    (io/slurp x)
    (cstr/split x #",")
    (mapv js/parseInt x)))

(defn- intcode-exec-instr
  "execute a single intcode instruction from mem @ pc, yielding [continue, new-state]"
  [pc mem]
  (let [[op ax bx cx] (take 4 (drop pc mem))]
    (case op
      1 [true (assoc mem cx (+ (get mem ax) (get mem bx)))]
      2 [true (assoc mem cx (* (get mem ax) (get mem bx)))]
      99 [false mem])))

(defn- intcode-exec [mem]
  (loop [pc 0 mem mem]
    (let [[running mem-next] (intcode-exec-instr pc mem)]
      (if-not running mem-next (recur (+ 4 pc) mem-next)))))

(defn ^:private day2a
  []
  (first 
   (intcode-exec (-> (day2-data)
                     (assoc 1 12)
                     (assoc 2 2)))))

(defn ^:private noun-verb [mem noun verb]
  (first (intcode-exec 
          (-> mem 
              (assoc 1 noun)
              (assoc 2 verb)))))

(defn ^:private day2b []
  (let [mem (day2-data)
        ans 19690720
        trials (for [noun (range 100) verb (range 100)]
                 [noun verb])
        match (first (filter #(= ans (noun-verb mem (first %) (second %))) trials))]
    (+ (* 100 (first match)) (second match))
    ))

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
    (reduce (fn [pts pt]
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

(defn ^:private abs [x] 
  (max x (- x)))

(defn ^:private manhattan-dist-origin
  [[x y]]
  (+ (abs x) (abs y)))

(defn ^:private day3a []
  (let [[path1 path2] (day3-data)
        crossings (disj (cset/intersection (points-traversed-by-path path1)
                                           (points-traversed-by-path path2)) [0 0])
        nearest-point (apply min-key manhattan-dist-origin crossings)]
  (manhattan-dist-origin nearest-point)))



(comment
  (def $x (day3a))
  
  )

(ctest/deftest all-tests
  (ctest/is (= (day1a) 3382136))
  (ctest/is (= (day1b) 5070314))
  (ctest/is (= (day2a) 3562624))
  (ctest/is (= (day2b) 82938))
  (ctest/is (= (day3a) 1285)))

(comment (time (all-tests))
         )