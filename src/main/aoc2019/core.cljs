(ns aoc2019.core
  (:require [cljs-node-io.core :as io]
            [clojure.string :as cstr]))

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

(comment
  (def mem (day2-data))
  (noun-verb mem 12 2)
)

(defn ^:private day2b []
  (let [mem (day2-data)
        ans 19690720
        trials (for [noun (range 100) verb (range 100)]
                 [noun verb])
        match (first (filter #(= ans (noun-verb mem (first %) (second %))) trials))]
    (+ (* 100 (first match)) (second match))
    ))

(comment
  (day2b)
  )
  
(defn main [& args]
  (println "day1-a: " (day1a))
  (println "day1-b: " (day1b))
  (println "day2-a: " (day2a))
  (println "day2-b: " (day2b))
  )
(main)

