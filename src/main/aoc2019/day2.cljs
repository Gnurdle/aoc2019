(ns aoc2019.day2
  (:require [cljs-node-io.core :as io]
            [clojure.string :as cstr]
            [cljs.test :as ctest]))

(defn- intcode-exec-instr
  "execute a single intcode instruction from mem @ pc, yielding [continue, new-state]"
  [pc mem]
  (let [[op ax bx cx] (take 4 (drop pc mem))]
    (case op
      1 [true (assoc mem cx (+ (get mem ax) (get mem bx)))]
      2 [true (assoc mem cx (* (get mem ax) (get mem bx)))]
      99 [false mem])))

(defn- intcode-exec
  "exec intcode 'program' starting at @ 0, until it halts"
  [mem]
  (loop [pc 0 mem mem]
    (let [[running mem-next] (intcode-exec-instr pc mem)]
      (if-not running mem-next (recur (+ 4 pc) mem-next)))))

(defn ^:private noun-verb
  "run intcode program in mem, after replacing noun and verb in locations 1 and 2"
  [mem noun verb]
  (first (intcode-exec
          (-> mem
              (assoc 1 noun)
              (assoc 2 verb)))))

(defn ^:private day2-data []
  (as-> "resources/day2.txt" x
    (io/slurp x)
    (cstr/split x #",")
    (mapv js/parseInt x)))

(defn ^:private day2a
  []
  (first
   (intcode-exec (-> (day2-data)
                     (assoc 1 12)
                     (assoc 2 2)))))

(defn ^:private day2b []
  (let [mem (day2-data)
        ans 19690720
        trials (for [noun (range 100) verb (range 100)]
                 [noun verb])
        match (first (filter #(= ans (noun-verb mem (first %) (second %))) trials))]
    (+ (* 100 (first match)) (second match))))

(comment
  (ctest/is (= (day2a) 3562624))
  (ctest/is (= (day2b) 8298))
  )
