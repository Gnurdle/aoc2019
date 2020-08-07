(ns aoc2019.core
  (:require [aoc2019.day1 :as day1]
            [aoc2019.day2 :as day2]
            [aoc2019.day3 :as day3]))

(defn main []
  (let [runs
        [["day1a" day1/day1a 3382136]
         ["day1b" day1/day1b 5070314]
         ["day2a" day2/day2a 3562624]
         ["day2b" day2/day2b 8298]
         ["day3a" day3/day3a 1285]
         ]]
    (doseq [[txt fnc expected] runs]
      (let [start    (system-time)
            result   (fnc)
            stop     (system-time)
            timestr  (str " " (.toFixed (- stop start) 6) " msecs")]
        (if (= expected result)
          (println txt "passed=" result timestr)
          (println txt "failed, expected: " expected ", got: " result timestr))))))

(comment
  (main)
  )
