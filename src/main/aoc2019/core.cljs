(ns aoc2019.core
  (:require [cljs.core.async :refer-macros [go] :as A]
            [aoc2019.day1 :as day1]
            [aoc2019.day2 :as day2]
            [aoc2019.day3 :as day3]
            ["ws" :as ws]))

;; start a websocket server, just to hang a resource out
;; there to test lifecycle stuff
(defn- setup-wss []
  (let [wss (ws/Server. (clj->js {:port 8088}))]
    (println "started websocket server @ 8088")
    (.on wss "connection" #(println "got ws connection"))))

(defn main []
  (setup-wss)

  #_(cljs.repl/repl (cljs.repl.node/repl-env :port 8192))
  (let [runs
        [["day1a" day1/day1a 3382136]
         ["day1b" day1/day1b 5070314]
         ["day2a" day2/day2a 3562624]
         ["day2b" day2/day2b 8298]
         ["day3a" day3/day3a 1285]
         ]]
    (go
      (doseq [[txt fnc expected] runs]
        (let [start    (system-time)
              result   (fnc)
              stop     (system-time)
              timestr  (str " " (.toFixed (- stop start) 6) " msecs")]
          (if (= expected result)
            (println txt "passed=" result timestr)
            (println txt "failed, expected: " expected ", got: " result timestr)))))))

(comment
  (main)
  (day3/day3a)
  )
