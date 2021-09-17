(ns aoc2017.day11
  (:require [aoc2017.utils :refer [load-input alpha-words]]))

;;   ___
;;  /   \ x-ax (ne, sw)
;;  \___/ y-ax (se, nw)
;;   z-ax (n,s)

(defn move [loc dir]
  (mapv + loc (case dir
                "n"  [1 -1 0]
                "s"  [-1 1 0]
                "ne" [0 -1 1]
                "sw" [0 1 -1]
                "nw" [1 0 -1]
                "se" [-1 0 1])))

(defn distance [loc]
  (/ (apply + (map #(Math/abs %) loc)) 2))

(def input (first (load-input 11 alpha-words)))

(comment
  "Part 1: determine the fewest number of steps required to reach him."
  (distance (reduce move [0 0 0] input))
  ;; => 743

  "Part 2: How many steps away is the furthest he ever got from his starting position?"
  (->> input
       (reductions move [0 0 0])
       (map distance)
       (apply max))
  ;; => 1493
  )
