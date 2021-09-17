(ns aoc2017.day12
  (:require [clojure.set :as set]
            [aoc2017.utils :refer [load-input integers]]))

"Part 1: How many programs are in the group that contains program 
 ID 0?"

(defn add-edges [graph [node-a & others]]
  (concat graph (for [node others] [node node-a])))

(def graph (reduce add-edges [] (load-input 12 integers)))

(defn connected-nodes [graph node]
  (set (map second (filter #(= (first %) node) graph))))

(defn bfs
  ([graph start] (bfs graph [start] #{start}))
  ([graph queue checked]
   (if (empty? queue) checked
       (let [new-conx (set/difference (connected-nodes graph (first queue)) checked)]
         (recur graph (concat (rest queue) new-conx) (set/union checked new-conx))))))

(comment
  (let [example-graph (reduce add-edges [] [[0 2] [1 1] [2 0 3 4] [3 2 4] [4 2 3 6] [5 6] [6 4 5]])]
    (count (bfs example-graph 0)))
  ;; => 6

  (count (bfs graph 0))
  ;; => 239
  )

"Part 2: how many groups are there in total?"

(defn groups
  ([graph] (groups graph (set (flatten graph)) []))
  ([graph nodes groups]
   (let [unaccounted-nodes (set/difference nodes (set (apply concat groups)))]
     (if (empty? unaccounted-nodes) groups
         (recur graph nodes (conj groups (bfs graph (first unaccounted-nodes))))))))

(comment
  (let [example-graph (reduce add-edges [] [[0 2] [1 1] [2 0 3 4] [3 2 4] [4 2 3 6] [5 6] [6 4 5]])]
    (groups example-graph))
  ;; => [#{0 4 6 3 2 5} #{1}]
  (count (groups graph))
  ;; => 215
  )