(ns aoc2017.day7
  (:require [aoc2017.utils :refer [load-input alpha-words integers]]))

"Day 7: Recursive Circus
  What is the name of the bottom program?"

(def input (load-input 7 alpha-words))

(defn edges [input]
  (mapcat (fn [[prnt & children]]
            (for [c children]
              [prnt c]))
          input))

(def graph (edges input))

(defn top-parent [graph node]
  (if-let [p (ffirst (filter #(= node (second %)) graph))]
    (recur graph p)
    node))

(comment
  (top-parent graph (ffirst graph))
  ;; => "svugo"
  )

"Part2: "

(def weights
  (apply hash-map (interleave (map first (load-input 7 alpha-words))
                              (load-input 7 integers))))

(defn children [graph node]
  (map second (filter #(= node (first %)) graph)))

(defn descendants [graph node]
  (cons node (mapcat #(descendants graph %)
                     (children graph node))))

(defn total-weight [node graph weights]
  (apply + (map weights (descendants graph node))))

(defn unbalanced? [node graph weights]
  (let [w (sort-by (comp count second) (group-by #(total-weight % graph weights) (children graph node)))]
    (if (= (count w) 1)
      nil
      [(first (second (first w)))
       (apply - (map first w))])))

(defn rebalance-program [node edges weights diff]
  (if-let [[nd amt] (unbalanced? node edges weights)]
    (recur nd edges weights amt)
    (- (weights node) diff)))

(comment
  (time (rebalance-program "svugo" graph weights 0))
  ;; => 1152
  )