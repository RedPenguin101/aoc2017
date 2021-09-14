(ns aoc2017.day7
  (:require [aoc2017.utils :refer [load-input alpha-words integers]]))

;; Generic graph functions

(defn top-parent [graph node]
  (if-let [p (ffirst (filter #(= node (second %)) graph))]
    (recur graph p)
    node))

(defn children [graph node]
  (map second (filter #(= node (first %)) graph)))

(defn descendants [graph node]
  (cons node (mapcat #(descendants graph %)
                     (children graph node))))

"Day 7: Recursive Circus
  Part 1: What is the name of the bottom program?"

(defn edges [input]
  (mapcat (fn [[prnt & children]]
            (for [c children]
              [prnt c]))
          input))

(def graph (edges (load-input 7 alpha-words)))

(comment
  (top-parent graph (ffirst graph))
  ;; => "svugo"
  )

"Part2: What is the corrected weight of the unbalanced program?"

(def weights
  (apply hash-map (interleave (map first (load-input 7 alpha-words))
                              (load-input 7 integers))))

(defn total-weight [node graph weights]
  (->> node
       (descendants graph)
       (map weights)
       (apply +)))

(defn unbalanced? [node graph weights]
  (let [weights-by-freq (sort-by (comp count second) (group-by #(total-weight % graph weights) (children graph node)))]
    (if (= (count weights-by-freq) 1)
      nil
      [(first (second (first weights-by-freq)))
       (apply - (map first weights-by-freq))])))

(defn rebalance-program [node graph weights diff]
  (if-let [[ub-node ub-amount] (unbalanced? node graph weights)]
    (recur ub-node graph weights ub-amount)
    (- (weights node) diff)))

(comment
  (time (rebalance-program "svugo" graph weights 0))
  ;; => 1152
  )