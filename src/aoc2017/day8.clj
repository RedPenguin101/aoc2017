(ns aoc2017.day8
  (:require [clojure.string :as str]
            [aoc2017.utils :refer [load-input]]))

"Instruction format: r1 inc/dec v1 if r2 cmp v2 (where r=register, v=value)
 
 cmp (compartors) are ==,!=,>,>=,<,<=,
 
 All registers start at zero"

(def cmps {"==" = "!=" not= "<" < ">" > "<=" <= ">=" >=})

(defn parse-instruction [string]
  (let [[r1 id v1 _if r2 cmp v2] (str/split string #" ")]
    [(keyword r1)
     (if (= id "inc") + -)
     (Long/parseLong v1)
     (keyword r2)
     (cmps cmp)
     (Long/parseLong v2)]))

"Part 1: What is the largest value in any register after completing the instructions in your puzzle input?"

(defn process-instruction [registers [r1 id v1 r2 cmp v2]]
  (if (cmp (get registers r2 0) v2)
    (update registers r1 (fnil id 0) v1)
    registers))

(->> (load-input 8)
     (map parse-instruction)
     (reduce process-instruction {})
     vals
     (apply max))

"Part 2: What is he highest value held in any register during this process"

(defn process-instruction-track-max [registers [r1 id v1 r2 cmp v2]]
  (if (cmp (get registers r2 0) v2)
    (let [new-reg (update registers r1 (fnil id 0) v1)]
      (update new-reg :max max (new-reg r1)))
    registers))

(->> (load-input 8)
     (map parse-instruction)
     (reduce process-instruction-track-max {:max 0})
     :max)
