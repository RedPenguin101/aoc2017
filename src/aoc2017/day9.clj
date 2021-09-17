(ns aoc2017.day9
  (:require [clojure.test :refer [deftest are]]
            [aoc2017.utils :refer [load-input]]))

"Day 9: Stream Processing
 Stream of characters
 Groups are enclosed with {}
 Groups contains sequences of comma separated objects (which can include other groups)
 Garbage sequences are enclosed with <>. A garbage sequence can include { or } or <, which in this
 context have no special meaning
 ! is the escape character
 The 'score' of a group is one more than the score of the group that encloses it
 (the outermost group has a score of 1)
 
 So we're writing a parser"


(defn parse
  ([string] (first (parse [] string false)))
  ([ast [fst & rst] garbage?]
   (cond
     (nil? fst) ast

     (= \! fst) (recur ast (drop 1 rst) garbage?)

     (and garbage? (= \> fst)) (recur ast rst false)
     garbage?                  (recur (conj ast fst) rst true)
     (= \< fst)                (recur ast rst true)

     (#{\space \,} fst) (recur ast rst garbage?)

     (= \{ fst) (let [[expr unproc] (parse [] rst garbage?)]
                  (recur (conj ast expr) unproc garbage?))
     (= \} fst) [ast rst])))

(defn score
  ([ast] (score ast 1))
  ([ast depth]
   (cond (not (coll? ast)) 0
         (every? #(not (coll? %)) ast) depth
         :else (->> ast
                    (map #(score % (inc depth)))
                    (apply +)
                    (+ depth)))))

(deftest t
  (are [i out] (= out (score (parse i)))
    "{}" 1
    "{{{}}}" 6
    "{{},{}}" 5
    "{{{},{},{{}}}}" 16
    "{<a>,<a>,<a>,<a>}" 1
    "{{<ab>},{<ab>},{<ab>},{<ab>}}" 9
    "{{<!!>},{<!!>},{<!!>},{<!!>}}" 9
    "{{<a!>},{<a!>},{<a!>},{<ab>}}" 3))

(def input (first (load-input 9 identity)))

(comment
  (score (parse input))
  ;; => 16869

  "Part 2: How many non-escaped garbage characters?"

  (count (flatten (parse input)))
  ;; => 7284
  )
