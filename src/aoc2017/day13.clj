(ns aoc2017.day13
  (:require [aoc2017.utils :refer [load-input integers]]))

(def input (into {} (map (fn [[l r]] [l [r 0 inc]]) (load-input 13 integers))))

"Firewall: 
 comprised of layer 
 each layer has a scanner
 input in form layer-number: scanner-range
 Scanners move up and down their range
 You need to move through the layers, at the top of the range
 You get caught if you move into the layer when the scanner is at the top
 Severity of a catch is depth*range.
 "

"Part 1: If you start immediately, what is the severity of your whole trip?"

'{layer [range loc inc/dec]}

(def example {0 [3 0 inc]
              1 [2 0 inc]
              4 [4 0 inc]
              6 [4 0 inc]})

(defn update-layer [[range loc dir]]
  (let [new-dir (cond (= 0 loc) inc
                      (= (dec range) loc) dec
                      :else dir)]
    [range (new-dir loc) new-dir]))

(defn map-kv [f m]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

(defn run
  ([state] (run 0 0 state))
  ([layer score state]
   (let [[range scanner] (or (state layer) [0 1])
         hit (if (zero? scanner) (* layer range) 0)]
     (println layer (take 2 (state layer))
              (if (zero? scanner) "hit!" "miss"))
     (if (= layer (apply max (keys state))) (+ hit score)
         (recur (inc layer) (+ score hit) (map-kv update-layer state))))))

(comment
  (run example)
  ;; => 24

  (run input)
  ;; => 2604
  )

"What is the fewest number of picoseconds that you need to delay the packet to pass through the firewall without being caught?"

(defn hit?
  ([state] (hit? 0 state))
  ([layer state]
   (cond
     (zero? (second (or (state layer) [0 1]))) true
     (= layer (apply max (keys state))) false
     :else (recur (inc layer) (map-kv update-layer state)))))

(defn find-delay [state delay]
  (if (not (hit? state)) delay
      (recur (map-kv update-layer state) (inc delay))))

(comment
  (find-delay example 0)

  (time (find-delay input 0))
  ;; 3941460
  "Took like 4 mins :P")
