(ns aoc2017.day16
  (:require [aoc2017.utils :refer [load-input]]
            [clojure.string :as str]
            [clojure.set :refer [map-invert]]))

(defn pick-numbers [string]
  (map #(Long/parseLong %) (re-seq #"\d+" string)))

(defn parse [[mv & rst]]
  (case mv
    \x (into [:x] (pick-numbers (apply str rst)))
    \s (into [:s] (pick-numbers (apply str rst)))
    \p (into [:p (str (first rst)) (str (last rst))])))

(def input (map parse (first (load-input 16 #(str/split % #",")))))

(take 5 input)
;; => ([:x 10 0] [:s 2] [:x 6 11] [:s 3] [:x 12 1])

"Spin, (sX), makes X programs move from the end to the front, 
 but maintain their order otherwise. 
 (For example, s3 on abcde produces cdeab).
 
 Exchange, (x3/4), makes the programs at positions 3 and 4 
 swap places.
 
 Partner, (pr/b), makes the programs named r and b swap 
 places."

(def start (mapv str "abcdefghijklmnop"))

(defn swap-postions [coll a b]
  (let [m (map-invert (into {} (map-indexed vector coll)))]
    (-> coll (assoc (m a) b) (assoc (m b) a))))

(defn move [progs [m a b]]
  (case m
    :s (vec (take (count progs) (drop (- (count progs) a) (cycle progs))))
    :x (-> progs (assoc a (progs b)) (assoc b (progs a)))
    :p (swap-postions progs a b)))

(comment
  (apply str (reduce move start input)))
;; => "kgdchlfniambejop"

(let [mvs [[:s 1] [:x 3 4] [:p "e" "b"]]
      st ["a" "b" "c" "d" "e"]
      f #(reduce move % mvs)]
  (f st))

(defn find-loop
  ([moves start] (find-loop moves start (reduce move start moves) 1))
  ([mvs start current it]
   (if (= start current)  it
       (recur mvs start (reduce move current mvs) (inc it)))))

(comment
  (find-loop [[:s 1] [:x 3 4] [:p "e" "b"]]
             ["a" "b" "c" "d" "e"])
  (find-loop input start)
  (mod 1000000000 42)
  ;; => 34


  (apply str (last (take 35 (iterate #(reduce move % input) start))))
  ;; => "fjpmholcibdgeakn"
  )