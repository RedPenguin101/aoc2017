(ns aoc2017.day6)

(def input [5 1 10 0 1 7 13 14 3 12 8 10 7 12 0 6])

"Day 6: Memory reallocation
 
 16 memory banks, each bank can hold infinite blocks
 A reallocation routine balances blocks between banks
 
 Cycle
 * finds bank with most blocks (ties won by lowest)
 * remove all blocks from bank
 * move to next bank inserts 1, until out of blocks
 
 How many redistributions before finding a repeat?"

(defn idx-of-largest [banks]
  (first (last (sort-by second (reverse (sort-by first (map-indexed vector banks)))))))

(defn redistribute [banks idx amt]
  (if (zero? amt) banks
      (recur (update banks idx inc)
             (mod (inc idx) (count banks))
             (dec amt))))

(redistribute [0 2 0 0] 3 7)

(defn step
  ([banks] (step banks #{} 0))
  ([banks seen steps]
   (if (seen banks) steps
       (let [i (idx-of-largest banks)]
         (recur (redistribute (assoc banks i 0)
                              (mod (inc i) (count banks))
                              (banks i))
                (conj seen banks)
                (inc steps))))))

(comment
  (step [0 2 7 0])
  (step input)
  ;; => 5042
  )


(defn step2
  ([banks] (step2 banks {} 0))
  ([banks seen steps]
   (if-let [seen-on-step (seen banks)]
     (- steps seen-on-step)
     (let [i (idx-of-largest banks)]
       (recur (redistribute (assoc banks i 0)
                            (mod (inc i) (count banks))
                            (banks i))
              (assoc seen banks steps)
              (inc steps))))))

(comment
  (step2 [0 2 7 0])
  (step2 input)
  ;; => 1086
  )