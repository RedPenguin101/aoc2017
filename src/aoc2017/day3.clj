(ns aoc2017.day3)

"Day 3: Spiral Memory
 
 Infinite two-dimensional grid with location labels spiraling outwards counter-clockwise

 17  16  15  14  13
 18   5   4   3  12
 19   6   1   2  11
 20   7   8   9  10
 21  22  23---> ...

 What is the manhattan distance from your input location to 1?"

(def input 277678)

(def corners (map (juxt identity #(long (Math/pow (+ (* 2 %) 1) 2))) (range)))

(defn dist [x]
  (let [[n c] (first (drop-while #(< (second %) x) corners))]
    (if (even? (quot (- c x) n))
      (- (* 2 n) (mod (- c x) n))
      (+ n (mod (- c x) n)))))

(comment
  (dist 1024)
  (dist input)
  ;; => 475
  )