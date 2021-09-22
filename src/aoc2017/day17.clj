(ns aoc2017.day17)

(def input 355)

"Day17 Spinlock
 circular buffer containing only the value 0, current position.
 steps forward through the circular buffer 35 steps
 inserting the first new value, 1
 The inserted value becomes the current position
 it steps forward from there the same number of steps, and wherever it stops, inserts after it the second new value, 2
 inserting 2017 as its final operation
 "

(defn g [i]
  (fn [xs]
    (let [c (count xs)]
      (cons c (take c (drop (inc i) (cycle xs)))))))

(defn h [i x] (last (take (inc x) (iterate (g i) '(0)))))

(comment
  (take 2 (h 3 2017))

;; => (2017 638)
  (take 2 (h input 2017))
;; => (2017 1912)
  )
