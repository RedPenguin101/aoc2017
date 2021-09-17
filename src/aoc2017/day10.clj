(ns aoc2017.day10)

(def input [183,0,31,146,254,240,223,150,2,206,161,1,255,232,199,88])

"What is the result of multiplying the first two numbers in the list?"

(def example-list '(0 1 2 3 4))
(def example-lengths '(3 4 1 5))

(defn f [lst skip [len & rst]]
  (if len
    (let [[a b] (split-at len lst)]
      (recur (take (count lst) (drop skip (cycle (concat b (reverse a)))))
             (inc skip)
             rst))
    lst))

(f example-list 0 example-lengths)

(apply + (into example-lengths (range 0 (count example-lengths))))
;; => 19
(apply + (map-indexed + example-lengths))

(defn de-cycle [lst lens]
  (let [x (mod (apply + (map-indexed + lens)) (count lst))]
    (reverse (take (count lst)
                   (drop x (cycle (reverse lst)))))))

(de-cycle (f example-list 0 example-lengths) example-lengths)

(apply * (take 2 (de-cycle (f (range 0 256) 0 input) input)))

