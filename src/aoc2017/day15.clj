(ns aoc2017.day15)

"Generator A starts with 873
 Generator B starts with 583"

"Day 15: Dueling Generators
 compares the lowest 16 bits of both values
 keeps track of the number of times those parts of the values match.
 To create its next value, a generator will take the previous value it produced, multiply it by a factor 
 (16807, 48271)
 divide by 2147483647"

(defn next-value [factor]
  (fn [previous]
    (mod (* previous factor) 2147483647)))

(def A (next-value 16807))
(def B (next-value 48271))

(comment
  (time
   (reduce (fn [acc [a b]]
             (if (= (bit-and 65535 a)
                    (bit-and 65535 b))
               (inc acc) acc))
           0
           (take 1060
                 (partition 2 (interleave (filter #(zero? (mod % 4)) (iterate A 65))
                                          (filter #(zero? (mod % 8)) (iterate B 8921)))))))


  (time
   (reduce (fn [acc [a b]]
             (if (= (bit-and 65535 a)
                    (bit-and 65535 b))
               (inc acc) acc))
           0
           (take 5000000
                 (partition 2 (interleave (filter #(zero? (mod % 4)) (iterate A 873))
                                          (filter #(zero? (mod % 8)) (iterate B 583))))))))
