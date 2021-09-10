(ns aoc2017.day1
  (:require [aoc2017.utils :refer [load-input digitize]]))

(def input (load-input 1 digitize))

"part 1: review a sequence of digits (your puzzle input) and find the sum of all digits that 
 match the next digit in the list. The list is circular, so the digit after the last 
 digit is the first digit in the list."

"part 2: instead of considering the next digit, it wants you to consider the digit halfway 
 around the circular list. Your list has an even number of elements"

(defn captcha [digits cycle-amount]
  (reduce (fn [A [x y]] (if (= x y) (+ A x) A)) 0
          (map list digits (drop cycle-amount (cycle digits)))))

(comment
  (captcha input 1)
  ;; => 1158

  (captcha input (/ (count input) 2))
  ;; => 1132
  )