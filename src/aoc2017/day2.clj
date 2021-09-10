(ns aoc2017.day2
  (:require [aoc2017.utils :refer [load-input integers]]))

"Day 2: Corruption Checksum
 For each row, determine the difference between the largest value and the smallest value; 
 the checksum is the sum of all of these differences.
 
 What is the checksum for the spreadsheet in your puzzle input?"

(def input (load-input 2 integers))

(defn checksum [spreadsheet]
  (reduce (fn [A row]
            (+ A (- (apply max row) (apply min row))))
          0 spreadsheet))

(checksum input)

"Part 2: find the only two numbers in each row where one evenly divides the other - that is, 
 where the result of the division operation is a whole number
 find those numbers on each line, divide them, and add up each line's result."

(defn divisible-numbers
  "Given a sequence of digits, finds every number-pair [a b]
   that is evenly divisible, and returns the sum of a/b"
  [row]
  (apply + (for [a row b row
                 :when (< b a)]
             (if (zero? (mod a b)) (/ a b) 0))))

(comment
  (apply + (map divisible-numbers [[5 9 2 8]
                                   [9 4 7 3]
                                   [3 8 6 5]]))
  (apply + (map divisible-numbers input))
  ;; => 351
  )
