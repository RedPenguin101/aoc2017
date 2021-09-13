(ns aoc2017.day4
  (:require [aoc2017.utils :refer [load-input words]]))

(def input (load-input 4 words))

"Part 1: a valid passphrase must contain no duplicate words
 How many passphrases are valid?"

(defn valid? [passphrase]
  (= (count passphrase) (count (set passphrase))))

(comment
  (count (filter valid? input))
  ;; => 477
  )

"Now, a valid passphrase must contain no two words that are anagrams of each other"

(defn valid2? [passphrase]
  (let [sorted (map sort passphrase)]
    (= (count sorted) (count (set sorted)))))

(comment
  (count (filter valid2? input))
  ;; => 167
  )