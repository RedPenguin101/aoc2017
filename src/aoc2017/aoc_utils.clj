(ns aoc2017.utils
  (:require [clojure.string :as str]))

(defn load-input [day parser]
  (parser (str/trim-newline (slurp (str "resources/day" day "input")))))

(defn digitize [string]
  (map #(Integer/parseInt (str %)) string))

(comment
  (load-input 1 digitize))