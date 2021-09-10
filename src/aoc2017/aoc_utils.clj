(ns aoc2017.utils
  (:require [clojure.string :as str]))

(defn load-input [day parser]
  (map parser (str/split-lines (str/trim-newline (slurp (str "resources/day" day "input"))))))

(defn digitize [string]
  (map #(Integer/parseInt (str %)) string))

(defn integers [string]
  (map #(Integer/parseInt %) (re-seq #"\d+" string)))

(comment
  (load-input 1 digitize))