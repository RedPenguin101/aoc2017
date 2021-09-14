(ns aoc2017.utils
  (:require [clojure.string :as str]))

(defn load-input [day parser]
  (map parser (str/split-lines (str/trim-newline (slurp (str "resources/day" day "input"))))))

(defn digitize [string]
  (map #(Integer/parseInt (str %)) string))

(defn integers [string]
  (let [ns (map #(Long/parseLong %) (re-seq #"-?\d+" string))]
    (if (= 1 (count ns))
      (first ns)
      ns)))

(defn words [string]
  (str/split string #" "))

(defn alpha-words [string]
  (re-seq #"[A-Za-z]+" string))

(comment
  (load-input 1 digitize)
  (load-input 4 words)
  (load-input 5 integers))