(ns aoc2017.utils
  (:require [clojure.string :as str]))

(defn load-input
  ([day] (load-input day identity))
  ([day row-parser]
   (map row-parser (str/split-lines (str/trim-newline (slurp (str "resources/day" day "input")))))))

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

(defn try-number [string]
  (try (Long/parseLong string)
       (catch NumberFormatException e string)))

(try-number "hello")
(try-number "123")
(try-number "-123")

(defn split-row-and-atom [re]
  (fn [row]
    (map try-number (str/split row re))))

(comment
  (load-input 1 digitize)
  (load-input 4 words)
  (load-input 5 integers))