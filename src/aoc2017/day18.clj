(ns aoc2017.day18
  (:require [aoc2017.utils :refer [load-input split-row-and-atom]]))

(def input (vec (load-input 18 (split-row-and-atom #" "))))
(take 3 input)
;; => (("set" "i" 31) ("set" "a" 1) ("mul" "p" 17))

"Day 18: Duet
 registers: letter, hold integer
 
 instr:
 snd X
 set, add, mod (all X Y, X=reg, Y=val or reg)
 jgz 
 rcv X: recovers the last snd when RX not zero. 
 
 Part 1: What is the value of the recovered frequency (the value of the most recently played sound) 
 the first time a rcv instruction is executed with a non-zero value?"

{:pc 0
 :reg {"a" 1}
 :instr []}

(defn instr-constr [f]
  (fn [registers x y]
    (if (string? y)
      (update registers x f (registers y 0))
      (update registers x f y))))

(def reg-instr
  {"set" (fn [registers x y]
           (if (string? y)
             (assoc registers x (registers y))
             (assoc registers x y)))
   "add" (instr-constr +)
   "mul" (instr-constr (fnil * 0))
   "mod" (instr-constr mod)})

(defn tick [computer]
  (let [[op x y] (get-in computer [:instr (:pc computer)])]
    #_(println [op x y])
    (cond
      (nil? op) (assoc computer :terminated true)

      (contains? reg-instr op)
      (-> computer
          (update :reg #((reg-instr op) % x y))
          (update :pc inc))

      (= "snd" op)
      (-> computer
          (update :play-stack conj (get-in computer [:reg x]))
          (update :pc inc))

      (and (= "rcv" op) (not (zero? (get-in computer [:reg x] 0))))
      (-> computer
          (update :recover-stack conj (first (:play-stack computer)))
          (update :pc inc))

      (and (= "jgz" op) (pos-int? (get-in computer [:reg x] 0)))
      (update computer :pc + (if (string? y) (get-in computer [:reg y] 0) y))

      :else (update computer :pc inc))))

(defn setup [instr]
  {:pc 0
   :reg {}
   :instr instr})

(comment
  (let [ex [["set" "a" 1] ["add" "a" 2] ["mul" "a" "a"] ["mod" "a" 5] ["snd" "a"] ["set" "a" 0] ["rcv" "a"] ["jgz" "a" -1] ["set" "a" 1] ["jgz" "a" -2]]]
    (->> (setup ex)
         (iterate tick)
         (drop-while #(empty? (:recover-stack  %)))
         (first)
         (:recover-stack)))

  (->> (setup input)
       (iterate tick)
       (drop-while #(empty? (:recover-stack  %)))
       (first)
       (:recover-stack))
  ;; => (3188)
  )

"Part 2:
 2 programs: number 0 and 1
 snd X sends the value of X to the other program.
 rcv X receives the next value and stores it in register X
 register p is initiated with program number
 
 Once both of your programs have terminated (regardless of what caused them to do so), 
 how many times did program 1 send a value?
"

(defn setup2 [prog-id instr]
  {:pc 0
   :reg {"p" prog-id}
   :instr instr})

(defn tick2 [computer]
  (let [[op x y] (get-in computer [:instr (:pc computer)])]
    #_(println [op x y])
    (cond
      (nil? op) (assoc computer :terminated true)

      (contains? reg-instr op)
      (-> computer
          (update :reg #((reg-instr op) % x y))
          (update :pc inc))

      (= "snd" op)
      (let [snd (if (string? x) (get-in computer [:reg x]) x)]
        (-> computer
            (update :outputs conj snd)
            (update :send-count (fnil inc 0))
            (update :pc inc)))

      (= "rcv" op)
      (if (empty? (:inputs computer))
        (assoc computer :waiting true)
        (-> computer
            (update :reg #((reg-instr "set") % x (first (:inputs computer))))
            (update :inputs rest)
            (update :pc inc)))

      (and (= "jgz" op) (pos-int? (get-in computer [:reg x] 0)))
      (update computer :pc + (if (string? y) (get-in computer [:reg y] 0) y))

      :else (update computer :pc inc))))

(defn run-until-wait
  ([computer inputs] (run-until-wait (assoc computer :inputs inputs)))
  ([computer]
   (if (:waiting computer)
     computer
     (recur (update (tick2 computer) :cycles (fnil inc 0))))))

(run-until-wait (setup2 0 input))

(defn no-outputs? [computer]
  (empty? (:outputs computer)))

(defn get-outputs [computer]
  (or (:outputs computer)
      (list)))

(defn duet [c1 c2 it]
  (println "iteration" it)
  (println "c1: cycles:" (:cycles c1) "outputs:" (count (:outputs c1)) "sent" (:send-count c1))
  (println "c2: cycles:" (:cycles c2) "outputs:" (count (:outputs c2)) "sent" (:send-count c2))
  (cond
    (> it 100) [:break c1 c2]
    (and (no-outputs? c1) (no-outputs? c2))
    [c1 c2]
    :else (recur (-> c1
                     (assoc :inputs (get-outputs c2))
                     (assoc :outputs (list))
                     (dissoc :waiting)
                     (run-until-wait))
                 (-> c2
                     (assoc :inputs (get-outputs c1))
                     (assoc :outputs (list))
                     (dissoc :waiting)
                     (run-until-wait))
                 (inc it))))

(duet (run-until-wait (setup2 0 input))
      (run-until-wait (setup2 1 input))
      0)

(-> (setup2 1 input)
    run-until-wait
    (assoc :inputs (get-outputs (run-until-wait (setup2 0 input))))
    (dissoc :waiting)
    run-until-wait)

(let [instr [["snd" 1] ["snd" 2] ["snd" "p"] ["rcv" "a"] ["rcv" "b"] ["rcv" "c"] ["rcv" "d"]]]
  (duet (run-until-wait (setup2 0 instr))
        (run-until-wait (setup2 1 instr))
        0)
  #_(run-until-wait (setup2 0 instr)))

