(ns aoc2017.day5
  (:require [aoc2017.utils :refer [load-input integers]]))

(def input (vec (load-input 5 integers)))

"list of integers
 relative jump instructions
 follow jumps until outside of list
 when you use a number, increase it by 1"

(defn find-exit
  ([instructions] (find-exit instructions 0 0))
  ([instructions pc steps]
   (if (>= pc (count instructions)) steps
       (let [instr (instructions pc)
             new-pc (+ pc instr)]
         (recur (update instructions pc inc)
                new-pc
                (inc steps))))))

(comment
  (find-exit [0 3 0 1 -3])

  (find-exit input)
  ;; => 381680
  )

"after each jump, if the offset was three or more, instead decrease it by 1. 
 Otherwise, increase it by 1 as before."

(defn find-exit2
  ([instructions] (find-exit2 instructions 0 0))
  ([instructions pc steps]
   (if (>= pc (count instructions)) steps
       (let [instr (instructions pc)]
         (recur (update instructions pc (if (>= instr 3) dec inc))
                (+ pc instr)
                (inc steps))))))

(comment
  (find-exit2 [0 3 0 1 -3])
  (find-exit2 input)
 ;; => 29717847
  )