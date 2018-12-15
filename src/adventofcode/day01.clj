(ns adventofcode.day01
  (:require [adventofcode.utils :as u]))

(def freq-file "src/adventofcode/input.txt")

;;; Day 1 Part 1 ;;;
;; https://adventofcode.com/2018/day/1
(->> (u/read-file freq-file)
     (reduce +)
     (println "Day 1 Part 1:"))

;;; Day 1 Part 2 ;;;
;; https://adventofcode.com/2018/day/1#part2
(defn search [x input acc]
  (let [[y & more] input
        sum (+ x y)
        seen (conj acc sum)]
    (if (and (not (contains? acc sum)) more)
      (recur sum more seen)
      sum)))

(->> (search 0 (cycle (u/read-file freq-file)) #{})
     (println "Day 1 Part 2:"))
