(ns adventofcode.core
  (:require [clojure.java.io :as io]))

(def input-file "src/adventofcode/input.txt")

(def input-seq
  (with-open [rdr (io/reader input-file)]
    (->> (line-seq rdr)
         (map read-string) ;; converts str to signed numbers
         (reduce conj [])))) ;; create a vector of numbers but no longer lazy seq

;;; Day 1 Part 1 ;;;
(->> input-seq
     (reduce +))

;;; Day 1 Part 2 ;;;
(defn search [x input acc]
  (let [[y & more] input
        sum (+ x y)
        seen (conj acc sum)]
    (if (and (not (contains? acc sum)) more)
      (recur sum more seen)
      (print sum))))

(search 0 (cycle input-seq) #{})
