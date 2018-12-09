(ns adventofcode.core
  (:require [clojure.java.io :as io]))

;;; Day 1 ;;;
(with-open [rdr (io/reader "src/adventofcode/input.txt")]
  (->> (line-seq rdr)
       (map read-string) ;; converts to signed number
       (reduce +) ;; sums the sequence
       (print)))
