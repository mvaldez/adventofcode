(ns adventofcode.utils
  (:require [clojure.java.io :as io]))

(defn read-file
  "Reads file and returns a vector where the elements
   are each line. Converts number strings to longs
   else defaults to Symbols"
  [filename]
  (with-open [rdr (io/reader filename)]
    (->> (line-seq rdr)
         (map read-string) ;; converts str to signed numbers
         (reduce conj []))))

(defn len [s]
  (count s))

(defn eq [s1 s2]
  (= (len s1) (len s2)))
