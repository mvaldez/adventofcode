(ns adventofcode.utils
  (:require [clojure.java.io :as io]))

(defn read-file
  "Reads file and returns a vector where the elements
   are each line. Converts number strings to longs
   else defaults to Symbols"
  [filename]
  (with-open [rdr (io/reader (.getFile (clojure.java.io/resource filename)))]
    (->> (line-seq rdr)
         (map read-string) ;; converts str to signed numbers
         (reduce conj []))))

(defn file->seq
  [filename]
  (with-open [rdr (io/reader (.getFile (clojure.java.io/resource filename)))]
    (->> (line-seq rdr)
         (reduce conj []))))

(defn len [s]
  (count s))

(defn eq [s1 s2]
  (= (len s1) (len s2)))

(defn remove-last [s]
  (subs s 0 (- (len s) 1)))

(defn remove-first [s]
  (subs s 1))

(defn parse-int [s]
  (Integer. (re-find  #"\d+" s)))

(defn deep-merge [a b]
  (merge-with (fn [x y]
                (cond (map? y) (deep-merge x y)
                      (vector? y) (concat x y)
                      (set? y) (concat x y)
                      :else y))
              a b))

(defn pp-map [m]
  (clojure.pprint/pprint m))
