(ns adventofcode.day02
(:require [adventofcode.utils :as u]))

(def box-ids-file "src/adventofcode/box_ids.txt")

;;; Day 2 Part 1 ;;;
;; https://adventofcode.com/2018/day/2
(defn search-id
  "accepts a map accumulator and char array.
   Returns a lazy sequence of vectors of kv
   matching values 2 and 3"
  [acc [x & xs]]
  (let [m (if (contains? acc (keyword (str x)))
            (update acc (keyword (str x)) inc)
            (assoc acc (keyword (str x)) 1))]
    (if xs (recur m xs)
        (filter (comp #{2 3} last) m))))

(defn count-twos-threes
  "Given a sequence of vectors of kv returns
   a vector where the first element is the
   count of distinct two values and the second
   is the count of distinct three values"
  [xs]
  (let [x (count (filter #{2} (distinct (vals xs))))
        y (count (filter #{3} (distinct (vals xs))))]
    [x y]))

(defn sum-seq
  "Given a sequence of vectors returns the a
   two element vector with the sum of first
   elements followed by the sum of the second"
  [xs]
  (let [x (->> xs
               (map (comp first))
               (reduce +))
        y (->> xs
               (map (comp second))
               (reduce +))]
    [x y]))

(->> (u/read-file box-ids-file)
     (map (comp char-array str)) ;; seq of symbols -> char array
     (map #(search-id {} %)) ;; decompose each id
     (map count-twos-threes)
     (sum-seq)
     (reduce *)
     (println "Day 2 Part 1: "))

;;; Day 2 Part 2 ;;;
;; https://adventofcode.com/2018/day/2#part2
