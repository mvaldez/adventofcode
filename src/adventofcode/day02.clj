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

(time
 (->> (u/read-file box-ids-file)
      (map (comp char-array str)) ;; seq of symbols -> char array
      (map #(search-id {} %)) ;; decompose each id
      (map count-twos-threes)
      (sum-seq)
      (reduce *)
      (println "Day 2 Part 1: ")))

;;; Day 2 Part 2 ;;;
;; https://adventofcode.com/2018/day/2#part2
(defn remove-differences
  [[x y]]
  {:pre [(u/eq x y)]}
  (->> (for [[i j] (map list (seq x) (seq y)) ;; zip
             :when (= i j)] ;; only if equal
         i)
       (apply str))) ;; convert to string

(defn hamming-distance
  "https://en.wikipedia.org/wiki/Hamming_distance"
  [[s1 s2]]
  {:pre [(u/eq s1 s2)]
   :post [(not (neg? %))]}
  (->> (for [[x y] (map list (seq s1) (seq s2)) ;; zip char seq
             :when (not (= x y))] ;; filter those not equal
         [x y])
       (count)))

(defn make-combos
  "Recursively makes every combination of the
   list elements, not including itself or dups.
   i.e. [a b c] => [[a b] [a c] [b c]]"
  ([acc [x & xs]]
   (let [r (for [i xs] (vector x i))]
     (if xs (recur (into acc r) xs)
         acc)))
  ([xs]
   (make-combos [] xs)))

(make-combos [1 2 3])

(time
 (->> (u/read-file box-ids-file)
      (map str)
      (make-combos)
      (group-by hamming-distance) ;; creates map of hamming dist -> list of id tuples
      (#(first (get % 1))) ;; get id's with a hamming distance of 1
      (remove-differences)
      (println "Day 2 Part 2: ")))
