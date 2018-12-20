(ns adventofcode.day03
  (:require [adventofcode.utils :as u]
            [clojure.string :as s]
            [cheshire.core :as json]
            [clojure.set :as set]))

;;; Day 3 Part 1 ;;;
;; https://adventofcode.com/2018/day/3
(def fabric-claims-file "src/adventofcode/fabric_claims.txt")

(defn calculate-coordinates [{:keys [id] {:keys [x y l w]} :coords}]
  (let [xi (range x (+ x l))
        yi (range y (+ y w))]
    (assoc {:id id} :area (for [x xi y yi] (vector x y)))))

(defn parse-location [s]
  (let [[id at coords area] (s/split s #" ")
        [x y] (map u/parse-int (s/split coords #","))
        [l w] (map u/parse-int (s/split area #"x"))]
    {:id (u/parse-int id)
     :coords {:x x
              :y y
              :l l
              :w w}}))

(defn build-mappings [x]
  (let [id (:id x)
        area (:area x)]
    (into [] (map (fn [[a b]] [a b id]) area))))

(defn build-map
  ([acc [x & xs]]
   (let [k (keyword (str (first x) (second x)))
         m (if (contains? acc k)
             (update acc k inc)
             (assoc acc k 1))]
     (if xs (recur m xs)
         (filter #(> (second %) 1) m))))
  ([xs]
   (build-map {} xs)))

;; Elapsed time: 113353.342489 msecs
(defn search-claims
  "Too slow.  Need to make this faster."
  ([acc [x & xs]]
   (let [r (for [i xs] (set/intersection x i))]
     (if xs (recur (set/union acc (apply set/union r)) xs)
         acc)))
  ([xs]
   (search-claims #{} xs)))

(time
 (->> (u/file->seq fabric-claims-file)
      (map (comp
            build-mappings
            calculate-coordinates
            parse-location))
      (reduce concat)
      (build-map)
      (count)))
