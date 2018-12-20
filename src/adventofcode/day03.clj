(ns adventofcode.day03
  (:require [adventofcode.utils :as u]
            [clojure.string :as s]
            [cheshire.core :as json]
            [clojure.set :as set]))

(def fabric-claims-file "src/adventofcode/fabric_claims.txt")
(def more-than-one-claim (fn [a b] (if (> (:claim b) 1) {a b} nil)))

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

(defn flatten-coordinates [x]
  (let [id (:id x)
        area (:area x)]
    (into [] (map (fn [[a b]] [a b id]) area))))

(defn build-coordinate-map
  ([acc [x & xs]]
   (let [k  (coord->key [(first x) (second x)])
         id (last x)
         m (if (contains? acc k)
             (update-in acc [k :claim] inc)
             (assoc acc k {:id id :claim 1}))]
     (if xs (recur m xs)
         m)))
  ([xs]
   (build-coordinate-map {} xs)))

(defn filter-m [f m]
  (into {} (keep (fn [[k v]] (f k v)) m)))

(defn coord->key [[x y]]
  (keyword (str x "," y)))

;;; Day 3 Part 1 & 2 ;;;
;; https://adventofcode.com/2018/day/3
(time
 (let [coordinates (->> (u/file->seq fabric-claims-file)
                        (map (comp
                              calculate-coordinates
                              parse-location)))
       claims (->> coordinates
                   (map flatten-coordinates)
                   (reduce concat)
                   (build-coordinate-map)
                   (filter-m more-than-one-claim))]
   (->> claims
        (count)
        (println "Day 3 Part 1: "))
   (->> (map (fn [{:keys [id area]}]
               (if (not-any? #(contains? claims (coord->key %)) area) id))
             coordinates)
        (remove nil?)
        (first)
        (println "Day 3 Part 2: "))))
