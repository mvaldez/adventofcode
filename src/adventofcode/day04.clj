(ns adventofcode.day04
  (:require [adventofcode.utils :as u]
            [clojure.string :as s]))

(def input-file "guard_log.txt")

(defn days-in-month [x]
  (condp contains? x
    #{4 6 9 11} 30
    #{2} 28
    31))

(def months (cycle (range 1 13)))

(defn inc-month [month]
  (last (take (inc month) months)))

(defn adjust-date [year month day hour minute]
  (cond
    (not (= hour 23)) [year month day hour minute]
    (= day (days-in-month month)) [year (inc-month month) 1 0 0]
    :else [year month (inc day) hour minute]))

(defn parse-log
  ([log [line & xs]]
   (let [[date ts action id] (s/split line #" ")
         [year month day] (map u/parse-int (s/split date #"-"))
         [hour minute] (map u/parse-int (s/split ts #":"))
         [year month day hour minute] (adjust-date year month day hour minute)
         k (keyword (str month "-" day))
         m (cond
             (= "Guard" action) (if (contains? log k)
                                  (assoc-in log [k :id] id)
                                  (assoc log k {:id id :asleep [] :awake []}))
             (= "wakes" action) (if (contains? log k)
                                  (update-in log [k :awake] #(conj % minute))
                                  (assoc log k {:id nil :asleep [] :awake [minute]}))
             (= "falls" action) (if (contains? log k)
                                  (update-in log [k :asleep] #(conj % minute))
                                  (assoc log k {:id nil :asleep [minute] :awake []})))]
     (if xs (recur m xs) m)))
  ([xs]
   (parse-log {} xs)))

(defn calc-sleep-times
  [{:keys [asleep awake]}]
  (let [asleep (sort asleep)
        awake (sort awake)]
    (mapv (fn [a b] (into [] (range a b))) asleep awake)))

(defn gen-sleep-log
  ([logs [x & xs]]
   (let [id (keyword (:id x))
         sleep-times (calc-sleep-times x)
         sleep-total (reduce + (map count sleep-times))
         m (if (contains? logs id)
             (-> logs
                 (update-in [id :sleep-times] #(reduce conj % sleep-times))
                 (update-in [id :sleep-total] (partial + sleep-total)))
             (assoc logs id {:sleep-times sleep-times
                             :sleep-total sleep-total}))]
     (if xs (recur m xs) m)))
  ([xs]
   (gen-sleep-log {} xs)))

(defn gen-answer
  [[id log]]
  (let [id (u/parse-int (str id))
        minute (->> (:sleep-times log)
                    (reduce concat) ;; concat all sleep time logs
                    (frequencies) ;; find the num of times a minute appears
                    (apply max-key val) ;; find entry containing the most seen minute
                    (first))] ;; first val in entry is the minute val
    (println "Day 3 Part 1: " (* id minute))))

(->> (u/file->seq input-file)
     (map str)
     (parse-log) ;; group-by dates
     (vals) ;; ignore dates
     (gen-sleep-log)
     (apply max-key (comp :sleep-total second)) ;; get the kv of the max sleep total
     (gen-answer))
