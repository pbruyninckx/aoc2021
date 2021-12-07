(ns aoc.day07
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math :refer [abs]]))

(defn parse-data [f]
  (as-> f $
        (slurp $)
        (str/trim-newline $)
        (str/split $ #",")
        (mapv #(Integer/parseInt %) $)))

(defn solve1 [data]
  (let [sorted-data (sort data)
        median-data (nth sorted-data (quot (count sorted-data) 2))]
    (->> data
         (map #(abs (- % median-data)))
         (reduce +))))

(defn cost2 [p x]
  (let [d (abs (- p x))]
    (/ (* d (inc d)) 2)))

(defn total-cost [data p]
  (->> data
       (map (partial cost2 p))
       (reduce +)))

(defn solve2 [data]
  (let [unique-data (into #{} data)]
    (->> unique-data
         (map (partial total-cost data))
         (apply min))))


(defn -main []
  (let [data (parse-data "resources/input07.txt")]
    (println (solve1 data))
    (println (solve2 data))))
