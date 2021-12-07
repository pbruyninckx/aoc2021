(ns aoc.aoc.day07
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

(defn solve2 [data]
  (let [mean-data (/ (reduce + data) (count data))
        _ (println (count data))
        _ (println mean-data)]
    (->> data
         (map #(abs (- % mean-data)))
         (reduce +))))


(defn -main []
  (let [data (parse-data "resources/input07.txt")]
    (println (solve1 data))
    (println (solve2 data))))
