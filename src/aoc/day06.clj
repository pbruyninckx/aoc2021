(ns aoc.day06
  (:require [clojure.string :as str]))

(defn parse-data [f]
  (as-> f $
        (slurp $)
        (str/trim-newline $)
        (str/split $ #",")
        (map #(Integer/parseInt %) $)
        (frequencies $)
        (reduce (fn [acc [k v]] (assoc acc k v))
                (vec (repeat (inc 8) 0))
                $)
        {:freqs $
         :index 0}))

(defn mod-index [i]
  (mod i (inc 8)))

(defn step [{:keys [freqs index] :as data}]
  (let [i0 index
        i7 (mod-index (+ 7 index))]
    (-> data
        (update :index (comp mod-index inc))
        (update-in [:freqs i7] (partial + (freqs i0))))))


(defn solve [part data]
  (->> (nth (iterate step data) (if (= part 1) 80 256))
       :freqs
       (reduce +)))

(defn -main []
  (let [input (parse-data "resources/input06.txt")]
    (println (solve 1 input))
    (println (solve 2 input))))

