(ns aoc.day01
  (:require [clojure.string :as str]))

(defn parse-input [f]
  (->> f
       slurp
       str/split-lines
       (map #(Integer/parseInt %))))


(defn solve [part v]
  (->> (map vector v (drop (if (= part 1) 1 3) v)) 
       (filter #(< (% 0) (% 1))) 
       count))

(defn -main []
  (let [input (parse-input "resources/input01.txt")]
    (println (solve 1 input))
    (println (solve 2 input))))
