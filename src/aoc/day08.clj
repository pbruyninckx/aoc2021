(ns aoc.day08
  (:require [clojure.string :as str]))

(defn parse-input [f]
  (->> f
       slurp
       (str/split-lines)
       (map (fn [line] (->> (str/split line #" ")
                            (map set)
                            (#(hash-map :in (take 10 %) :out (drop 11 %))))))))

(defn solve1 [data]
  (->> data
       (map :out)
       flatten
       (filter #(#{2 3 4 7} (count %)))
       count))

(defn solve2 [data])

(defn -main []
  (let [input (parse-input "resources/input08.txt")]
    (println (solve1 input))
    (println (solve2 input))))
