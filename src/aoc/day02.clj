(ns aoc.day02
  (:require [clojure.string :as str]))

(defn parse-input []
  (->> "input02.txt"
       slurp
       str/split-lines
       (map (fn [l]
              (let [[w i] (str/split l #" ")]
                [(keyword w) (Integer/parseInt i)])))))

(defn solve1 [input]
  (->> input
       (reduce
         (fn [[h d] [dir i]]
           (case dir
             :forward [(+ h i) d]
             :down [h (+ d i)]
             :up [h (- d i)]))
         [0 0])
       (apply *)))

(defn solve2 [input]
  (->> input
       (reduce
         (fn [[h d aim] [dir i]]
           (case dir
             :forward [(+ h i) (+ d (* i aim)) aim]
             :down [h d (+ aim i)]
             :up [h d (- aim i)]))
         [0 0 0])
       (take 2)
       (apply *)))


(defn -main []
  (let [input (parse-input)]
    (println (solve1 input))
    (println (solve2 input))))
