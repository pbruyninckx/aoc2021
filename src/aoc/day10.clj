(ns aoc.day10
  (:require [clojure.string :as str]))

(defn parse-input [f]
  (->> f
       slurp
       str/split-lines))

(def score-map {\) 3, \] 57, \} 1197, \> 25137})

(def close-map {\( \), \[ \], \{ \}, \< \> })

(defn first-incorrect [line]
  (loop [expected []
         [c & rest-line :as line] line]
    (cond
      (empty? line) nil
      (= (peek expected) c) (recur (pop expected) rest-line)
      (close-map c) (recur (conj expected (close-map c)) rest-line)
      :else c)))

(defn solve1 [lines]
  (->> lines
       (map first-incorrect)
       (filter identity)
       (map score-map)
       (reduce +)))

(defn solve2 [lines])

(defn -main []
  (let [input (parse-input "resources/input10.txt")]
    (doall (map #(println (% input)) [solve1 solve2]))))
