(ns aoc.day10
  (:require [clojure.string :as str]))

(defn parse-input [f]
  (->> f
       slurp
       str/split-lines))

(def score-map {\) 3, \] 57, \} 1197, \> 25137})

(def close-map {\( \), \[ \], \{ \}, \< \> })

(defn examine [line]
  (loop [expected []
         [c & rest-line :as line] line]
    (cond
      (empty? line) {:expected expected}
      (= (peek expected) c) (recur (pop expected) rest-line)
      (close-map c) (recur (conj expected (close-map c)) rest-line)
      :else {:failing c})))

(defn solve1 [lines]
  (->> lines
       (map examine)
       (map :failing)
       (filter identity)
       (map score-map)
       (reduce +)))

(def score-map2 {\) 1, \] 2, \} 3, \> 4})

(defn solve2 [lines]
  (->> lines
       (map examine)
       (map :expected)
       (filter identity)
       (map reverse)
       (map #(reduce (fn [score c]
                       (+ (* 5 score)
                          (score-map2 c)))
                     0
                     %))
       sort
       ((fn [vals] (nth vals (quot (count vals) 2))))))

(defn -main []
  (let [input (parse-input "resources/input10.txt")]
    (doall (map #(println (% input)) [solve1 solve2]))))
