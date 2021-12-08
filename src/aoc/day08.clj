(ns aoc.day08
  (:require [clojure.string :as str]
            [clojure.set :as set]))

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

(defn filter-by-count
  ([size sets]
   (filter-by-count identity size sets))
  ([f size sets]
   (filter #(#{size} (count (f %))) sets)))

(defn get-number-dict [input]
  "Returns a map of input-maps to digits"
  (let [one (->> input (filter-by-count 2) first)
        four (->> input (filter-by-count 4) first)
        seven (->> input (filter-by-count 3) first)
        eight (->> input (filter-by-count 7) first)
        nine (->> input (filter-by-count 6)
                  (filter-by-count #(set/difference % four) 2) first)
        six (->> input (filter-by-count 6)
                 (filter-by-count #(set/difference % one) 5) first)
        five (->> input (filter-by-count 5)
                  (filter-by-count #(set/difference nine %) 1)
                  (filter-by-count #(set/difference % one) 4 ) first)
        zero (->> input (filter-by-count 6)
                  (filter-by-count #(set/intersection nine %) 5)
                  (filter-by-count #(set/intersection six %) 5) first)
        two (->> input (filter-by-count 5)
                 (filter-by-count #(set/union five %) 7) first)
        three (->> (set/difference (set input) #{zero one two four five six seven eight nine})
                   first)]
    {zero 0
     one 1
     two 2
     three 3
     four 4
     five 5
     six 6
     seven 7
     eight 8
     nine 9}))

(defn solve-line [{:keys [in out]}]
  (let [number-dict (get-number-dict in)
        out-digits (map number-dict out)]
    (reduce (fn [acc d] (+ d (* 10 acc))) out-digits)))

(defn solve2 [data]
  (->> data
       (map solve-line)
       (reduce +)))

(defn -main []
  (let [input (parse-input "resources/input08.txt")]
    (println (solve1 input))
    (println (solve2 input))))
