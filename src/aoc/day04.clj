(ns aoc.day04
  (:require [clojure.string :as str]))

(defn create-board [raster]
  (let [indexed-raster (->> raster
                            (map #(map-indexed vector %))
                            (map-indexed vector))
        num->pos (into {} (for [[ri row] indexed-raster
                                [ci val] row]
                            [val [ri ci]]))]
    {:raster   raster
     :num->pos num->pos
     :bingo    false}))

(defn parse-input [f]
  (let [all-input (->> (slurp f)
                       str/split-lines)
        numbers (as-> all-input $
                      (first $)
                      (str/split $ #",")
                      (map #(Integer/parseInt %) $))
        boards (->> all-input
                    (drop 2)
                    (mapv (fn [line] (->> (str/split line #" ")
                                          (filter (complement empty?))
                                          (mapv #(Integer/parseInt %)))))
                    (partition-by empty?)
                    (take-nth 2)
                    (map vec)
                    (map create-board))]
    {:numbers numbers
     :boards  boards}))

(defn score [{:keys [raster]} last-number]
  (* last-number
     (reduce + (->> raster flatten (filter pos-int?)))))

(defn bingo? [raster [r c]]
  (or (every? neg? (raster r))
      (every? neg? (map #(nth % c) raster))))

(defn mark-number [board n]
  (if-let [[r c] (get-in board [:num->pos n])]
    (let [new-board (assoc-in board [:raster r c] -1)]
      (assoc new-board :bingo (or (:bingo board)
                                  (bingo? (:raster new-board) [r c]))))
    board))

(defn solve1 [{:keys [numbers boards]}]
  (let [[n & rest-numbers] numbers
        marked-boards (map #(mark-number % n) boards)
        finished-boards (filter :bingo marked-boards)]
    (if (empty? finished-boards)
      (recur {:numbers rest-numbers
              :boards  marked-boards})
      (score (first finished-boards) n))))

(defn solve2 [{:keys [numbers boards]}]
  (let [[n & rest-numbers] numbers
        marked-boards (map #(mark-number % n) boards)]
    (if (and (= 1 (count boards))
             (:bingo (first marked-boards)))
      (score (->> (map vector boards marked-boards)
                  (filter (fn [[old-board _]] (not (:bingo old-board))))
                  first
                  second)
             n)
      (recur {:numbers rest-numbers
              :boards  (filter (complement :bingo) marked-boards)}))))

(defn -main []
  (let [f "resources/input04.txt"
        input (parse-input f)]
    (println (solve1 input))
    (println (solve2 input))))
