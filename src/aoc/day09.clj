(ns aoc.day09
  (:require [clojure.string :as str]))

(defn parse-input [f]
  (let [grid
        (->> f
             slurp
             str/split-lines
             (map str/trim)
             (mapv (fn [line]
                     (mapv #(Character/digit % 10) line))))]
    {:size [(count grid) (count (first grid))]
     :grid grid}))

(defn neighbour-pos [[r c] [nrow ncol]]
  (->>
    (for [[rr cc] [[(dec r) c] [(inc r) c]
                   [r (dec c)] [r (inc c)]]
          :when (and (< -1 rr nrow)
                     (< -1 cc ncol))]
      [rr cc])))

(defn neighbour-vals[{:keys [size grid]} [r c :as pos]]
  (let [positions (neighbour-pos pos size)]
    (map #(get-in grid %) positions)))

(defn is-minimum [{:keys [size grid] :as field} pos]
  (every? #(< (get-in grid pos) %)
          (neighbour-vals field pos)))

(defn get-minima[{:keys [size grid] :as field}]
  (->> (for [r (range (size 0))
             c (range (size 1))]
         [r c])
       (filter #(is-minimum field %))))

(defn solve1 [{:keys [grid] :as field}]
  (->> (get-minima field)
       (map #(get-in grid %))
       (map inc)
       (reduce +)))

(defn get-basin-size [{:keys [size grid] :as field}
                       pos]
  (loop [seen #{}
         todo #{pos}]
    (if (empty? todo)
      (count seen)
      (let [[r c :as pos] (first todo)
            new-neighbs (->> (neighbour-pos pos size)
                             (filter #(not= 9 (get-in grid %)))
                             (filter (complement seen)))]
        (recur (conj seen pos)
               (into (disj todo pos)
                     new-neighbs))))))

(defn solve2 [field]
  (->> (get-minima field)
       (map (partial get-basin-size field))
       (sort >)
       (take 3)
       (reduce *)))

(defn -main []
  (let [input (parse-input "resources/input09.txt")]
    (println (solve1 input))
    (println (solve2 input))))
