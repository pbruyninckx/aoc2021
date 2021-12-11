(ns aoc.day11
  (:require [clojure.string :as str]))

(defn parse-input [f]
  (let [data
        (->> f
             slurp
             str/split-lines
             (mapv #(mapv (fn [^Character c] (Character/digit c 10)) %)))]
    {:data data
     :num-flashed 0
     :size [(count data) (count (first data))]}))

(defn get-neighbours [[r c]]
  (for [rr [(dec r) r (inc r)]
        cc [(dec c) c (inc c)]
        :when (not= [r c] [rr cc])]
    [rr cc]))

(defn step [{:keys [data size] :as grid}]
  (loop [data data
         [pos & rest-pos :as to-inc] (for [r (range (size 0)) c (range (size 1))] [r c])
         has-flashed #{}]
    (if (empty? to-inc)
      (assoc grid :data data :num-flashed (count has-flashed))
      (case (get-in data pos)
        ; Out of bounds - do nothing
        nil (recur data rest-pos has-flashed)
        ; Increase only if not yet flashed
        0 (recur (if (has-flashed pos)
                   data
                   (update-in data pos inc))
                 rest-pos has-flashed)
        ; Flash - set to zero, and increase neighbours (later)
        9 (recur (assoc-in data pos 0)
                 (concat (get-neighbours pos) rest-pos)
                 (conj has-flashed pos))
        ; Default
        (recur (update-in data pos inc) rest-pos has-flashed)))))

(defn solve1 [grid]
  (->> (iterate step grid)
       (map :num-flashed)
       (take 101)
       (reduce +)))

(defn solve2 [grid]
  (->> (iterate step grid)
       (map-indexed (fn [i g] {:index i :grid g}))
       (filter #(= (apply * (:size grid)) (get-in % [:grid :num-flashed])))
       first
       :index))

(defn -main []
  (let [input (parse-input "resources/input11.txt")]
    (doall (map #(println (% input)) [solve1 solve2]))))