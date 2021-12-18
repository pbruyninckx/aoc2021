(ns aoc.day17
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math]))

(defn parse-input [f]
  (let [[minx maxx miny maxy]
        (->> f
             slurp
             (re-seq #"-?\d+")
             (map #(Integer/parseInt %)))]
    {:x-target [minx maxx]
     :y-target [miny maxy]}))

(defn solve1 [{[min-y _] :y-target}]
  (assert (neg? min-y))
  (reduce + (range (- min-y))))

(defn num-steps-to-y-target [y y-target]
  (->> [0 y] ;[position, velocity]
       (iterate (fn [[p v]] [(+ p v) (dec v)]))
       (map-indexed (fn [i [p _]] [i p])) ; drop velocity
       (take-while (fn [[_ p]] (>= p (y-target 0))))
       (filter (fn [[_ p]] (<= (y-target 0) p (y-target 1))))
       (map first) ;index aka step
       vec))

(defn num-ways-to-x-target [steps x-target]
  (->> (for [v (range ((math/exact-integer-sqrt (dec (* 2 (x-target 0)))) 0)
                      (inc (x-target 1)))]
         (->> (iterate (fn [[p v]] [(+ p v) (max 0 (dec v))]) [0 v])
              (map first)
              (drop (first steps))
              (take (inc (- (last steps) (first steps))))
              (filter #(<= (x-target 0) % (x-target 1)))))
       (filter #(not= 0 (count %)))
       count))

    

(defn solve2 [{:keys [x-target y-target] :as data}]
  (let [y-range (range (y-target 0) (- (y-target 0)))]
    (->> (for [y y-range]
           (num-steps-to-y-target y y-target))
         (filter (complement empty?))
         frequencies
         (map (fn [[num-steps freq]]
                (* freq (num-ways-to-x-target num-steps x-target))))
         (reduce +))))



(defn -main []
  (let [data (parse-input "resources/input17.txt")]
    (doall (map #(println (% data)) [solve1 solve2]))))
