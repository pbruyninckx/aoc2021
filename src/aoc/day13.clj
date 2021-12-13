(ns aoc.day13
  (:require [clojure.string :as str]))

(defn parse-points [lines]
  (->> lines
       (map (fn [line]
              (->> (str/split line #",")
                   (mapv #(Integer/parseInt %)))))))

(defn parse-folds [lines]
  (letfn [(parse-fold [line]
            (let [[_ direction position]
                  (re-find #"(.)=(\d+)" line)]
              {:direction (first direction)
               :position (Integer/parseInt position)}))]
    (map parse-fold lines)))



(defn parse-input [f]
  (->> f
       slurp
       str/split-lines
       (partition-by empty?)
       ((fn [[points _ folds]]
          {:points (parse-points points)
           :folds  (parse-folds folds)}))))

(defn fold [points {:keys [direction position]}]
  (letfn [(fold-pt [pt]
            (update pt
                    ({\x 0 \y 1} direction)
                    #(if (> % position) 
                       (- (* 2 position) %)
                       %)))]
    (into #{}
      (map fold-pt)
      points)))

(defn solve1 [{:keys [points folds]}]
  (-> points
      (fold (first folds))
      count))

(defn solve2 [{:keys [points folds]}]
  (let [points (reduce fold points folds)
        size   (mapv inc (apply map max points))
        canvas (vec (replicate (size 1) (vec (replicate (size 0) \ ))))
        code (reduce
               (fn [canvas [x y]] (assoc-in canvas [y x] \u2588 ))
               canvas
               points)]
    (doall (for [line code]
             (println (str/join line))))
    nil))


(defn -main []
  (let [input (parse-input "resources/input13.txt")]
    (doall (map #(println (% input)) [solve1 solve2]))))
