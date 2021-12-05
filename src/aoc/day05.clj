(ns aoc.day05
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as math :refer [abs]]))

(defn parse-input [f]
  "Returns [x0 y0 x1 y1] for each line in the input file f."
  (->> f
       slurp
       str/split-lines
       (map (fn [line] (->> (str/split line #"\D+")
                            (mapv #(Integer/parseInt %)))))))

(defn horizontal? [[_ y0 _ y1]]
  (= y0 y1))

(defn vertical? [[x0 _ x1 _]]
  (= x0 x1))

(defn get-points [[x0 y0 x1 y1 :as line]]
  (let [dx (compare x1 x0)
        dy (compare y1 y0)
        n (max (abs (- y1 y0)) (abs (- x1 x0)))]
    (for [i (range (inc n))]
      [(+ x0 (* i dx))
       (+ y0 (* i dy))])))

(defn solve [part lines]
  (->> ((if (= part 1)
         #(filter (some-fn horizontal? vertical?) %)
         identity)
        lines)
       (map get-points)
       (apply concat)
       frequencies
       (filter #(>= (val %) 2))
       count))

(defn -main []
  (let [f "resources/input05.txt"
        input (parse-input f)]
    (println (solve 1 input))
    (println (solve 2 input))))
