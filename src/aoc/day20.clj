(ns aoc.day20
  (:require [clojure.string :as str]))

(defn parse-input [f]
  (let [[enhance _ & img] (->> (slurp f)
                               str/split-lines
                               (map #(mapv {\. 0 \# 1} %)))]
    (assert (= (count img) (count (first img))))
    {:enhance enhance
     :img     (vec img)}))

(defn transpose [m]
  (apply mapv vector m))

(defn convolve [row factor]
  "Convolution with [1 factor factor^2]"
  (map #(+ %3 (* factor (+ %2 (* factor %1)))) row (drop 1 row) (drop 2 row)))

(defn pad [m pad-val pad-s]
  (let [s (+ (count m) (* 2 pad-s)) ; Assume square imgs/matrices
        padding-rows (repeat pad-s (repeat s pad-val))
        padding-col (vec (repeat pad-s pad-val)) ]
   (concat padding-rows
           (map (fn [r] (concat padding-col r padding-col)) m)
           padding-rows)))

(defn enhance-once [enhance img pad-char]
  (as-> img $
    (pad $ pad-char 2)
    (map #(convolve % 2) $)
    (transpose $)
    (map #(convolve % 8) $)
    (transpose $)
    (map #(map enhance %) $)))

(defn enhance-twice [enhance img]
  (as-> img $
    (enhance-once enhance $ 0)
    (enhance-once enhance $ (enhance 0))))

(defn solve [{:keys [img enhance]} times]
  (assert (even? times))
  (->> img
       (iterate (partial enhance-twice enhance))
       (drop (/ times 2))
       first
       (flatten)
       (reduce +)))


(defn -main []
  (let [data (parse-input "resources/input20.txt")]
    (doall (map #(println (solve data %)) [2 50]))))


