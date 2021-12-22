(ns aoc.day22
  (:require [clojure.string :as str]))

(defn java-binsearch [xs x]
  (java.util.Collections/binarySearch xs x compare))

(defn parse-line [l]
  (let [[state box] (str/split l #" ")
        numbers (->> (re-seq #"-?\d+" box)
                     (map #(Integer/parseInt %))
                     (partition 2)
                     (mapv vec))]
    {:state ({"on" true "off" false} state)
     :box numbers}))

(defn parse-input [f]
  (->> f
       slurp
       str/split-lines
       (map parse-line)))

(defn is-initialisation [{:keys [box]}]
  ; All numbers are either within or outside the init box
  (<= -50 (get-in box [0 0]) 50))

(defn compact-range [points]
  (let [points (->> points sort dedupe)
        point-ranges (map #(vector % %) points)
        inter-ranges (->> (map #(vector (inc %1) (dec %2)) points (rest points))
                          (filter #(apply <= %)))]
    (->> (concat point-ranges inter-ranges)
         sort
         vec)))

(defn extract-points [sides]
  (->> sides flatten sort dedupe))

(defn matching-ranges [ranges [side-min side-max]]
  (let [i (java-binsearch ranges [side-min side-min])]
    (->> ranges
         (drop i)
         (take-while #(<= (% 0) side-max)))))

(defn process-step [ranges cubes-on {:keys [state box]}]
  (println (count cubes-on))
  (apply (if state conj disj)
         cubes-on
         (for [x (matching-ranges (ranges 0) (box 0))
               y (matching-ranges (ranges 1) (box 1))
               z (matching-ranges (ranges 2) (box 2))]
           [x y z])))

(defn count-blocks [cell]
  (->> cell
       (map #(apply - (reverse %)))
       (map inc)
       (reduce *)))

(defn solve [steps]
  (let [ranges (->> steps
                    (map :box)
                    (#(for [dim (range 3)]
                        (->> (map (fn [box] (box dim)) %)
                             extract-points
                             compact-range)))
                    vec)]
    (->> steps
         (reduce (partial process-step ranges) #{})
         (map count-blocks)
         (reduce +)
         )))
    

(defn -main []
  (doall (let [steps (parse-input "resources/input22.txt")]
           (println (map #(solve (filter % steps)) [is-initialisation identity])))))


