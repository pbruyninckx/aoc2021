(ns aoc.day15
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

(defn parse-input [f]
  (let [grid (->> f
       slurp
       str/split-lines
       (mapv (fn [line] (mapv #(Character/digit % 10) line))))]
    {:size [(count grid) (count (first grid))]
     :grid grid}))

(defn neighbours [pos size]
  (->> (for [dpos [[1 0] [-1 0] [0 1] [0 -1]]]
         (mapv + pos dpos))
       (filter (fn [pos]
                 (and (every? (complement neg?) pos)
                      (every? identity (map < pos size)))))))

(defn min-cost [{:keys [size grid]} get-in-grid]
  (loop [cost {}
         queue (priority-map [0 0] 0)]
    (let [[pos pos-cost] (peek queue)
          queue (pop queue) ]
      (if (= pos (map dec size))
        pos-cost
        (recur (assoc cost pos pos-cost)
               (reduce (fn [q npos]
                         (let [new-cost (+ pos-cost (get-in-grid grid npos))]
                           (update q npos (fnil #(min new-cost %) new-cost))))
                       queue
                       (filter (complement cost) (neighbours pos size))))))))

(defn get-in-large [grid pos]
  (let [size [(count grid) (count (first grid))]
        opos (mapv rem pos size)
        tile-offset (apply + (map quot pos size))]
    (-> (get-in grid opos)
        (+ tile-offset)
        dec
        (rem 9)
        inc)))



(defn solve1 [{:keys [size grid] :as data}]
  (min-cost data get-in))

(defn solve2 [{:keys [size grid]}]
  (min-cost {:size (mapv (partial * 5) size)
             :grid grid}
            get-in-large))

(defn -main []
  (let [data (parse-input "resources/input15.txt")]
    (doall (map #(println (% data)) [solve1 solve2]))))
