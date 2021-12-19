(ns aoc.day19
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.numeric-tower :refer [abs]]
            [clojure.math.combinatorics :refer [permutations cartesian-product]])
  (:import (clojure.lang PersistentQueue)))

(def empty-queue
  PersistentQueue/EMPTY)

(defn parse-point [line]
  (->> (str/split line #",")
       (mapv #(Integer/parseInt %))))

(defn parse-input [f]
  (->> f
       slurp
       str/split-lines
       (partition-by empty?)
       (take-nth 2)
       (map rest)
       (map #(mapv parse-point %))
       (mapv #(apply hash-set %))))

(defn cross-product [[x1 y1 z1] [x2 y2 z2]]
  [(- (* y1 z2) (* z1 y2))
   (- (* z1 x2) (* x1 z2))
   (- (* x1 y2) (* y1 x2))])

(def rotate-fns
  (->> (for [axis-perm (permutations [0 1 2])
             sign-perm (apply cartesian-product (repeat 3 [identity -]))]
         (fn [point]
           (mapv (mapv #(%1 %2) sign-perm point) axis-perm)))
       (filter (fn [rot-fn]
                 (let [[x y z] (map rot-fn [[1 0 0] [0 1 0] [0 0 1]])]
                   (= (cross-product x y) z))))
       (into [])))

(defn translate-pt [p delta]
  (mapv + p delta))

(defn inv-pt [p]
  (mapv - p))

(defn create-move-fn [fixed-pt var-pt rot-fn]
  (fn [pt] (-> pt
               (translate-pt (inv-pt var-pt))
               rot-fn
               (translate-pt fixed-pt))))

(defn match-points [fixed, points]
  "Returns [f, (into {} (map f points)] or nil."
  (assert (set? fixed))
  (->> (for [fp (drop 11 fixed) p (drop 11 points) rot-fn rotate-fns]
         (let [move-fn (create-move-fn fp p rot-fn)
               moved-pts (map move-fn points)]
           (if (>= (count (filter fixed moved-pts)) 12)
             [move-fn (into #{} (map move-fn points))]
             nil)))
       (filter identity)
       first))

(defn match-scanners [scanners]
  (let [num-scanners (count scanners)]
    (loop [points-by-scanner (assoc (vec (repeat num-scanners nil)) 0 (first scanners))
           move-fns (assoc (vec (repeat num-scanners nil)) 0 identity)
           [fixed-scanner & _ :as fixed-queue] (conj empty-queue 0)
           [testing & _ :as to-test] (range 1 num-scanners)]
      (cond (empty? fixed-queue)
            [points-by-scanner move-fns]
            (empty? to-test)
            (recur points-by-scanner move-fns (pop fixed-queue) (filter (complement points-by-scanner) (range num-scanners)))
            :else
            (if-let [[move-fn moved-points] (match-points (points-by-scanner fixed-scanner) (scanners testing))]
              (recur (assoc points-by-scanner testing moved-points)
                     (assoc move-fns testing move-fn)
                     (conj fixed-queue testing)
                     (rest to-test))
            (recur points-by-scanner move-fns fixed-queue (rest to-test)))))))

(defn solve2 [move-fns]
  (let [scanner-locations ((apply juxt move-fns) [0 0 0])]
    (->> (for [s1 scanner-locations s2 scanner-locations]
           (->> (map - s1 s2)
                (map abs)
                (reduce +)))
         (apply max))))

(defn -main []
  (let [data (parse-input "resources/input19.txt")]
    (let [[moved-points move-fns] (match-scanners data)]
      (println (count (apply set/union moved-points)))
      (println (solve2 move-fns)))))
