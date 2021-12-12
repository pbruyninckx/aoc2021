(ns aoc.day12
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-add-vertex [graph line]
  (let [[a b] (str/split line #"-")
        add-edge (fn [graph a b] (update graph a (fnil #(conj % b) #{})))]
    (-> graph
        (add-edge a b)
        (add-edge b a))))

(defn parse-input [f]
  (->> f
       slurp
       str/split-lines
       (reduce parse-add-vertex {})))

(defn small? [cave]
  (Character/isLowerCase ^Character (first cave)))

(defn get-paths
  ([graph]
   (get-paths graph #{"start"} '("start")))
  ([graph small-visited [last-visited & _ :as path]]
   (cond (= "end" last-visited) (list path)
         :else
         (let [next-nodes (set/difference (graph last-visited) small-visited)]
           (if (empty? next-nodes)
             '()
             (apply concat
                    (for [node next-nodes]
                      (get-paths graph
                                 (if (small? node) (conj small-visited node) small-visited)
                                 (conj path node)))))))))

(defn solve1 [graph]
  (->> (get-paths graph)
       count))

(defn solve2 [graph])

(defn -main []
  (let [input (parse-input "resources/input12.txt")]
    (doall (map #(println (% input)) [solve1 solve2]))))
