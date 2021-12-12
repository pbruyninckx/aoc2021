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
  ([part graph]
   (get-paths graph #{"start"} "start" (= part 1)))
  ([graph small-visited last-visited small-twice]
   (cond (= "end" last-visited) 1
         :else
         (let [next-nodes (set/difference (graph last-visited)
                                          (if small-twice small-visited #{})
                                          #{"start"})]
           (if (empty? next-nodes)
             0
             (apply +
                    (for [node next-nodes]
                      (get-paths graph
                                 (if (small? node) (conj small-visited node) small-visited)
                                 node
                                 (or small-twice (small-visited node))))))))))

(defn solve1 [graph]
  (get-paths 1 graph))

(defn solve2 [graph]
  (get-paths 2 graph))

(defn -main []
  (let [input (parse-input "resources/input12.txt")]
    (doall (map #(println (% input)) [solve1 solve2]))))
