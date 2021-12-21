(ns aoc.day21-2
  (:require [clojure.math.combinatorics :as combo]
            [clojure.algo.generic.functor :refer [fmap]]))

(def dirac-die
  "Distribution map per turn: {total rolls: frequency}"
  (->> (apply combo/cartesian-product (repeat 3 (range 1 4)))
       (map #(reduce + %))
       frequencies))

(def universe-factor
  "Each turn the number of universes multiplies with this factor"
  (->> dirac-die (map val) (reduce +)))

(defn create-player [pos]
  {:position pos
   :score 0})

(defn wrap-position [p]
  (-> p dec (rem 10) inc))

(defn single-turn [{:keys [position score] :as player} roll]
  (let [position (wrap-position (+ position roll))]
    (assoc player
       :position position
       :score (+ score position))))

(defn turn [distribution]
  (let [new-distributions
        (->> distribution
             (map
               (fn [[player player-frequency]]
                 (for [[roll roll-frequency] dirac-die]
                   [(single-turn player roll)
                    (* player-frequency roll-frequency)])))
             (apply concat)
             (reduce (fn [new-dist [k v]]
                       (update new-dist k (fnil (partial + v) 0)))
                     {}))]
    new-distributions))

(defn passive-turn [distribution factor]
  (fmap (partial * factor) distribution))

(def switch-player {0 1 1 0})

(defn count-universes [distribution]
  (->> distribution
       (map val)
       (reduce +)))

(defn play [pos1 pos2]
  (let [players (mapv #(hash-map (create-player %) 1) [pos1 pos2])]
    (loop [active-player 0
           players players
           games-won [0 0]]
      (if (= 0 (count-universes (first players)))
        (apply max games-won)
        (let [passive-player (switch-player active-player)
              updated-active (turn (players active-player))
              won (->> updated-active (filter #(>= (:score (key %)) 21)))
              num-won (->> won (map val) (reduce +))
              updated-active (apply dissoc updated-active (map key won))
              updated-passive (passive-turn (players passive-player)
                                            (/ (count-universes updated-active)
                                               (count-universes (players active-player))))]
          (recur (switch-player active-player)
                 (assoc players active-player updated-active
                                passive-player updated-passive)
                 (update games-won active-player (partial + num-won))))))))

(defn -main []
  (println (play 8 5)))

