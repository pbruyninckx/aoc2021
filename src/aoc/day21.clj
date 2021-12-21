(ns aoc.day21)

(def deterministic-die
  (map-indexed
    (fn [i t] {:n i :value t})
    (cycle (range 1 11))))

(defn create-player [pos]
  {:position pos
   :score 0})

(defn wrap-position [p]
  (-> p dec (rem 10) inc))

(defn turn [{:keys [position score] :as player} die]
  (let [[throws die] (split-at 3 die)
        throws (map :value throws)
        position (wrap-position (reduce + position throws))]
    [(assoc player 
            :position position
            :score (+ score position))
     die]))

(def switch-player {0 1 1 0})

(defn play [pos1 pos2]
  (let [players (mapv create-player [pos1 pos2])
        die deterministic-die]
    (loop [active-player 0
           players players
           die die]
      (let [[updated-player die] (turn (players active-player) die)]
        (if (>= (:score updated-player) 1000)
          (* (:n (first die)) (:score (players (switch-player active-player))))
          (recur (switch-player active-player)
                 (assoc players active-player updated-player)
                 die))))))

(defn -main []
  (println (play 8 5)))

