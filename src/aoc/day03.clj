(ns aoc.day03
  (:require [clojure.string :as str]))

(defn parse-input []
  (->> "input03.txt"
       slurp
       str/split-lines
       (map #(mapv {\0 0 \1 1} %))))

(defn bin->int [b]
  (reduce
    (fn [acc val]
      (-> acc
          (* 2)
          (+ val)))
    b))

(defn gamma [input]
  (->> input
       (apply map vector)
       (map frequencies)
       (map #(key (apply max-key val %)))
       bin->int))

(defn eps [input gamma]
  (- (int (.pow 2M (count (first input)))) gamma 1))

(defn solve1 [input]
  (let [g (gamma input)
        e (eps input g)]
    (* g e)))


(defn get-rating [numbers f]
  (if (empty? (first numbers))
    '()
    (let [freqs (frequencies (map first numbers))
          ret (key (apply f (fn [[k v]] (+ v (/ k 2))) freqs))
          rest-numbers (->> numbers
                            (filter #(= ret (first %)))
                            (map rest))]
      (cons ret (get-rating rest-numbers f)))))



(defn solve2 [input]
  (->> [min-key max-key]
       (map #(get-rating input %))
       (map bin->int)
       (reduce *)))

(defn -main []
  (let [input (parse-input)]
    (println (take 4 input))
    (println (solve1 input))
    (println (solve2 input))))
