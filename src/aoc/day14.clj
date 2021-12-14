(ns aoc.day14
  (:require [clojure.string :as str]))

(defn parse-rule [s]
  "Parses AB -> C into [[A B] C], so it can be easily inserted into a map"
  [[(first s) (second s)] (last s)])

(defn polymer-pairs [polymer]
  "Convert the string representation of a polymer into a representation of its pairs.
  This will allow the algorithm to be faster.
  It will also contain the first and last element, as these should be added when counting the elements, and they never change."
  {:pairs (frequencies (map vector polymer (rest polymer)))
   :first-el (first polymer)
   :last-el  (last polymer)})



(defn parse-input [f]
  (->> f
       slurp
       str/split-lines
       ((fn [[polymer _ & rules ]]
          {:polymer (polymer-pairs polymer)
           :rules (into {} (map parse-rule rules))}))))

(defn step [pairs rules]
  (reduce (fn [new-pairs [[A B :as pair] cnt]]
            (if-let [I (rules pair)]
              (-> new-pairs
                  (update [A I] (fnil (partial + cnt) 0))
                  (update [I B] (fnil (partial + cnt) 0)))
              (update new-pairs pair (fnil (partial + cnt) 0))))
          {}
          pairs))

(defn count-elements [pairs first-el last-el]
  (let [ counts (reduce (fn [sum [[A B] cnt]]
                         (-> sum
                             (update A (fnil (partial + cnt) 0))
                             (update B (fnil (partial + cnt) 0)))) {} pairs) ]
    (-> counts (update first-el inc) (update last-el inc))))

(defn solve [{:keys [polymer rules]} steps]
  (let [{:keys [pairs first-el last-el]} polymer
        new-pairs
        (->> (iterate (fn [pairs] (step pairs rules)) pairs)
             (drop steps)
             first)
        _ (println new-pairs)
        counts (count-elements new-pairs first-el last-el)
        _ (println counts)
        min-el (val (apply min-key val counts))
        max-el (val (apply max-key val counts))]
    (/ (- max-el min-el) 2)))





(defn -main []
  (let [data (parse-input "resources/input14.txt")]
    (doall (map #(println (solve data %)) [10 40]))))
