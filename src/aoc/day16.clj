(ns aoc.day16
  (:require [clojure.string :as str]))

(defn bin->int [s]
  (reduce (fn [acc digit] (+ (* 2 acc) digit)) s))

(defn parse-input [s]
  (->> s
       str/trim
       (map #(Character/digit % 16))
       (map #(Integer/toBinaryString %))
       (map #(seq (format "%4s" %)))
       flatten
       (map {\0 0 \  0 \1 1})))

(defn parse-number [bits]
  (let [partitions (partition 5 bits)
        num-partitions (inc (count (take-while #(= 1 (first %)) partitions)))
        number (->> partitions
                    (take num-partitions)
                    (map rest)
                    flatten
                    bin->int)
        bits (drop (* 5  num-partitions) bits)]
    {:value number
     :bits bits}))

(defn parse-packet [bits]
  "Parses a packet and returns a map with the following data:
  {:bits    remaining bits after the package was read
   :version
   :id
   :subpackets (optional [])}"
  (let [version (bin->int (take 3 bits))
        id      (bin->int (take 3 (drop 3 bits)))
        bits    (drop 6 bits)]
    (merge {:version version
            :id      id}
    (if
      (= id 4)
      (parse-number bits)
      ;operator
      (let [[length-type & bits] bits]
        (case length-type
          0
          (let [packet-length (bin->int (take 15 bits))
                bits (drop 15 bits)]
            (loop [subpackets []
                   subbits (take packet-length bits)]
              (if (empty? subbits)
                {:subpackets subpackets
                 :bits       (drop packet-length bits)}
                (let [new-subpacket (parse-packet subbits)]
                  (recur (conj subpackets new-subpacket)
                         (:bits new-subpacket))))))
          1
          (let [num-packets (bin->int (take 11 bits))
                bits (drop 11 bits)]
            (loop [subpackets []
                   subbits bits]
              (if (= num-packets (count subpackets))
                {:subpackets subpackets
                 :bits       subbits}
                (let [new-subpacket (parse-packet subbits)]
                  (recur (conj subpackets new-subpacket)
                         (:bits new-subpacket))))))))))))

(defn sum-versions [packet]
  (let [sub-sum (reduce + (map sum-versions (:subpackets packet)))]
    (+ (:version packet) sub-sum)))

(defn calculate [{:keys [subpackets id value]}]
  (let [sub-results (mapv calculate subpackets)]
    (case id
      0 (reduce + sub-results)
      1 (reduce * sub-results)
      2 (apply min sub-results)
      3 (apply max sub-results)
      4 value
      5 (if (apply > sub-results) 1 0)
      6 (if (apply < sub-results) 1 0)
      7 (if (apply = sub-results) 1 0))))


(defn solve1 [bits]
  (let [packet (parse-packet bits)]
    (sum-versions packet)))

(defn solve2 [bits]
  (let [packet (parse-packet bits)]
    (calculate packet)))

(defn -main []
  (let [bits (parse-input (slurp "resources/input16.txt"))]
    (doall (map #(println (% bits)) [solve1 solve2]))))
