(ns aoc.core
  (:require [clojure.string :refer [split trim]]))

(defn d1-in []
  (map #(Long. %) (split (slurp "resources/day1.txt") #"\n")))

(defn fuel [m]
  (- (long (/ m 3)) 2))

(defn d1-1 [ms]
  (reduce #(+ %1 (fuel %2)) 0 ms))

(defn fuel-rec [m]
  (let [f (fuel m)]
    (if (<= f 0)
    0
    (+ f (fuel-rec f)))))

(defn d1-2 [ms]
  (reduce #(+ %1 (fuel-rec %2)) 0 ms))

;;;

(defn d2-in []
  (vec (map #(Long. %) (split (trim (slurp "resources/day2.txt")) #","))))

;; run with 12 and 2 for d2-1
(defn d2-1 [input p1 p2]
  (let [mod-input (assoc input 1 p1 2 p2)
        op (fn [f i xs]
             (assoc xs (xs (+ i 3)) (f (xs (xs (+ i 1))) (xs (xs (+ i 2))))))]
    (loop [xs mod-input i 0]
      (condp = (xs i)
        1 (recur (op + i xs) (+ i 4))
        2 (recur (op * i xs) (+ i 4))
        (first xs)))))

(defn d2-2 []
  (let [magic-num 19690720
        input (d2-in)]
    (->> (for [p1 (range 100) p2 (range 100)]
           [(d2-1 input p1 p2) p1 p2])
         (filter #(= magic-num (first %)))
         first
         (#(+ (* 100 (second %)) (last %))))))

(defn d3-in []
  (map #(split % #",") (split (slurp "resources/day3.txt") #"\n")))

(defn d3-1 [wires]
  (let [parse #(vector (first %) (Long. (apply str (rest %))))
        u-fn (fn [p i f r] (for [x (range 1 (inc r))] (update p i #(f % x))))
        r-fn (fn [acc x]
               (let [[d run] (parse x)
                     prev (last acc)]
                 (vec (concat acc (condp = d
                                    \R (u-fn prev 0 + run)
                                    \L (u-fn prev 0 - run)
                                    \U (u-fn prev 1 + run)
                                    \D (u-fn prev 1 - run)
                                    prev)))))
        points #(disj (set (reduce r-fn [[0 0]] %)) [0 0])]
    (->> (clojure.set/intersection (points (first wires)) (points (last wires)))
         (map #(+ (Math/abs (first %)) (Math/abs (last %))))
         sort
         first)))

(defn d3-2 [wires]
  (let [parse #(vector (first %) (Long. (apply str (rest %))))
        u-fn (fn [p i f r] (for [x (range 1 (inc r))]
                            (update (update-in p [0 i] #(f % x)) 1 #(+ % x)) ))
        r-fn (fn [acc x]
               (let [[d run] (parse x) prev (last acc)]
                 (vec (concat acc (condp = d
                                    \R (u-fn prev 0 + run)
                                    \L (u-fn prev 0 - run)
                                    \U (u-fn prev 1 + run)
                                    \D (u-fn prev 1 - run)
                                    prev)))))
        points
        (fn [wire]
          (dissoc
           (reduce #(update %1 (first %2)
                            (fn [x] (if (nil? x) (last %2) (min x (last %2)))))
                   {}
                   (reduce r-fn [[[0 0] 0]] wire))
           [0 0]))
        p1s (points (first wires))
        p2s (points (last wires))
        q1s (select-keys p1s (keys p2s))
        q2s (select-keys p2s (keys p1s))]
    (first (sort (vals (merge-with + q1s q2s))))))
