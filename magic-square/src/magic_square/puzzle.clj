(ns magic-square.puzzle
  (:require [clojure.math.combinatorics :as combo]))

(def values [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

(defn ->matrix [v]
  (->> v (partition 3) (map vec) (into [])))

(defn magic? [values]
  (let [rows (->matrix values)
        cols (apply map vector rows)
        d1sum (+ (values 0) (values 4) (values 8))
        d2sum (+ (values 2) (values 4) (values 6))
        rsum (apply vector (map #(reduce + %) rows))
        csum (apply vector (map #(reduce + %) cols))]
    (->> (vector d1sum)
         (concat rsum csum)
         (every? #(= % d2sum)))))

(defn magic-square [values]
  (->> (combo/permutations values)
       (filter magic?)
       (first)
       (->matrix)))
