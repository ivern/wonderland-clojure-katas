(ns fox-goose-bag-of-corn.puzzle
  (:require [clojure.set :as set])
  (:import (clojure.lang PersistentQueue)))

(def start-pos [[#{:fox :goose :corn :you} #{:boat} #{}]])

(defn goal-pos? [pos]
  (= (first pos) [#{} #{:boat} #{:fox :goose :corn :you}]))

(defn options [place]
  (let [stuff (filter #{:fox :goose :corn} place)]
    (cons #{:you} (map #(hash-set :you %) stuff))))

(defn moves [pos]
  (let [[near boat far] (first pos)
        outof set/difference]
    (cond
      (:you near)
      (map #(vector (outof near %) (into boat %) far) (options near))

      (:you boat)
      (let [to-near (map #(vector (into near %) (outof boat %) far) (options boat))
            to-far (map #(vector near (outof boat %) (into far %)) (options boat))]
        (concat to-near to-far))

      (:you far)
      (map #(vector near (into boat %) (outof far %)) (options far)))))

(defn valid? [state]
  (let [boat (nth state 1)]
    (and (< (count boat) 4)
         (not-any? #(and (or (and (:fox %) (:goose %))
                             (and (:goose %) (:corn %)))
                         (not (:you %)))
                   state))))

(defn river-crossing-plan []
  (loop [fringe (cons start-pos PersistentQueue/EMPTY)
         seen (hash-set (first start-pos))]
    (when-let [next-pos (first fringe)]
      (if (goal-pos? next-pos)
        (vec (reverse (map #(vec (map vec %)) next-pos)))
        (let [valid-moves (filter #(and (not (contains? seen %)) (valid? %))
                                  (moves next-pos))]
          (recur (into (rest fringe) (map #(cons % next-pos) valid-moves))
                 (into seen valid-moves)))))))
