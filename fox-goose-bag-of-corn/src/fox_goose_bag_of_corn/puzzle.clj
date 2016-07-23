(ns fox-goose-bag-of-corn.puzzle
  (:require [clojure.set :as set])
  (:import (clojure.lang PersistentQueue)))

(def start-pos [[#{:fox :goose :corn :you} #{:boat} #{}]])
(def end-state [#{} #{:boat} #{:fox :goose :corn :you}])

(defn options [place]
  (let [stuff (filter #(% #{:fox :goose :corn}) place)]
    (cons #{:you} (map #(hash-set :you %) stuff))))

(defn moves [state]
  (let [near (nth state 0)
        boat (nth state 1)
        far (nth state 2)
        outof set/difference]
    (cond
      (:you near) (map #(vector (outof near %) (into boat %) far) (options near))
      (:you boat) (let [foo (map #(vector (into near %) (outof boat %) far) (options boat))
                        bar (map #(vector near (outof boat %) (into far %)) (options boat))]
                    (vec (into foo bar)))
      (:you far) (map #(vector near (into boat %) (outof far %)) (options far)))))

(defn valid? [state]
  (and (< (count (nth state 1)) 4)
       (not-any? #(and (or (and (:fox %) (:goose %))
                           (and (:goose %) (:corn %)))
                       (not (:you %)))
                 state)))

(defn river-crossing-plan []
  (loop [fringe (cons start-pos PersistentQueue/EMPTY)
         seen (hash-set (first start-pos))]
    (if-let [next-pos (first fringe)]
      (let [next-state (first next-pos)]
        (if (= next-state end-state)
          (vec (reverse (map #(vec (map vec %)) next-pos)))
          (let [valid-moves (filter #(and (not (contains? seen %)) (valid? %))
                                    (moves next-state))]
            (recur (into (rest fringe) (map #(cons % next-pos) valid-moves))
                   (into seen valid-moves)))))
      [])))
