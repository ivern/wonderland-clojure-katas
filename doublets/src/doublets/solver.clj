(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:import (clojure.lang PersistentQueue)))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn candidate-words [w]
  (let [wl (count w)]
    (filter #(= wl (count %)) words)))

(defn distance [w1 w2]
  (cond
    (empty? w1) 0
    (= (first w1) (first w2)) (distance (rest w1) (rest w2))
    :else (+ 1 (distance (rest w1) (rest w2)))))

(defn adjacent? [w1 w2]
  (and (not= w1 w2)
       (= (count w1) (count w2))
       (= 1 (distance w1 w2))))

(defn doublets [word1 word2]
  (let [candidates (candidate-words word1)]
    (loop [fringe (cons (list word1) PersistentQueue/EMPTY)
           seen (hash-set word1)]
      (if-let [next-path (first fringe)]
        (let [next-word (first next-path)]
          (if (= next-word word2)
            (reverse next-path)
            (let [neighbors (filter #(and (not (contains? seen %))
                                          (adjacent? next-word %))
                                     candidates)]
                 (recur (into (rest fringe) (map #(cons % next-path) neighbors))
                        (into seen neighbors)))))
        []))))
