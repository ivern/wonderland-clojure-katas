(ns tiny-maze.solver
  (:import (clojure.lang PersistentQueue)))

(defn done? [maze]
  (not-any? #{:S :E} (flatten maze)))

(defn index [pred coll]
  (first (keep-indexed #(when (pred %2) %1) coll)))

(defn moves [maze]
  (let [eq-s (fn [x] (= :S x))
        abs (fn [n] (if (neg? n) (- n) n))
        start-row (index #(some eq-s %) maze)
        start-col (index eq-s (nth maze start-row))]
    (for [row (range (count maze))
          col (range (count (first maze)))
          :when (and (#{0 :E} (nth (nth maze row) col))
                     (= 1 (+ (abs (- start-row row))
                             (abs (- start-col col)))))
          :let [end? (= :E (nth (nth maze row) col))]]
      (vec (map-indexed #(cond-> %2
                          (= start-row %1) (assoc start-col :x)
                          (= row %1) (assoc col (if end? :x :S)))
                        maze)))))

(defn solve-maze [maze]
  (loop [fringe (cons maze (PersistentQueue/EMPTY))]
    (when-let [next-state (first fringe)]
      (if (done? next-state)
        next-state
        (recur (into fringe (moves next-state)))))))
