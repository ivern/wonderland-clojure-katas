(ns tiny-maze.solver
  (:import (clojure.lang PersistentQueue)))

(defn done? [maze]
  (not-any? #{:S :E} (flatten maze)))

(defn index [pred coll]
  (first (keep-indexed #(when (pred %2) %1) coll)))

(defn manhattan [r1 c1 r2 c2]
  (let [abs (fn [n] (if (neg? n) (- n) n))]
    (+ (abs (- r1 r2)) (abs (- c1 c2)))))

(defn moves [maze]
  (let [start? (fn [x] (= :S x))
        start-row (index #(some start? %) maze)
        start-col (index start? (nth maze start-row))]
    (for [row (range (count maze))
          col (range (count (first maze)))
          :when (and (#{0 :E} (nth (nth maze row) col))
                     (= 1 (manhattan start-row start-col row col)))
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
