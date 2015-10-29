(ns card-game-war.game)

(def suits [:spade :club :diamond :heart])
(def ranks [2 3 4 5 6 7 8 9 10 :jack :queen :king :ace])
(def cards
  (for [suit suits
        rank ranks]
    [suit rank]))

(defn- indexed-map [v]
  (into {} (map-indexed (fn [idx itm] [itm idx]) v)))

(def suit-val (indexed-map suits))
(def rank-val (indexed-map ranks))

(defn play-round [player1-card player2-card]
  (let [[s1 r1] player1-card
        [s2 r2] player2-card
        v1 (+ (* (rank-val r1) 4) (suit-val s1))
        v2 (+ (* (rank-val r2) 4) (suit-val s2))]
    (compare v1 v2)))

(defn play-game [player1-cards player2-cards]
  (loop [p1 (into (clojure.lang.PersistentQueue/EMPTY) player1-cards)
         p2 (into (clojure.lang.PersistentQueue/EMPTY) player2-cards)]
    (when (and (seq p1) (seq p2))
      (let [c1 (peek p1)
            c2 (peek p2)]
        (if (> (play-round c1 c2) 0)
          (recur (-> (pop p1) (conj c1 c2)) (pop p2))
          (recur (pop p1) (-> (pop p2) (conj c1 c2))))))))
