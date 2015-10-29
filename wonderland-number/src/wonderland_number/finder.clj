(ns wonderland-number.finder)

(defn check
  ([n] (and (check n 2) (check n 3) (check n 4) (check n 5) (check n 6)))
  ([n m] (= (set (str n)) (set (str (* n m))))))

(defn wonderland-number []
  (->> (range 100000 1000000)
       (filter check)
       (first)))
