(ns alphabet-cipher.coder)

(defn- gen-row [n m]
  (-> (+ n m) (mod 26) (+ (int \a)) (char)))

(defn- look-up [predicate [k m]]
  (let [col (map (comp char #(+ (int \a) (mod % 26))) (range))
        row (map (comp char #(+ (int \a) (quot % 26))) (range))
        enc (flatten (map #(map (partial gen-row %) (range 26)) (range 26)))
        zip (map vector col row enc)
        p (partial predicate k m)]
    (some p zip)))

(defn encode [keyword message]
  (let [key (flatten (repeat (seq keyword)))
        zip (map vector key (seq message))
        f (fn [k m [row col enc]]
            (if (and (= k col) (= m row))
              enc
              nil))]
    (apply str (map (partial look-up f) zip))))

(defn decode [keyword message]
  (let [key (flatten (repeat (seq keyword)))
        zip (map vector key (seq message))
        f (fn [k m [row col enc]]
            (if (and (= k col) (= m enc))
              row
              nil))]
    (apply str (map (partial look-up f) zip))))

(defn- grow [keyword n]
  (let [len (count keyword)]
    (->> (subs keyword 0 n) (repeat len) (apply str) (take len) (apply str))))

(defn- shrink [keyword]
  (loop [n 1]
    (if (= keyword (grow keyword n))
      (subs keyword 0 n)
      (recur (inc n)))))

(defn decipher [cipher message]
  (let [zip (map vector (seq cipher) (seq message))
        f (fn [k m [row col enc]]
            (if (and (= k enc) (= m row))
              col
              nil))]
    (shrink (apply str (map (partial look-up f) zip)))))
