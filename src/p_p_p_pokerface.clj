(ns p-p-p-pokerface)

(defn rank [[fst _]]
  (let [char-to-rank {\T 10,
                      \J 11,
                      \Q 12,
                      \K 13,
                      \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (char-to-rank fst))))

(defn suit [[_ snd]]
  (str snd))

(defn rank-counts [hand]
  (vals (frequencies (map rank hand))))

(defn suit-counts [hand]
  (vals (frequencies (map suit hand))))

(defn has-count? [k xs]
  (boolean
   (some (fn [count] (= count k)) xs)))

(defn pair? [hand]
  (has-count? 2 (rank-counts hand)))

(defn three-of-a-kind? [hand]
  (has-count? 3 (rank-counts hand)))

(defn four-of-a-kind? [hand]
  (has-count? 4 (rank-counts hand)))

(defn flush? [hand]
  (has-count? 5 (suit-counts hand)))

(defn full-house? [hand]
  (and (pair? hand) (three-of-a-kind? hand)))

(defn two-pairs? [hand]
  (let [rks (rank-counts hand)]
    (or (has-count? 4 rks)
        (= 2 (count (filter (fn [count] (= count 2)) rks))))))

(defn straight-ranks? [ranks]
  (= (sort ranks) (range (apply min ranks) (+ 1 (apply max ranks)))))

(defn straight? [hand]
  (let [ranks (map rank hand)]
    (if (= (apply max ranks) 14)
      (or (straight-ranks? (replace {14 1} ranks))
          (straight-ranks? ranks))
      (straight-ranks? ranks))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0))
