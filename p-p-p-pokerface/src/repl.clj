(defn suit [card]
  (let [ [_ suit] card]
  (str suit)))


(def ranks {\1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \J 10 \Q 11 \K 12 \A 13})


(defn rank [card]
  (let [ [rank _] card]
  ({\2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14} rank)))


(suit "2H")

(rank "AH")


(suit "2H") ;=> "H"
(suit "2D") ;=> "D"
(suit "2C") ;=> "C"
(suit "3S") ;=> "S"


(apply max (vals (frequencies (clojure.string/split "the cat sat on the mat, it was a good cat that sat on the mat" #" "))))



(def high-seven                   ["2H" "3S" "4C" "5C" "7D"])
(def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
(def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
(def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
(def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
(def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
(def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
(def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
(def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
(def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
(def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
(def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])


(map rank high-seven)

(= (count "this is a sentence") (count (distinct "this is a sentence")))

(defn repeats? [s]
  (not= (count s) (count (distinct s))))

(defn pair? [hand]
    (not (apply distinct? (map rank hand))))

(pair? pair-hand)  ;=> true
(pair? high-seven) ;=> false

(repeats? [1 2 3 4 5])



(defn three-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 3))


(defn four-of-a-kind? [hand]
  (contains? (set (vals (frequencies (map rank hand)))) 4))


(three-of-a-kind? two-pairs-hand)       ;=> false
(three-of-a-kind? three-of-a-kind-hand) ;=> true

(four-of-a-kind? two-pairs-hand)      ;=> false
(four-of-a-kind? four-of-a-kind-hand) ;=> true


(apply distinct? '[1 2 3 4])


(defn straight-flush? [hand]
  (let [hand-ranks (map rank hand)]
  (and (not (pair? hand))
        (= 5 (- (apply max hand-ranks) (apply max hand-ranks))))))


(defn flush? [hand]
  (= 1 (count (set (map suit hand)))))


(flush? pair-hand)  ;=> false
(flush? straight-flush-hand) ;=> true


(apply max (map rank flush-hand))

(apply min (map rank flush-hand))


(defn full-house? [hand]
  (= #{3 2} (set (vals (frequencies (map rank hand))))))


(full-house? three-of-a-kind-hand) ;=> false
(full-house? full-house-hand)      ;=> true




(defn two-pairs? [hand]
  (let [card-ranks               (map rank hand)
        card-rank-freqs          (frequencies card-ranks)
        card-rank-freq-vals      (vals card-rank-freqs)
        card-rank-freq-val-freqs (frequencies card-rank-freq-vals)]
    (or (= 1 (get card-rank-freq-val-freqs 4 ))
        (= 2 (get card-rank-freq-val-freqs 2 )))))

(two-pairs? two-pairs-hand)      ;=> true
(two-pairs? pair-hand)           ;=> false
(two-pairs? four-of-a-kind-hand) ;=> true


(contains? (set "kjhlkjh") \k)


(defn straight? [hand]
  (let [all-ranks (map rank hand)
        max-card (apply max all-ranks)
        min-card (apply min all-ranks)
        all-ranks-lo (map #(if (= 14 (rank %1)) 1 (rank %1)) hand)
        max-card-lo (apply max all-ranks-lo)
        min-card-lo (apply min all-ranks-lo)]
    (and
      (or (= 4 (- max-card min-card))
          (= 4 (- max-card-lo min-card-lo)))
      (= 5 (count (distinct all-ranks))))))


(straight? two-pairs-hand)             ;=> false
(straight? straight-hand)              ;=> true
(straight? low-ace-straight-hand)      ;=> true
(straight? ["2H" "2D" "3H" "4H" "5H"]) ;=> false
(straight? high-ace-straight-hand)     ;=> true


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))



(defn value [hand]
  (cond (straight-flush? hand)  8
        (four-of-a-kind? hand)  7
        (full-house? hand)      6
        (flush? hand)           5
        (straight? hand)        4
        (three-of-a-kind? hand) 3
        (two-pairs? hand)       2
        (pair? hand)            1
        :else 0))


(value high-seven)           ;=> 0
(value pair-hand)            ;=> 1
(value two-pairs-hand)       ;=> 2
(value three-of-a-kind-hand) ;=> 3
(value straight-hand)        ;=> 4
(value flush-hand)           ;=> 5
(value full-house-hand)      ;=> 6
(value four-of-a-kind-hand)  ;=> 7
(value straight-flush-hand)  ;=> 8
