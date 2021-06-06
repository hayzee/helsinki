(ns repl)

(require '[clojure.repl :refer [doc source]])

(source vector)

(vector 1 2 3 4)

(vec '(1 2 3 4))

(apply vector '(1 2 3 4))

;; Probably shouldn't do this.
;; I think what happens is the execution of piff will cause the definition (globally within ns of waff).
;; (defn piff[]
;;   (defn waff[]
;;     2)
;;   1)

(defn my-frequencies-helper [freqs a-seq]
  (let [addone (fn[x]
                   (assoc freqs x (if (contains? freqs x)
                                    (inc (freqs x))
                                    1)))]
    (if (empty? a-seq)
      freqs
      (my-frequencies-helper (addone (first a-seq)) (rest a-seq)))))

(defn my-frequencies [a-seq]
 (my-frequencies-helper {} a-seq))

(+ 99 1)

(my-frequencies []) ;=> {}
(my-frequencies [:a "moi" :a "moi" "moi" :a 1]) ;=> {:a 3, "moi" 3, 1 1}


(defn rev [s]
  (if (< (count s) 2)
    s
    (concat (rev (rest s)) (list (first s)))))


(defn perm [s]
  (if (< (count s) 2)
    (do
      (println s)
      s)
    (let [thiseval (concat (perm (rest s)) (list (first s)))]
;      (println thiseval)
      thiseval
      )))


(defn pow [n x]
  (apply * (take x (repeat n))))


(defn permutations [colls]
  (if (= 1 (count colls))
    (list colls)
    (for [head colls
          tail (permutations (disj (set colls) head))]
      (cons head tail))))


;(count (permutations [1 2 3 4 5 6 7 8 9 10]))


(map vector '(:x :y) [:a :b :c])


(for [x '(1 2 3) y '(5 6 7)] [x y])

(defn single? [s]
  (and (not (empty? s)) (empty? (rest s))))

(defn pair? [s]
  (single? (rest s)))


(defn permute[a-seq]
  (if (empty? a-seq)
    '()
    (if (single? a-seq)
      (seq)
      (if (pair? a-seq)
        (let [ [f s] a-seq]
          (list (seq a-seq) (list s f)))
        (concat (map #(cons (first a-seq) %1) (permute (rest a-seq))) (map #(concat  %1 (list (first a-seq))) (permute (rest a-seq))))
        ))))


(permute [1 2 3])

(list 1 2)

(single? #{:a})


(let [[x y] '(1 2 3 4 5)] y)

(defn rotate-left [s]
  "Rotate sequence s left by 1 stop"
  (concat (rest s) (list (first s))))

;; (defn rots [accum s n]
;;   "all rotations of sequence s"
;;   (if (= n (count s))
;;     s
;;     (cons s (rost ))))

;; (defn copy [s]
;;   (if (empty? s)
;;     '()
;;     (cons (first s) (copy (rest s)))))


(defn rots-n [s n]
  "Rotate sequence s left by n stops"
  (if (< n 1)
    '()
    (cons s (rots-n (rotate-left s) (dec n)))))

(rots-n '(1 2 3) 2)


(defn rots-all [s]
  (rots-n s (count s)))


(rots-all '(1 2 3 4 5 6))


(defn permute[a-seq]
  (if (empty? a-seq)
    '()
    (if (single? a-seq)
      (seq)
      (if (pair? a-seq)
;        (list (seq a-seq) (rotate-left a-seq))
        (rots-all a-seq)
        (reduce (fn [accum elem] (concat accum (map (fn [x] (cons (first elem) x)) (permute (rest elem))))) '() (rots-all a-seq))
        ))))

;; (time (doall (permute '(1 2 3 4 5 6 7 8 9 10))))

;; (time (doall (permutations '(1 2 3 4 5 6 7 8 9 10))))


(cons :a '(:b))


(map #(list % %) (repeat 3 '(1 2 3)))


(reduce #(concat % (map (fn[x] (cons (first %2) (rest %2))) %2)) '() (repeat 3 '(1 2 3)))

(defn mons [s]
  (if (empty? s)
    s
    (concat (list (take 2 s)) (mons (rest(rest s))))))

(mons [1 2 -1 2 5 6])


(defn monner [s]
  (reduce (fn [c e] (conj c e)) (vector (first s)) s))


(defn offsetmap [s]
  (map #(identity {:num %1 :dir (if (>= %1 %2) 1 -1)}) s (cons (first s) s)))


(defn mons[s]
  (let [monner (fn [s] (map #(identity {:num %1 :dir (if (>= %1 %2) :up :down)}) s (cons (first s) s)))]
    (monner s)
    ))

;    (split-with #(= (:dir (first monner)) (:dir %1)) monner)))



(take-while #(= (:dir %1) :up) (mons [1 2 3 4 3 2 1 2 34]))


(defn split-mon-helper [s r1 r2 l d]     ; (s)equence (r)est (l)ast (d)irection
  (if (empty? s)
    (list r1 r2)
    (cons (first s))))

(defn split-mon [s]
  (split-mon-helper s (list) (list) (first s) :up))



;Part1
(def p1 (partition-all 2 1 [1 2 3 4]))
p1

;Part2
(def p2 (map (fn [[x y]] [x y]) p1))
p2

;Part3
(take-while (fn [[x y]] (<= x (if (nil? y) x y))) p1)

(as->
  [1 2 3 4 5] data
  (map #(identity [%1 %2]) data (rest data))
;;   (take-while
;;      (if (>= (nth (first data) 0) (nth (first data) 1))
;;       (fn [[x y]] (>= y x))
;;       (fn [[x y]] (> x y)))
;;     data)
)


(def s (list 7 8 9 4 5 6 9 8 7))

s

(defn offset-seq [s]
  (partition 2 1 s (cons (first s) s)))

(offset-seq s)
