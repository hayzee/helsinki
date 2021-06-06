(ns repl
  (:require [clojure.set :as set]))


(defn all-subsets-of [s]
  (cond (empty? s) #{}
        (empty? (rest s)) #{first s}
        (empty? (-> s rest rest)) #{ #{(first s)} #{(second s)} #{(first s) (second s)} }
        :otherwise (set/union
                      #{(first s)}
                      (all-subsets-of (rest s))
                      (set/union (all-subsets-of (rest s)) #{(first s)})
                    )
        ))

;(all-subsets-of #{1 2 3 4 5 6 7 8 9 10 11 12})






(defn all-subsets-of [s]
  (if (empty? s) #{#{}}
      (set/union (all-subsets-of (rest s)) (map #(set/union #{(first s)} %) (all-subsets-of (rest s))))))

(def all-subsets-of-mz (memoize all-subsets-of))

(def all-subsets-of-mz
  (memoize
    (fn [s]
      (if (empty? s)
        (hash-set (hash-set))
        (set/union (all-subsets-of-mz (rest s)) (map #(conj % (first s)) (all-subsets-of-mz (rest s))))))))


(conj #{} 1)

;(all-subsets-of-mz (apply hash-set (range 17)))




(defn fast-all-subs [s]
  (loop [accum  (hash-set (hash-set))
         s      s]
    (if (empty? s)
      accum
      (recur (set/union accum (set (map #(conj % (first s)) accum))) (rest s)))))


;(fast-all-subs (apply hash-set (range 1)))



(defn fast-all-subs [s]
  (loop [accum  (list (list))
         s      (apply list s)]
    (if (empty? s)
      accum
      (recur (concat accum (set (map #(conj % (first s)) accum))) (rest s)))))


;(map #(into #{} %) (fast-all-subs (range 20)))


;; (->>
;;   (fast-all-subs (range 20))
;;   (map #(into #{} %))
;;   (into #{}))

(def board identity)


(def sudoku-board
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

;; (let [l '(1 2 3 4 5)]
;;   (disj (first l) l))

(reduce
  (fn [result coord]
    (if (zero? (get-in sudoku-board coord))
      (reduced coord)))
  nil
  (for [row (range 9) col (range 9)] [row col]))


(defn permute [s]
  (if (or (empty? s) (empty? (rest s)))
    s
    (for [elem (count s)]
      ())))


(defn permusplit [a-seq]
  (lazy-seq
    (loop [accum (list)
           s1    (list)
           s2    a-seq]
      (if (empty? s2)
        (cons (list s1 s2) accum)
        (recur (cons (list s1 s2) accum) (cons (first s2) s1) (rest s2))))))


(lazy-seq
  (map (fn [e]
         (concat
           (first e)
           (rest (second e))))
       (rest (permusplit '(1 2 3 4 5 6)))))




(permusplit '(1 2 3))


(defn permusplit2 [a-seq]
  (lazy-seq
    (loop [accum (list)
           s1    (list)
           s2    a-seq]
      (if (empty? s2)
        (cons (list s1 s2) accum)
        (recur (cons (list s1 s2) accum) (cons (first s2) s1) (rest s2))))))

(defn rot1 [s]
  (concat (rest s) (list (first s))))

(defn rot-all [s]
  (loop [accum (list)
         n     (count s)
         s      s]
    (if (pos? n)
      (recur (cons (rot1 s) accum) (dec n) (rot1 s))
      accum)))

(rot1 '(1 2 3 4 5))

(rot-all '(1 2 3 4 5))
(first '())

(defn perms [s]
  (if (first s)
    (for [rot (rot-all s)]
      (concat (list (first rot)) (perms (rest rot))))
    (list s)))

(perms '(1 2 3))

(apply list '(1 2 3 4 5))

(def a-seq '(1 2))

(map #(cons (first a-seq) %) (rot-all (rest a-seq)))

(defn perm [s]
  (if (empty? s)
    s
    (for [rot (rot-all s)]
      (cons (first rot) (perm (rest rot))))))



(defn perm [s]
  (if (empty? (rest s))
    (list s)
    (for [rot (rot-all s)]
      (map #(cons (first rot) %) (perm (rest rot)))
      )))


;(perm '(1 2 3 4 5 6 7))

(lazy-seq
  (map (fn [e]
         (concat
           (first e)
           (rest (second e))))
       (rest (permusplit '(1 2 3 4 5 6)))))


;; (head [1 2 3])

;(permusplit '(1 2 3))


;; (defn all-splits [s]
;;   (loop [s1 (list) s2 s]
;;     (if (first s2))))

(defn all-split [s]
  (for [x (range (count s))]
    (let [[split1 split2] (split-at x s)]
      ;{:splt [split1 split2] :removed (first split2) :remain (concat split1 (rest split2))}
      [(first split2) (concat split1 (rest split2))]
      )))

;(concat (rest (second splt)) (first splt) (rest (second splt))))))


;(all-split '(1 2 3 4))

;(for [x (range 0)] x)


(defn remove-element [a-seq idx]
  (let [[h t] (split-at idx a-seq)]
    (concat h (rest t))))

;(remove-element

;; (defn permutate [s]
;;   (if (empty? s)
;;     s
;;     (for [x (range (count s))]
;;       (map #(cons x %) (permutate s without x)))))


(defn split-remove [s]
   (if (rest s)
     (lazy-seq (cons (first s) (rest (rest s))))
     s))


(take 4 (map #(rest %) (partition 5 (cycle '(1 2 3 4)))))

(defn all-rots [s]
  (->>
    (cycle s)
    (partition (inc (count s)))
    (map #(rest %))
    (take (count s))))

;(map #(split-at 1 %) (all-rots [1 2 3 4 5]))


(defn permutate [s]
  (if (empty? (rest s))
    s
    (map #(cons (first %)(permutate (rest %))) (all-rots s))))

(defn perms [s]
  (if (empty? (rest s))
    (list s)
    (for [each-rot (all-rots s)
         each-tail (perms (rest each-rot))]
        (cons (first each-rot) each-tail))))


(perms '(1 2 3))


(defn perms [s]
  (if (empty? (rest s))
    (lazy-seq (list s))
    (for [ars (all-rots s)
          prs (lazy-seq (perms (rest ars)))]
       (cons (first ars) prs))))

;(type (perms (iterate inc 1)))

(perms '(1 2 3 4 5))

;(take 10 (iterate #(cons (first %) %) '(1)))

(def perms-mz (memoize perms))

;(count (perms-mz (range 3)))


(defn permulist
  "Given a list of lists, generate a list of all possible combinations of each of the lists' elements"
  [ss]
  (if (empty? ss)
    (list ss)
    (for [s (first ss)
          pr (permulist (rest ss))]
      (lazy-seq (cons s pr)))))


(permulist '([1 2 3 4 5] [1 2 3 4 5] [1 2 3 4 5] [1 2 3 4 5]))


(filter #(= 5 (count (into #{} %))) (permulist (list (range 1 6) (range 1 6) (range 1 6) (range 1 6) (range 1 6))))


(permulist (list [2] [1 2 3]))
