(ns looping-is-recursion)


;; Ex 1
;; Write the function (power n k) that computes the mathematical expression n to power of k.

(defn power [base exp]
  (let [power-accum (fn [accum base exp]
                      (if (zero? exp)
                        accum
                        (recur (* base accum) base (dec exp))))]
    (power-accum 1 base exp)))  ;=> 4


;; Ex 2
;; Compute the last element of a sequence.

(defn last-element [a-seq]
  (cond (empty? a-seq) nil
        (empty? (rest a-seq)) (first a-seq)
        :else (recur (rest a-seq))))


;; Ex 3
;; Write the function (seq= a-seq b-seq) that compares two sequences for equality.

(defn seq= [seq1 seq2]
  (cond (and (empty? seq1) (empty? seq2)) true
        (or (empty? seq1) (empty? seq2)) false
        (not= (first seq1) (first seq2)) false
        :else (recur (rest seq1) (rest seq2))))


;; Ex 4
;; Implement the function (find-first-index [predicate seq]) that returns the first index in seq for which predicate returns true, or nil if no such index exists.

(defn find-first-index [pred a-seq]
  (loop [idx   0
         pred  pred
         a-seq a-seq]
    (cond
      (empty? a-seq) nil
      (pred (first a-seq)) idx
      :else (recur (inc idx) pred (rest a-seq))
      )))


;; Ex 5
;; Implement the function (avg a-seq) that computes the average of a sequence.

(defn avg [a-seq]
  (loop [accum 0
         cnt   0
         a-seq a-seq]
    (if (empty? a-seq)
      (/ accum cnt)
      (recur (+ accum (first a-seq)) (inc cnt) (rest a-seq)))))


;; Ex 6
;; Write the function (parity a-seq) that takes in a sequence and returns a set of those elements that occur an odd number of times in the sequence.

(defn parity [a-seq]
  (let [toggle (fn [s e]
                 (if (contains? s e)
                   (disj s e)
                   (conj s e)))]
  (loop [accum #{}
         a-seq a-seq]
    (if (empty? a-seq)
      accum
      (recur (toggle accum (first a-seq)) (rest a-seq))))))


;; Ex 7
;; Write the function (fast-fibo n) that computes the nth fibonacci number using loop and recur. Do not use recursion.

(defn fast-fibo [n]
  (loop [accum 0
         prev  1
         n     n]
    (if (zero? n)
      accum
      (recur (+ accum prev) accum (dec n)))))


;; Ex 8
;; Write the function (cut-at-repetition a-seq) that takes in a sequence and returns elements from the sequence up to the first repetition.

(defn cut-at-repetition [a-seq]
  (loop [accum '()
         a-seq a-seq
         prior #{}]
    (if (or (empty? a-seq) (contains? prior (first a-seq)))
      (reverse accum)
      (recur (cons (first a-seq) accum) (rest a-seq) (conj prior (first a-seq))))))


