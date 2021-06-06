(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

; T E S T   D A T A

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

(def solved-board
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 2 1 9 5 3 4 8]
          [1 9 8 3 4 2 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 9]]))

(def invalid-board
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 2 1 9 5 3 4 8]
          [1 9 8 3 4 2 5 6 7]
          [8 5 9 9 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 9]]))

(def worlds-hardest-board ;; according to: http://www.telegraph.co.uk/news/science/science-news/9359579/Worlds-hardest-sudoku-can-you-crack-it.html
  (board [[8 0 0 0 0 0 0 0 0]
          [0 0 3 6 0 0 0 0 0]
          [0 7 0 0 9 0 2 0 0]
          [0 5 0 0 0 7 0 0 0]
          [0 0 0 0 4 5 7 0 0]
          [0 0 0 1 0 0 0 3 0]
          [0 0 1 0 0 0 0 6 8]
          [0 0 8 5 0 0 0 1 0]
          [0 9 0 0 0 0 4 0 0]]))

; S T A R T   H E R E


;Ex. 1

(defn value-at [board coord]
  (let [[row col] coord]
    (get-in board [row col])))

;;  (value-at sudoku-board [0 1]) ;=> 3
;;  (value-at sudoku-board [0 0]) ;=> 5


;Ex. 2

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

;; (has-value? sudoku-board [0 0]) ;=> true
;; (has-value? sudoku-board [0 2]) ;=> false


;Ex. 3

(defn row-values [board coord]
  (let [[row _] coord]
   (into #{} (nth board row))))

;; (row-values sudoku-board [0 2]) ;=> #{0 5 3 7}
;; (row-values sudoku-board [3 2]) ;=> #{0 8 6 3}


;Ex. 4

(defn col-values [board coord]
  (let [[_ col] coord]
    (into #{} (map #(nth % col) board))))

;; (col-values sudoku-board [0 2]) ;=> #{0 8}
;; (col-values sudoku-board [4 8]) ;=> #{3 1 6 0 5 9}


;Ex. 5

(defn coord-pairs [coord-sequence]
  (for [x coord-sequence y coord-sequence] [x y]))

;; (coord-pairs [0 1])   ;=> [[0 0] [0 1]
;;                       ;    [1 0] [1 1]]

;; (coord-pairs [0 1 2]) ;=> [[0 0] [0 1] [0 2]
;;                       ;    [1 0] [1 1] [1 2]
;;                       ;    [2 0] [2 1] [2 2]]


;Ex: 6

; Helpers:
; Use defs here as they are evaluated only once.

(def all-values #{1 2 3 4 5 6 7 8 9})

(def coord->block
  (into {}
    (for [y (range 9) x (range 9)]
      (vector [x y] (+ (* (quot x 3) 3) (quot y 3))))))

(def block->coordlist
  (into {}
    (reduce (fn [dict [coord b]] (assoc dict b (cons coord (get dict b)))) {} coord->block)))

(defn coord->coordlist [coord]
  (-> coord coord->block block->coordlist))

(defn block-values [board coord]
  (set (map #(value-at board %) (coord->coordlist coord))))

;; (block-values sudoku-board [0 2]) ;=> #{0 5 3 6 8 9}
;; (block-values sudoku-board [4 5]) ;=> #{0 6 8 3 2}


;Ex: 7

(defn valid-values-for [board coord]
  (if (not (zero? (value-at board coord)))
    #{}
    (set/difference all-values (set/union (block-values board coord) (row-values board coord) (col-values board coord)))))

;; (valid-values-for sudoku-board [0 0]) ;=> #{}
;; (valid-values-for sudoku-board [0 2]) ;=> #{1 2 4}


;Ex: 8

(defn filled? [board]
  (not (contains? (reduce (fn [s row] (set/union s (set row))) #{} board) 0)))

;; (filled? sudoku-board) ;=> false
;; (filled? solved-board) ;=> true
;; (filled? test-board)


;Ex: 9

(defn rows [board]
  (apply vector (map #(into #{} %) board)))

;; (rows sudoku-board) ;=> [#{5 3 0 7}
;;                     ;    #{6 0 1 9 5}
;;                     ;    #{0 9 8 6}
;;                     ;    #{8 0 6 3}
;;                     ;    #{4 0 8 3 1}
;;                     ;    #{7 0 2 6}
;;                     ;    #{0 6 2 8}
;;                     ;    #{0 4 1 9 5}
;;                     ;    #{0 8 7 9}]

;; (rows solved-board) ;=> [#{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}]

(defn cols [board]
  (apply vector (for [col (range 9)]
    (set (map #(nth % col) board)))))

;; (cols sudoku-board) ;=> [#{5 6 0 8 4 7}
;;                     ;    #{3 0 9 6}
;;                     ;    #{0 8}
;;                     ;    #{0 1 8 4}
;;                     ;    #{7 9 0 6 2 1 8}
;;                     ;    #{0 5 3 9}
;;                     ;    #{0 2}
;;                     ;    #{0 6 8 7}
;;                     ;    #{0 3 1 6 5 9}]

;; (cols solved-board) ;=> [#{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}]


;Ex: 10

(defn blocks [board]
  (apply vector (for [coord block->coordlist]
    (set (map (partial value-at board) (nth coord 1))))))

;; (blocks sudoku-board) ;=> [#{5 3 0 6 9 8}
;;                       ;    #{0 7 1 9 5}
;;                       ;    #{0 6}
;;                       ;    #{8 0 4 7}
;;                       ;    #{0 6 8 3 2}
;;                       ;    #{0 3 1 6}
;;                       ;    #{0 6}
;;                       ;    #{0 4 1 9 8}
;;                       ;    #{2 8 0 5 7 9}]

;; (blocks solved-board) ;=> [#{1 2 3 4 5 6 7 8 9}
;;                       ;    #{1 2 3 4 5 6 7 8 9}
;;                       ;    #{1 2 3 4 5 6 7 8 9}
;;                       ;    #{1 2 3 4 5 6 7 8 9}
;;                       ;    #{1 2 3 4 5 6 7 8 9}
;;                       ;    #{1 2 3 4 5 6 7 8 9}
;;                       ;    #{1 2 3 4 5 6 7 8 9}
;;                       ;    #{1 2 3 4 5 6 7 8 9}
;;                       ;    #{1 2 3 4 5 6 7 8 9}])


;Ex: 11

(defn valid-rows? [board]
  (every? #(= all-values (set %)) (rows board)))

;;  (valid-rows? solved-board)  ;=> truthy
;;  (valid-rows? invalid-board) ;=> falsey


(defn valid-cols? [board]
  (every? #(= all-values (set %)) (cols board)))

;; (valid-cols? solved-board)  ;=> truthy
;; (valid-cols? invalid-board) ;=> falsey

(defn valid-blocks? [board]
  (every? #(= all-values (set %)) (blocks board)))

;; (valid-blocks? solved-board)  ;=> truthy
;; (valid-blocks? invalid-board) ;=> falsey


; Ex: 12

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

;; (valid-blocks? solved-board)  ;=> truthy
;; (valid-blocks? invalid-board) ;=> falsey


; Ex: 13

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

;; (def before-change
;;   (board [[5 3 0 0 7 0 0 0 0]
;;           [6 0 0 1 9 5 0 0 0]
;;           [0 9 8 0 0 0 0 6 0]
;;           [8 0 0 0 6 0 0 0 3]
;;           [4 0 0 8 0 3 0 0 1]
;;           [7 0 0 0 2 0 0 0 6]
;;           [0 6 0 0 0 0 2 8 0]
;;           [0 0 0 4 1 9 0 0 5]
;;           [0 0 0 0 8 0 0 7 9]]))

;; (def after-change
;;   (board [[5 3 0 0 7 0 0 0 0]
;;           [6 0 0 1 9 5 0 0 0]
;;           [0 4 8 0 0 0 0 6 0]
;;           [8 0 0 0 6 0 0 0 3]
;;           [4 0 0 8 0 3 0 0 1]
;;           [7 0 0 0 2 0 0 0 6]
;;           [0 6 0 0 0 0 2 8 0]
;;           [0 0 0 4 1 9 0 0 5]
;;           [0 0 0 0 8 0 0 7 9]]))

;; ( = (set-value-at before-change [2 1] 4) after-change)


; Ex: 14

(defn find-empty-point [board]
  (reduce
    (fn [result coord]
      (if (zero? (get-in board coord))
        (reduced coord)))
    nil
    (for [row (range 9) col (range 9)] [row col])))

;; (find-empty-point sudoku-board)
;; (find-empty-point solved-board)


; Ex: 15

(defn get-move-list
  "Generates a bunch of new board positions"
  [board]
  (let [next-coord (find-empty-point board)]
    (if next-coord
      (map #(set-value-at board next-coord %) (valid-values-for board next-coord))
      (list))))


(defn solve [board]
  (loop [board       board
         move-list   (list)]
    (if (valid-solution? board)
      board
      (let [new-move-list (lazy-cat (get-move-list board) move-list)]
        (if (empty? new-move-list)
          :unsolvable
          (recur (first new-move-list) (rest new-move-list))
        )))))

(solve test-board)
