(cons 1 '(1 2 3 4 5))

(cons 1 [1 2 3 4 5])

(cons 1 {1 11 2 22 3 33 4 44 5 55})

(reverse (rest (reduce #(conj (conj %1 %2) 0 ) '() '(1 2 3 4 5))))

(apply str (reverse (rest (reduce #(conj (conj %1 %2) 0 ) '() "hahahaha"))))

;(reduce concat '() '(1 2 3 4 5))

(reduce #(concat %1 (list 0 %2)) '(1) '(2 3 4 5))


(seq (hash-map 1 2 3 4))



(reduce conj '() '(1 2 3 4))


(if (some empty? (map rest (list [1 2 3] [1 2 3] [1 2 3])))
  "yes"
  "no")
