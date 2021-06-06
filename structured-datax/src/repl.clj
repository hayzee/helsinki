(+ 1 2)


(apply + [1 2 3 4])


(concat [1 2 3 4 5] [:a :b :c :d :e :f])


(inc 1)


(let
   [addsquares #(+ (* %1 %1) (* %2 %2))]
(addsquares 4 4))


(let
   [cubit #(* % % %)]
(concat (map cubit [1 2 3 4 5])))


;------------------------------------



(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def cities {:title "The City and the City" :authors [china]})
(def wild-seed {:title "Wild Seed", :authors [octavia]})
(def embassytown {:title "Embassytown", :authors [china]})
(def little-schemer {:title "The Little Schemer"
                     :authors [friedman, felleisen]})



(def books [cities, wild-seed, embassytown, little-schemer])


(defn author-names [book]
  (map :name (:authors book)))

(defn all-author-names [books]
  (#(-> %) (apply concat (map author-names books))))


(all-author-names books)


(defn stars [n]
  (apply str (repeat n "*")))




(defn monotonic? [a-seq]
  (or (apply <= a-seq)
      (apply >= a-seq)))






(defn addit [x y]
  (+ x y))



(reduce addit [1 2 3 4 5])


(contains? #{1 2 3 4} 3)


(disj #{1 2 3 4} 2)



(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


(toggle #{1 2 3} 4)




(defn contains-duplicates? [seq]
  (not= (count seq) (count (set seq))))


(contains-duplicates? [1 1 2 3 -40]) ;=> true
(contains-duplicates? [1 2 3 -40]) ;=> false
(contains-duplicates? [1 2 3 "a" "a"]) ;=> true



(defn old-book->new-book [book]
  (assoc book :authors (set (book :authors))))


(set #{friedman, felleisen})

(old-book->new-book {:title "The Little Schemer"
                     :authors [friedman, felleisen]})
;=> {:title "The Little Schemer" :authors #{friedman, felleisen}}

(old-book->new-book {:title "Wild Seed", :authors [octavia]})
;=> {:title "Wild Seed", :authors #{octavia}}






(def china {:name "China Miéville", :birth-year 1972})
(def octavia {:name "Octavia E. Butler"
              :birth-year 1947
              :death-year 2006})
(def friedman {:name "Daniel Friedman" :birth-year 1944})
(def felleisen {:name "Matthias Felleisen"})

(def cities {:title "The City and the City" :authors #{china}})
(def wild-seed {:title "Wild Seed", :authors #{octavia}})
(def embassytown {:title "Embassytown", :authors #{china}})
(def little-schemer {:title "The Little Schemer"
                     :authors #{friedman, felleisen}})

(def books [cities, wild-seed, embassytown, little-schemer])



(defn has-author? [book author]
  (contains? (book :authors) author))


(has-author? cities china)             ;=> true
(has-author? cities felleisen)         ;=> false
(has-author? little-schemer felleisen) ;=> true
(has-author? little-schemer friedman)  ;=> true
(has-author? little-schemer octavia)   ;=> false



(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(authors [cities, wild-seed])              ;=> #{china, octavia}
(authors [cities, wild-seed, embassytown]) ;=> #{china, octavia}
(authors [little-schemer, cities])         ;=> #{china, friedman, felleisen}



(defn all-author-names [books]
  (set (map :name (authors books))))

(all-author-names books)
;=> #{"Matthias Felleisen" "China Miéville"
;     "Octavia E. Butler" "Daniel Friedman"}
(all-author-names [cities, wild-seed])
;=> #{"China Miéville" "Octavia E. Butler"}
(all-author-names []) ;=> #{}


(defn author->string [author]
  (str (:name author)
       (if (:birth-year author)
         (str " ("
              (:birth-year author)
              " - "
              (:death-year author)
              ")"))))

(author->string friedman)


(defn authors->string [authors]
  (let [getname (fn [author] (str (:name author) " (" (:birth-year author) " - " (:death-year author) ")"))]
  (apply str (interpose ", " (map getname authors) ))))
;  (interpose ", " authors ))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors) )))

(authors->string #{octavia, friedman felleisen})

(use '[clojure.set :only [union]])

(union #{:bish} #{:bash} #{:bosh})


(authors->string (:authors little-schemer))
;=> "Daniel Friedman (1944 - ), Matthias Felleisen"
(authors->string #{octavia})          ;=> "Octavia E. Butler (1947 - 2006)"
(authors->string #{})                 ;=> ""
(authors->string #{octavia, friedman})
;=> "Octavia E. Butler (1947 - 2006), Daniel Friedman (1944 - )"
;   order doesn't matter



(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))))
;  book)



(book->string wild-seed) ;=> "Wild Seed, written by Octavia E. Butler"
(book->string little-schemer)
;=> "The Little Schemer, written by Daniel Friedman (1944 - ), Matthias Felleisen"
;                                   ^-- order doesn't matter




(defn books->string [books]
  (cond
    (= 0 (count books)) "No books."
    :else (apply str (count books)
                     (if (= 1 (count books)) " book. " " books.")
                     (apply str (interpose ". " (map book->string books)))
                     ".")))

;; (defn books->string [books]
;;                      (apply str (interpose ". " (map book->string books))))



(books->string []) ;=> "No books."
(books->string [cities])
;=> "1 book. The City and the City, written by China Miéville (1972 - )."
(books->string [little-schemer, cities, wild-seed])
;=> "3 books. The Little Schemer, written by Daniel Friedman (1944 - ), Matthias Felleisen. The City and the City, written by China Miéville (1972 - ). Wild Seed, written by Octavia E. Butler (1947 - 2006)."



(defn books-by-author [author books]
  (filter #(contains? (%1 :authors) author) books))

(books-by-author china books)   ;=> (cities embassytown)
(books-by-author octavia books) ;=> (wild-seed)


(def authors #{china, felleisen, octavia, friedman})

authors

(defn author-by-name [name authors]
  (first (filter #(= (%1 :name) name) authors)))


(author-by-name "Octavia E. Butler" authors)                ;=> octavia
(author-by-name "Octavia E. Butler" #{felleisen, friedman}) ;=> nil
(author-by-name "China Miéville" authors)                   ;=> china
(author-by-name "Goerge R. R. Martin" authors)              ;=> nil

(= nil nil)




(defn alive? [author]
  (not (contains? author :death-year)))


(defn living-authors [authors]
  (filter #(alive? %1) authors))


(living-authors authors)             ;=> (china, felleisen, friedman)
(living-authors #{octavia})          ;=> ()
(living-authors #{china, felleisen}) ;=> (china, felleisen)


;wild-seed


(def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
(def christopher {:name "Christopher Tolkien" :birth-year 1924})
(def kay {:name "Guy Gavriel Kay" :birth-year 1954})

(def silmarillion {:title "Silmarillion"
                   :authors #{jrrtolkien, christopher, kay}})

(def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
(def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

(def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})

(defn has-a-living-author? [book]
  (not (empty? (living-authors (book :authors)))))


(has-a-living-author? wild-seed)      ;=> false
;(has-a-living-author? silmarillion)   ;=> true
(has-a-living-author? little-schemer) ;=> true
(has-a-living-author? cities)         ;=> true
;(has-a-living-author? deus-irae)      ;=> false



(defn books-by-living-authors [books]
  (filter #(has-a-living-author? %1) books))


(books-by-living-authors books) ;=> (little-schemer cities embassytown)
(books-by-living-authors (concat books [deus-irae, silmarillion]))
;=> (little-schemer cities embassytown silmarillion)
