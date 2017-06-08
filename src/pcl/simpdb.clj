(ns pcl.simpdb
  "Implements code from Chapter 3 of practical common lisp"
  (:gen-class))

;; code from chapter 3 of practical common lisp

;; pcl book uses plist in cl for storing single record/cd
;; hashmap serves us equally well for storing property and value mapping
(hash-map :a 1 :b 2 :c 3)

;; getf is get here
(get (hash-map :a 1 :b 2 :c 3) :a)

;; constructor for building single record
(defn make-cd
  [title artist rating ripped]
  (hash-map :title title :artist artist :rating rating :ripped ripped))

;; example
(make-cd "Roses" "Kathy Mattea" 7 true)

;; storing database in a list
;; current state of database is an atom with database list
(def db (atom '()))

(defn add-record
  [cd]
  (swap! db conj cd))

;; test on few examples
(add-record (make-cd "Roses" "Kathy Mattea" 7 true))
(add-record (make-cd "Fly" "Dixie Chicks" 8 true))
(add-record (make-cd "Home" "Dixie Chicks" 9 true))

;; print db using pprint instead of elaborate string formatting
(defn dump-db
  []
  (clojure.pprint/pprint (deref db)))

;; saving to file , pr in clojure implements readable serialization
;; we store current state of the database
(defn save-db [filename]
  (let [current-db (deref db)]
    (with-open [w (clojure.java.io/writer filename)]
      (binding [*out* w]
        (pr current-db)))))

;; generate the right filter condition
;; note that for parameter not passed, we always return true so if works out.
;; where function returns a predicate, where is used with select
(defn where
  [& {:keys [artist rating title]
      :or [artist false rating false title false]}]
  (fn [cd]
    (and
     (if artist (= artist (get cd :artist)) true)
     (if rating (= rating (get cd :rating)) true)
     (if title (= title (get cd :title)) true))))

(defn select [wherefunc]
  (filter wherefunc (deref db)))

(select (where :rating 8))
;; macro part

(def ^:dynamic row nil)

(defn make-comparison-expr
  [[field value]]
  (list '= value (list 'get row field)))

(defn make-comparison-expr
  [[field value]]
  `(= (get row ~field) ~value))

(binding [row {:rating 8 :artist 9}]
  (make-comparison-expr '(:rating 8)))

;; for multiple pairs passed to the where form, iterate over the loop
(defn make-comparison-expr-list
  [all-field-pairs]
  (let [pairs (partition 2 all-field-pairs)]
    (for [x (map #(make-comparison-expr %) pairs)]
      x)))

(binding [row {:rating 8 :artist 9}]
  (make-comparison-expr-list '(:artist "Dixie Chicks" :rating 8)))

(defmacro where [clauses]
  `(fn [cd#]
     (binding [row cd#]
      (every? eval (make-comparison-expr-list ~clauses)))))

(defn select [wherefunc]
  (filter wherefunc (deref db)))

(select (where '(:title "Fly")))

;; for updating, you move over the list, update it and then reset the value of db

(defn updatedb
  [select-fn update-map]
  (let [update-seq (flatten (seq update-map))]
    (reset! db
      (map
       #(if (select-fn %)
          (apply assoc % update-seq)
          %) (deref db)))))

(updatedb (where '(:artist "Dixie Chicks")) {:rating 12})

