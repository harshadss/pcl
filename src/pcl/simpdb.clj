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



;; saving to file , pr in clojure implements readable serialization
;; we store current state of the database
(defn save-db [filename]
  (let [current-db (deref db)]
    (with-open [w (clojure.java.io/writer filename)]
      (binding [*out* w]
        (pr current-db)))))
