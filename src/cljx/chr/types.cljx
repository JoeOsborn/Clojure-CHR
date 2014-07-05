(ns chr.types)

(defrecord Variable [name])
(defn variable? [x]
  (instance? Variable x))

(defn variable
  "No effort expended to make variables hygenic.
   Scope ranges over entire rules (head & body)."
  [x]
  (->Variable x))
