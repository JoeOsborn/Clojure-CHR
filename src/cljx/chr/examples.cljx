(ns chr.examples
  (#+clj :use #+cljs :use-macros [chr.macros :only (fresh rule chrfn)])
  (:use [chr :only (awake unwrap)]))

(def leq-rules (fresh [x y z a b eq eq1 eq2 c d]
                       [{:name :Reflexivity
                         :head [[:- [:leq d d]]]}

                        {:name :Antisymmetry
                         :head [[:- [:leq x y]]
                                [:- [:leq y x]]]
                         :bodyfn (chrfn [_ x y] (if (< (hash x) (hash y))
                                                  [[:equivclass x y]]
                                                  [[:equivclass y x]]))}

                        ;"Herbrand equality:"
                        {:name :Eq-rewrite1
                         :head [[:- [:leq x b]]
                                [:+ [:equivclass eq x]]]
                         :body [[:leq eq b]]}
                        {:name :Eq-rewrite2
                         :head [[:- [:leq b x]]
                                [:+ [:equivclass eq x]]]
                         :body [[:leq b eq]]}
                        {:name :Eq-reflexivity
                         :head [[:- [:equivclass d d]]]}
                        {:name :Eq-transitivity
                         :head [[:- [:equivclass y x]]
                                [:+ [:equivclass eq y]]]
                         :body [[:equivclass eq x]]}
                        {:name :Eq-simplification
                         :head [[:- [:equivclass eq1 x]]
                                [:- [:equivclass eq2 x]]]
                         :bodyfn (chrfn [_ x eq1 eq2] [[:equivclass (if (< (hash eq1) (hash eq2)) eq1 eq2) x]])}

                        {:name :Transitivity
                         :head [[:+ [:leq x y]]
                                [:+ [:leq y z]]]
                         :body [[:leq x z]]}]))

(defn solve-leq-chain
  "all variables equal on x1 <= x2 <= ... xn <= x1"
  [length]
  (awake leq-rules (map (fn [[l u]] [:leq (list :v l) (list :v u)])
                        (conj (map vector (range (dec length)) (drop 1 (range)))
                              [(dec length) 0]))))


(def gcd-rules (fresh [n m]
                       [{:head [[:- [:gcd 0]]]}
                        {:head [[:+ [:gcd n]]
                                [:- [:gcd m]]]
                         :guards [(chrfn [_ m n] (>= m n))]
                         :bodyfn (chrfn [_ m n] [[:gcd (- m n)]])}]))

(defn find-gcd [n1 n2]
  (unwrap (awake gcd-rules [[:gcd n1] [:gcd n2]])))
