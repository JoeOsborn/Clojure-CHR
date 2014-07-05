(ns chr.macros
	(:use [chr.types])
	(:require [clojure.walk :as walk]
	          [clojure.set :as set]))

(defn let-binder*
  "convert a normal let binding into a tuple holding a function that returns a binding map.
   a let binder is a [required-lvars, provided-lvars, (fn [store required] ...)] tuple"
  [name argform new-vars bindform expr]
  (let [new-var-aliases (map #(gensym (str % "-")) new-vars)]
    `[~(vec (drop 1 argform))
      ~(vec new-vars)
      (fn ~name ~argform
        (let [~@(mapcat (fn [alias v] [alias v]) new-var-aliases new-vars)
              ~bindform ~expr]
          (hash-map ~@(interleave new-var-aliases new-vars))))]))

(defmacro fresh
  [varlist & body]
  `(let [~@(mapcat (fn [v] [v `(variable (quote ~v))]) varlist)]
     ~@body))

(defmacro gather-matches
  "lvar introduces lvar(s) to match in the pattern.
   Can be a single lvar, returning a seq of values for matched lvar,
   or a vec of lvars in which case will return a seq of tuples
   representing matched values."
  [lvar & store-guards-pattern]
  (if (vector? lvar)
    `(fresh ~lvar (map (fn [m#] (vec (map (fn [v#] (get m# v#)) ~lvar)))
                        (find-matches ~@store-guards-pattern)))
    `(fresh [~lvar] (map (fn [m#] (get m# ~lvar))
                          (find-matches ~@store-guards-pattern)))))

(defmacro chrfn
  "chrfns must be of the form
   (chrfn name? [store arg1 ...argn]) where store is bound to
   the current state of the constraint store.
   becomes [required-lvars, (fn [store required] ...)] tuple"
  {:forms '[(chrfn name? [store params*] exprs*)]}
  [args-or-name & rst]
  (if (vector? args-or-name)
    `[~(vec (drop 1 args-or-name)) (fn ~args-or-name ~@rst)]
    (let [[args & body] rst]
      `[~(vec (drop 1 args)) (fn ~args-or-name ~args ~@body)])))

(defmacro rule
  ([head]
     `(rule ~(symbol (str "rule-" (mod (hash head) 10000))) ~head))
  ([head body]
     (if (vector? head)
       `(rule ~(symbol (str "rule-" (mod (hash [head body]) 10000))) ~head ~body)
       `(rule ~head ~body [])))
  ([name head body]
     (let [occurrences (vec (map (fn [[op pat]] [op (walk/postwalk
                                                     (fn [t] (get {'& ::&
                                                                   '_ (variable (gensym "_"))} t t))
                                                     pat)])
                                 (filter (fn [[op pat]] (#{:- :+} op)) (partition 2 head))))
           guards (vec (map second (filter (fn [[op pat]] (= :when  op)) (partition 2 head))))
           let-bindings (vec (mapcat #(partition 2 (second %))
                                     (filter (fn [[op pat]] (= :let  op))
                                             (partition 2 head))))
           store-alias (or (last (map second (filter (fn [[op pat]] (= :store op)) (partition 2 head))))
                           'store)
           tabled? (or (:tabled (meta name)) (:tabled (meta head)) (:tabled (meta body)))
           variables (into #{} (for [pattern (concat (map second occurrences)
                                                     (map first let-bindings))
                                     term ((fn gather [f] (cond (symbol? f) #{f}
                                                                (coll? f) (apply set/union (map gather f))
                                                                :else nil)) pattern)] term))
           collect-vars (fn [form]
                          ((fn gather [f] (cond (variables f) #{f}
                                                (coll? f) (apply set/union (map gather f))
                                                :else nil)) form))]
       `(fresh ~(vec variables)
                {:name (quote ~name)
                 :head ~occurrences
                 :guards [~@(map (fn [g] `(chrfn ~name [~store-alias ~@(collect-vars g)] ~g)) guards)]
                 :let-binders [~@(map (fn [[bindform expr]]
                                        (let-binder* name
                                                     (vec (concat [store-alias] (collect-vars expr)))
                                                     (collect-vars bindform)
                                                     bindform expr))
                                      let-bindings)]
                 :tabled ~tabled?
                 :bodyfn (chrfn ~name [~store-alias ~@(collect-vars body)] ~body)}))))
