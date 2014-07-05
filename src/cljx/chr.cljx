(ns chr
  (:use [chr.debug :only (trace reset-bench bench-here nanotime)]
				[chr.types :only (variable? variable)])
  (#+clj :use #+cljs :use-macros
    [chr.macros :only (fresh gather-matches chrfn rule)]
    [chr.debug.macros :only (bench no-bench)])
  (:require [clojure.set :as set]
            [clojure.walk :as walk]))

(def error
  #+clj (fn 
    ([] (throw (Exception.)))
    ([e] (throw (Exception. e)))
    ([e1 e2] (throw (Exception. e1 e2))))
  #+cljs (fn [& err-args] (throw (apply js/Error. err-args))))

(defn rewrite
  [pattern rewrite-map]
  (when (some #(not (instance? clojure.lang.Symbol (:name %))) (keys rewrite-map))
    (trace [:rewrite :error] [pattern rewrite-map]))
  (no-bench
   :rewrite
   (map (fn [t] (get rewrite-map t t)) pattern)))

(defn dissoc-constraint
  [store [term & rst]]
  (trace [:dissoc-constraint]
         ["store:" store
          "term:" term
          "rst:" rst
          "store Type" (type store)])
  (no-bench
   :dissoc-constraint
   (if (= ::& term)
     (dissoc-constraint store (first rst))
     (if (empty? rst)
       (set/difference store #{term})
       (let [b (dissoc-constraint (get store term) rst)]
         (if (empty? b)
           (dissoc store term)
           (assoc store term b)))))))

(defn impose-constraint
  [store constraint]
  (do
    (trace [:impose-constraint]
           ["on store" store "with constraint" constraint])
    (if (= 1 (count constraint))
      (if (= {} store)
        #{(first constraint)}
        (into store constraint))
      (update-in store (drop-last constraint) set/union #{(last constraint)}))))

(defn sort-guards
  "given a collection of variables that will be grounded, sorts into
   [grounded, ungrounded] guards--so you know which are possible to check."
  [guards grounded-variables]
  (no-bench
   :sort-guards
   (let [ground-set (set grounded-variables)
         {ground true unground false}
         , (group-by (fn [[args gfn]] (every? #(or (not (variable? %))
                                                   (ground-set %)) args))
                     guards)]
     [ground unground])))

(defn sort-let-binders
  "given a collection of variables that will be grounded, sorts into
   [grounded-lbs, ungrounded-lbs, newly-grounded-lvars], reflecting the additional lvars
   that will be ground by running these binders"
  [let-binders grounded-variables]
  (let [ground-set (set
                    grounded-variables)
        {ground true unground false}
        , (group-by (fn [[args news gfn]] (every? #(or (not (variable? %))
                                                       (ground-set %)) args))
                    let-binders)
        newly-ground (apply set/union (map (comp set second) ground))]
    (if (empty? (set/difference newly-ground ground-set))
      (do (trace [:sort-let-binders] [[ground unground ground-set]])
          [ground unground ground-set])
      (let [[g ug ngs] (sort-let-binders unground (set/union ground-set newly-ground))]
        [(concat ground g) ug ngs]))))

(defn unwrap
  "Returns the sequence of constraints comprised by a given store.
   Nested stores are not recursively unwrapped.
   assert: (= some-store (reduce impose-constraint {} (unwrap some-store)))"
  [store]
  (if (set? store)
    (map vector store)
    (mapcat (fn [[k v]] (map #(vec (concat [k] %)) (unwrap v))) store)))

(defn satisfies-guards?
  [root-store substs guards]
  (every? (fn [[args gfn]] (trace [:guard-call] ["store:" root-store] (apply gfn root-store (rewrite args substs)))) guards))

(defn let-bind
  "returns the substs map modified by the let-binder"
  [root-store substs let-binders]
  (trace [:let-bind]
         [substs "->" (reduce (fn [s [args _ bfn]] (merge s
                                                          (let [bindings (apply bfn root-store (rewrite args s))]
                                                            (when (some #(not (instance? clojure.lang.Symbol (:name %)))
                                                                        (keys bindings))
                                                              (trace [:let-bind :error] ["roots" root-store  "current subs" s "bindings" bindings "args" args "rewritten to" (rewrite args s) "bfn" bfn])
                                                              (error "Bad type for variable binding. Check the let binder."))
                                                            bindings)))
                              substs
                              let-binders)]))

(defn find-matches*
  "Returns a seq of substitution maps, arity of pattern must be matched."
  ([root-store store substs guards let-binders [term & next-terms]]
     (no-bench
      :find-matches
      (let [term (if (variable? term) (get substs term term) term)]
        (cond
         (vector? term) (let [[grnd-binders ungrnd-binders next-ground] (sort-let-binders let-binders (concat (keys substs) term))
                              [grnd-guards ungrnd-guards] (sort-guards guards next-ground)]
                          (mapcat (fn [[k v]]
                                    (if v (mapcat (fn [submatch]
                                                    (find-matches* root-store v (merge substs submatch) ungrnd-guards ungrnd-binders next-terms))
                                                  (find-matches* root-store k substs grnd-guards grnd-binders term))
                                        (find-matches* root-store k substs grnd-guards grnd-binders term)))
                                  (if (map? store)
                                    (filter (fn [[k v]] (map? k)) store)
                                    (map (fn [s] [s nil]) (filter map? store)))))
         (nil? next-terms) (if (set? store)
                             (if (variable? term)
                               (filter
                                #(satisfies-guards? root-store % guards)
                                (map #(let-bind root-store (assoc substs term %) let-binders) store))
                               (if (contains? store term) [substs] []))
                             [])
         (= ::& term) (let [rest (first next-terms)
                            [grnd-binders _ next-ground] (sort-let-binders let-binders (conj (keys substs) rest))
                            [grnd-guards _] (sort-guards guards next-ground)]
                        (filter
                         #(satisfies-guards? root-store % grnd-guards)
                         (map #(let-bind root-store (assoc substs rest %) grnd-binders) (unwrap store))))
         (set? store) (if (= (first next-terms) ::&)
                        (let [rest (second next-terms)]
                          (if (variable? term)
                            (filter
                             #(satisfies-guards? root-store % guards)
                             (map #(let-bind root-store (assoc substs term % rest []) let-binders) store))
                            (if (contains? store term) [(let-bind root-store (assoc substs rest []) let-binders)] [])))
                        ())
         (variable? term) (if (map? store)
                            (let [[grnd-binders ungrnd-binders next-ground] (sort-let-binders let-binders (conj (keys substs) term))
                                  [grnd-guards ungrnd-guards] (sort-guards guards next-ground)]
                              (mapcat (fn [[k v]]
                                        (let [next-substs (assoc substs term k)]
                                          (if (satisfies-guards? root-store next-substs grnd-guards)
                                            (find-matches* root-store
                                                           v
                                                           (let-bind root-store next-substs grnd-binders)
                                                           ungrnd-guards
                                                           ungrnd-binders
                                                           next-terms)
                                            [])))
                                      store))
                            [])
         :else (find-matches* root-store (get store term) substs guards let-binders next-terms))))))

(defn find-matches
  ([store pattern] (find-matches* store store {} [] [] pattern))
  ([store guards pattern] (find-matches* store store {} guards [] pattern))
  ([store substs guards terms]
     (find-matches* store store substs guards [] terms))
  ([root-store store substs guards let-binders terms]
     (find-matches* root-store store substs guards let-binders terms)))

(defn store-values
  "flat list of every value in a store (not grouped by constraints)"
  [store]
  (if (map? store)
    (concat (keys store) (mapcat store-values (vals store)))
    store))

(defn store?
  [t] (and (map? t)
           (every? coll? (vals t))))

(defn find-matches-recursive
  "descends into nested stores to find matches."
  ([store pattern] (find-matches-recursive store [] pattern))
  ([store guards pattern]
     (trace [:find-matches-recursive] ["store" store "pat" pattern "guards" guards])
     (concat (find-matches store guards pattern)
             (mapcat #(find-matches-recursive % guards pattern)
                     (filter store? (store-values store))))))

(defn partial-apply-chrfns
  "takes a collection of guards or let-binders, and grounds their
   argument templates according to the substitution."
  [guards substs]
  (trace [:partial-apply] [guards "with" substs])
  (map (fn [[args & rst]] (vec (concat [(rewrite args substs)] rst))) guards))

(defn match-head
  "list of all viable [subststitutions, store-after-removal]
   pairs that match this collection of patterns"
  [root-store store substs guards let-binders [pattern & rst]]
  (if pattern
    (let [[grnd-binders ungrnd-binders next-ground]
          , (trace [:mh-letbinders] ["store" store "root-store" root-store "pattern" pattern "subts" substs "->"
                                     (sort-let-binders (partial-apply-chrfns let-binders substs) (concat (flatten pattern) (keys substs)))])
          [grnd-guards ungrnd-guards] (sort-guards (partial-apply-chrfns guards substs) next-ground)
          subbed-pat (trace [:mh-rewrite] ["lvar sig:" (map variable? (rewrite pattern substs)) (bench :mh-rewrite (rewrite pattern substs))])
          next-substs (bench :mh-find-matches (find-matches root-store store substs grnd-guards grnd-binders subbed-pat))]
      (trace [:match-head] ["Matched on " pattern "with subs" next-substs "with guards"(map first grnd-guards) ])
      (when (and (empty? rst)  (not (empty? ungrnd-guards)))
        (trace [:match-head :error] ["Some guards will not be fired:" (map first guards) "with substs:" next-substs "choice informed by " next-ground]))
      (mapcat (fn [sb] (match-head root-store
                                   (bench :mh-dissoc (dissoc-constraint store (rewrite pattern sb)))
                                   sb ungrnd-guards ungrnd-binders rst))
              next-substs))
    [[substs store]]))

(defn matching-rule-seq
  "doesn't filter against propagation history.
   returns a seq of [fired-rule, subs, next-store] tuples."
  [store rules active-constraint]
  (for [rule rules
        [_op pattern] (:head rule)
        :let [[grnd-binders ungrnd-binders newly-ground]
              , (bench :sort-guards
                       (do
                         (trace [:matching-rule-seq] ["initial binders with" pattern "variable sig:" (map variable? pattern) "on store" store "with AC" active-constraint])
                         (sort-let-binders (:let-binders rule) pattern)))
              [grnd-guards ungrnd-guards] (bench :sort-guards
                                                 (sort-guards (:guards rule) newly-ground))
              _ (trace [:matching-rule-seq] ["Unground guards" (map first ungrnd-guards)])]
        next-substs (bench :find-matches
 (find-matches store (impose-constraint {} active-constraint) {} grnd-guards grnd-binders pattern))
        [sibling-substs s0] (trace [:awake :search]
                                   ["subs" next-substs
                                    "on pattern:" pattern
                                    "with grnd-guards" grnd-guards]
                                   (bench :match-head
                                          (match-head store
                                                      store
                                                      next-substs
                                                      ungrnd-guards
                                                      ungrnd-binders
                                                      (filter #(not= pattern %)
                                                              (map second (:head rule))))))]
    [rule sibling-substs s0]))

(defn fire-rule
  [fired-rule substs store]
  (trace [:fire-rule] [(:name fired-rule) "args:" substs "store" store ])
  (concat (map #(rewrite % substs) (:body fired-rule))
          (when-let [[args bfn] (:bodyfn fired-rule)]
            (apply bfn store (rewrite args substs)))))

(defn group-pairs
  "like group-by, except groups by first elt as the keys,
   and a seq of second elts as values."
  [seq-of-pairs]
  (into {}
         (map (fn [[k v]] [k (map second v)])
              (group-by first seq-of-pairs))))

(def rule-propagation-limit (atom 10000))
(def propagations (atom 0))

(defn awake
  ([rules initial-constraints]
     (do (reset! propagations 0)
         (awake {} rules (first initial-constraints) (rest initial-constraints) #{} nil)))
  ([rules store initial-constraints]
     (do (reset! propagations 0)
         (awake store rules (first initial-constraints) (rest initial-constraints) #{} nil)))
  ([store rules active-constraint queued-constraints prop-history continued-rule-matches]
     (if active-constraint
       (let [_ (when (> (swap! propagations inc) @rule-propagation-limit)
                 (error "Rule propagation overflow."))
             t1 (nanotime)
             [[fired-rule substs next-store new-constraints] & next-rule-matches]
             , (filter
                (fn [[fired-rule substs next-store new-constraints]]
                  (let [{kept :+ removed :-} (group-pairs (map (fn [[op pat]] [op (rewrite pat substs)])
                                                               (:head fired-rule)))]
                    (and (not= (into #{} (concat kept removed))
                               (into #{} (concat kept new-constraints)))
                         (if (:tabled fired-rule)
                           (not (prop-history [fired-rule substs new-constraints]))
                           true))))
                (map (fn [[fired-rule substs next-store]]
                       [fired-rule substs next-store (fire-rule fired-rule substs next-store)])
                     (or continued-rule-matches
                         (matching-rule-seq store rules active-constraint))))]
         (if (and (empty? (bench :find-matches (find-matches store [] active-constraint)))
                  fired-rule)
           (let [_ (bench-here :awake-found t1)
                 t2 (nanotime)
                 _ (trace [:awake] [(map (fn [[op pat]] [op (rewrite pat substs)]) (:head fired-rule))])
                 next-history (if (:tabled fired-rule)
                                (into prop-history [[fired-rule substs new-constraints]])
                                prop-history)
                 {kept-awake [:+ true],
                  kept-asleep [:+ false]}
                 ,  (group-pairs (map (fn [[op pat]]
                                        (let [t (rewrite pat substs)]
                                          [[op (= t active-constraint)] t]))
                                      (:head fired-rule)))
                 [next-active & next-queued] (concat new-constraints
                                                     kept-awake
                                                     queued-constraints)]
             (trace [:awake :firing] [(:name fired-rule) "on store:" store "::"active-constraint"::" queued-constraints
                                      "kept-awake:" kept-awake "kept-asleep:" kept-asleep "creating" new-constraints "with subs:" substs])
             (bench-here (:name fired-rule) t2)
             ;"If no constraints to be removed, maintain same store and position within the iterator."
             (if (empty? (filter (fn [[op _]] (= op :-)) (:head fired-rule)))
               (recur store
                      rules active-constraint (doall (concat new-constraints queued-constraints)) next-history (or next-rule-matches [])) ;must distinguish between empty and nil, nil means don't use, empty means none left-
               (recur (reduce impose-constraint next-store kept-asleep)
                      rules next-active next-queued next-history nil)))
           (do
             (bench-here :awake-fail t1)
             (trace [:awake :awake-fail] ["store" store "active c:" active-constraint "::" queued-constraints])
             (recur (impose-constraint store active-constraint)
                    rules
                    (first queued-constraints)
                    (rest queued-constraints)
                    prop-history
                    nil))))
       store)))