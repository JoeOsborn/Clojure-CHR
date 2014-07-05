(ns chr.debug
  (:require [clojure.set :as set]))

(def nanotime 
  #+clj (fn [] (System/nanoTime))
  #+cljs 
    (let [perf (.-performance js/window)]
      (or (.-now perf)
          (.-webkitNow perf)
          (fn [] (.getTime (js/Date.))))))

(def trace-set (atom #{}))
(def trace-ignore (atom #{}))
(defn trace
  ([labels strs]
     (trace labels strs (last strs)))
  ([labels strs expr]
     (when (and (not-empty (set/intersection (into #{:all} labels) @trace-set))
                (empty? (set/intersection (into #{} labels) @trace-ignore)))
       (print (last labels))
       (print ", ")
       (doall (for [s strs] (print s "")))
       (println)
       (flush))
     expr))

(def times (atom {}))

(defn reset-bench []
  (swap! times (fn [_] {})))

(defn bench-here
  "add a benchmark time from the given start-time (in nanoseconds)"
  [bench-key start-time]
  (let [end-time (nanotime)]
    (swap! times update-in [bench-key] (fn [[old-count old-time]]
                                         [(inc (or old-count 0))
                                          (+ (or old-time 0) (* (- end-time start-time)
                                                                0.000001))]))))
