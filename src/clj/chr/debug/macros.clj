(ns chr.debug.macros)

;comment out traces at the source level:
#_(defmacro trace
  ([labels strs] (last strs))
  ([labels strs expr] expr))

#_(defmacro bench
  [bench-key expression]
  `(let [start-time# (chr.debug/nanotime)
         e# ~expression
         end-time# (chr.debug/nanotime)]
     (swap! chr.debug/times update-in [~bench-key] (fn [[old-count# old-time#]]
                                           [(inc (or old-count# 0))
                                            (+ (or old-time# 0) (* (- end-time# start-time#)
                                                                   0.000001))]))
     e#))

(defmacro bench
  "commenting out all benchmark hooks at the sorce level."
  [bench-key expression] expression)

(defmacro no-bench
  "comment out specific benchmark hooks at the source level."
  [bench-key expression]
  expression)
