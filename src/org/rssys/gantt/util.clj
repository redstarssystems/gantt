(ns org.rssys.gantt.util)


(defn prf
  [& args]
  (println (apply format args)))


(defn prfe
  [& args]
  (.println System/err (apply format args)))


