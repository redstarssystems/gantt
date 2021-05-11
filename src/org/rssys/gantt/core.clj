(ns org.rssys.gantt.core
  (:gen-class)
  (:require
    [io.pedestal.log :as log]))


(defn set-global-exception-hook
  "Catch any uncaught exceptions and print them."
  []
  (Thread/setDefaultUncaughtExceptionHandler
    (reify Thread$UncaughtExceptionHandler
      (uncaughtException
        [_ thread ex]
        (println "uncaught exception" :thread (.getName thread) :desc ex)))))


(defn -main
  "entry point to app."
  [& args]
  (set-global-exception-hook)
  (log/info :msg "app is started." :args args)

  (System/exit 0))
