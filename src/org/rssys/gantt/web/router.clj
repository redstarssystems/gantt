(ns org.rssys.gantt.web.router
  (:require
    [reitit.ring]))


(def ^{:doc "Routes for health checks, like liveness / readiness probes."}
  health-routes
  ["/health"
   ["/alive" {:handler (constantly {:status 200  :headers {"Content-Type" "text/plain"} :body "ok"})}]
   ["/ready" {:handler (constantly {:status 200  :headers {"Content-Type" "text/plain"} :body "ok"})}]])


(def routes
  [""
   health-routes])


(defn make-router
  []
  (reitit.ring/router routes))

