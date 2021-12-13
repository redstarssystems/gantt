(ns org.rssys.gantt.web.server
  (:require
    [clojure.string :as string]
    [io.pedestal.log :as log]
    [org.rssys.gantt.util :refer [prf]]
    [org.rssys.gantt.web.router :as router]
    [reitit.ring]
    [ring.adapter.jetty :as jetty]
    [ring.middleware.content-type :as content-type]
    [ring.middleware.defaults :as ring.defaults]
    [ring.middleware.reload :refer [wrap-reload]]
    [ring.middleware.resource]
    [ring.middleware.session :refer [wrap-session]])
  (:import
    (org.eclipse.jetty.server
      Server)))


(def ^{:doc "Site default opts."}
  site-default-opts
  (-> ring.defaults/site-defaults
    (assoc-in [:security :anti-forgery] false)))


(defn wrap-site-defaults
  "Site default opts middleware."
  [handler]
  (ring.defaults/wrap-defaults handler site-default-opts))


(defn wrap-site-session
  "Site default opts middleware."
  [handler]
  (wrap-session handler {:cookie-attrs {:max-age 3600}}))


(defn only-images-middleware
  "Allows only png,svg files"
  [handler]
  (fn [request]
    (let [allowed-extensions #{"png" "svg"}
          uri                (:uri request)]
      (if (some #(string/ends-with? uri %) allowed-extensions)
        (handler request)
        {:status  415
         :headers {"Content-Type" "text/html"}
         :body    "Only images are allowed"}))))


(defn make-handler
  [{:keys [input-folder]}]
  (prf "Serving %s folder. Use it as root path to access image files." input-folder)
  (reitit.ring/ring-handler
    (router/make-router)
    (reitit.ring/routes
      (reitit.ring/create-file-handler {:path "/" :root input-folder})
      ;; (reitit.ring/create-resource-handler {:path "/" :root "public"})
      (reitit.ring/create-default-handler))
    {:middleware [wrap-site-defaults
                  wrap-site-session
                  only-images-middleware
                  content-type/wrap-content-type]}))


(def ^:dynamic *dev-mode* true)

(def app (partial make-handler))

(def dev-app (wrap-reload #'app))


(defn run
  ^Server
  [opts]
  (let [host (or (str (:server opts)) "0.0.0.0")
        port (or (:port opts) 8080)]
    (prf "Running web server on address %s and port %s" host port)
    (log/info :msg (format "Running web server in %s mode." (if *dev-mode* "dev" "prod")))
    (jetty/run-jetty (if *dev-mode* (dev-app opts) (app opts))
      {:host  host
       :port  port
       :join? false})))


(defn stop
  [^Server server]
  (log/info :msg "Stopping server...")
  (.stop server))


(comment
  (def s (run {:input-folder "/Users/mike/projects/planning"}))
  (stop s)

  )

