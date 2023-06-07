(ns clj.core
  (:gen-class)
  (:require [cheshire.core :as json]
            [compojure.core :refer [defroutes POST]]
            [compojure.route :as route]
            [libpython-clj2.python :as py]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.util.response :as response]))

(py/initialize! :python-executable "../.venv/bin/python")

(py/from-import sentence_transformers SentenceTransformer util)

(def model
  (SentenceTransformer "all-mpnet-base-v2" :cache_folder "models"))

(defn calculate-score
  [example query]
  (let [embeddings (py/$a model encode [example] :convert_to_tensor true)
        query-embeddings (py/$a model encode [query] :convert_to_tensor true)
        scores (py/$a util cos_sim query-embeddings embeddings)]
    (ffirst (py/$a scores tolist))))

(defn api-handler [request]
  (let [example (get-in request [:body :example])
        query (get-in request [:body :query])]
    (if (and example query)
      (response/response (json/generate-string (calculate-score example query)))
      (response/bad-request "Missing 'example' and/or 'query' parameters."))))

(defroutes app-routes
  (POST "/" [] api-handler)
  (route/not-found "Not Found"))

(def app
  (-> app-routes
      wrap-params
      (wrap-json-body {:keywords? true})
      wrap-json-response))

(defn -main [& args]
  (jetty/run-jetty app {:port 8080 :join? false}))
