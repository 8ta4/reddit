(ns clj.core
  (:gen-class)
  (:require [libpython-clj2.python :as py]
            [ring.adapter.jetty :as jetty]
            [compojure.core :refer [defroutes POST]]
            [compojure.route :as route]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [ring.util.response :as response]
            [cheshire.core :as json]))

(py/initialize! :python-executable "../.venv/bin/python")

(py/from-import sentence_transformers SentenceTransformer util)

(def model
  (SentenceTransformer "all-mpnet-base-v2" :cache_folder "models"))

(defn get-scores
  [examples query]
  (let [embeddings (py/$a model encode examples :convert_to_tensor true)
        query-embeddings (py/$a model encode [query] :convert_to_tensor true)
        scores (py/$a util cos_sim query-embeddings embeddings)]
    (vec (first (py/$a scores tolist)))))

(defn api-handler [request]
  (let [examples (get-in request [:body :examples])
        query (get-in request [:body :query])]
    (println (get-scores examples query))
    (if (and examples query)
      (response/response (json/generate-string {:scores (get-scores examples query)}))
      (response/bad-request "Missing 'examples' and/or 'query' parameters."))))

(defroutes app-routes
  (POST "/api" [] api-handler)
  (route/not-found "Not Found"))

(def app
  (-> app-routes
      wrap-params
      (wrap-json-body {:keywords? true})
      wrap-json-response))

(defn -main [& args]
  (jetty/run-jetty app {:port 8080 :join? false}))
