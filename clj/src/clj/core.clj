(ns clj.core
  (:gen-class)
  (:require [libpython-clj2.python :as py]))

(py/initialize! :python-executable "../.venv/bin/python")

(py/from-import sentence_transformers SentenceTransformer util)

(def model
  (SentenceTransformer "paraphrase-MiniLM-L6-v2" :cache_folder "models"))

(py/$a model encode ["Hello world!"] :convert_to_tensor true)

(defn get-scores
  [examples query]
  (let [embeddings (py/$a model encode examples :convert_to_tensor true)
        query-embeddings (py/$a model encode [query] :convert_to_tensor true)
        scores (py/$a util cos_sim query-embeddings embeddings)]
    (py/$a scores tolist)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
