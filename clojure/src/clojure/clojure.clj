(ns clojure.clojure
  (:gen-class)
  (:require [libpython-clj2.python :as py]))

(py/initialize! :python-executable "../.venv/bin/python")

(defn greet
  "Callable entry point to the application."
  [data]
  (println (str "Hello, " (or (:name data) "World") "!")))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (greet {:name (first args)}))
