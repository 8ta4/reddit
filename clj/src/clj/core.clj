(ns clj.core
  (:gen-class)
  (:require [libpython-clj2.python :as py]))

(py/initialize! :python-executable "../.venv/bin/python")

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))