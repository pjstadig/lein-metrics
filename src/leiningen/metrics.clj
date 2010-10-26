(ns leiningen.metrics
  (:use [clojure.java.io :only [file reader]]
        [leiningen.metrics.code :only [read-code get-ns find-calls]]
        [leiningen.metrics.counts :only [counts]]
        [leiningen.metrics.missing-docstrings :only [missing-docstrings]])
  (:import [java.io PushbackReader]
           [java.util Date]
           [java.text SimpleDateFormat]))

(defn- metrics-dir [project]
  (let [f (file (.getParent (file (:source-path project)))
                ".metrics"
                (.format (SimpleDateFormat. "yyyyMMddhhmmss") (Date.)))]
    (when-not (.exists f)
      (.mkdirs f))
    f))

(defn write-metrics [dir metrics]
  (doseq [[report metric] metrics]
    (let [path (file dir (str (name report) ".clj"))]
      (.mkdirs (.getParentFile path))
      (spit path metric))))

(defn- usage []
  (println "Try lein metrics count"))

(defn metrics-command [project command args]
  (cond
   (= "count" command) (counts project args)
   (= "missing-docstrings" command) (missing-docstrings project args)
   (empty? command) (-> {}
                        (counts project args)
                        (missing-docstrings project args))
   :else (println "Unknown command")))

(defn metrics [project & [command & args]]
  (if (= "help" command)
    (usage)
    (write-metrics (metrics-dir project)
                   (metrics-command project command args))))
