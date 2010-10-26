(ns leiningen.metrics
  (:use [clojure.java.io :only [file reader]]
        [leiningen.metrics.code :only [read-code get-ns find-calls]])
  (:import [java.io PushbackReader]
           [java.util Date]
           [java.text SimpleDateFormat]))

(defn clj? [file]
  (re-find #"\.clj$" (.getCanonicalPath file)))

(defn- find-files [dir pred]
  (doall (filter pred
                 (mapcat #(if (.isDirectory %)
                            (find-files % pred)
                            [%])
                         (.listFiles dir)))))

(defn- codes [dir]
  (doall (map read-code
              (find-files dir clj?))))

(defn- count-calls* [sym code]
  (let [ns (get-ns code)]
    {ns {sym (count (find-calls code sym))}}))

(defn- count-calls [counts sym codes]
  (reduce (partial merge-with merge)
          counts
          (map (partial count-calls* sym) codes)))

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

(defn- do-count
  ([project args]
     (do-count {} project args))
  ([reports project args]
     (let [src-path (file (:source-path project))
           clj-files (find-files src-path clj?)
           codes (doall (map #(read-code (PushbackReader. (reader %)))
                             clj-files))]
       (assoc reports
         :count
         (-> {}
             (assoc :files (count (find-files src-path
                                              (constantly true))))
             (assoc :namespaces (-> {}
                                    (count-calls 'defn codes)
                                    (count-calls 'defn- codes)
                                    (count-calls 'def codes)
                                    (count-calls 'defmacro codes))))))))

(defn metrics-command [project command args]
  (cond
   (= "count" command) (do-count project args)
   (empty? command) (-> {}
                      (do-count project args))
   :else (println "Unknown command")))

(defn metrics [project & [command & args]]
  (if (= "help" command)
    (usage)
    (write-metrics (metrics-dir project)
                   (metrics-command project command args))))
