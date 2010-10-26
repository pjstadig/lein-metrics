(ns leiningen.metrics.counts
  (:use [clojure.java.io :only [file]]
        [leiningen.metrics.code :only [find-calls get-ns codes find-files]]))

(defn- count-calls* [sym code]
  (let [ns (get-ns code)]
    {ns {sym (count (find-calls code sym))}}))

(defn- count-calls [counts sym codes]
  (reduce (partial merge-with merge)
          counts
          (map (partial count-calls* sym) codes)))

(defn counts
  ([project args]
     (counts {} project args))
  ([metrics project args]
     (let [src-path (file (:source-path project))
           codes (codes src-path)]
       (assoc metrics
         :count
         (-> {}
             (assoc :files (count (find-files src-path
                                              (constantly true))))
             (assoc :namespaces (-> {}
                                    (count-calls 'defn codes)
                                    (count-calls 'defn- codes)
                                    (count-calls 'def codes)
                                    (count-calls 'defmacro codes))))))))

