(ns leiningen.metrics.missing-docstrings
  (:use [clojure.java.io :only [file]]
        [leiningen.metrics.code :only [codes get-ns find-calls]]))

(defn missing-docstring? [e]
  (and (list? e)
       (>= (count e) 3)
       (not (string? (nth e 2)))))

(defn missing-docstring* [code]
  (let [ns (get-ns code)
        defns (find-calls code 'defn)
        defns (filter missing-docstring?
                      defns)]
    (if (not (empty? defns))
      {ns (map second defns)})))

(defn missing-docstrings
  ([project args]
     (missing-docstrings {} project args))
  ([metrics project args]
     (let [src-path (file (:source-path project))
           codes (codes src-path)]
       (assoc metrics :docstring
              (apply merge (map missing-docstring* codes))))))
