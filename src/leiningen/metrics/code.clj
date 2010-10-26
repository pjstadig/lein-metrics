(ns leiningen.metrics.code
  (:use [clojure.java.io :only [reader file]])
  (:import [java.io PushbackReader]))

(defn clj?
  "Return true if file is a clojure source file"
  [file]
  (re-find #"\.clj$" (.getCanonicalPath file)))

(defn find-files [dir pred]
  (doall (filter pred
                 (mapcat #(if (.isDirectory %)
                            (find-files % pred)
                            [%])
                         (.listFiles dir)))))

(defn read-code
  ([file]
     (read-code file []))
  ([file code]
     (let [file (PushbackReader. (reader file))
           e (read file false ::eof)]
       (lazy-seq
        (if (= ::eof e)
          code
          (read-code file (conj code e)))))))

(defn codes [dir]
  (map read-code
       (find-files dir clj?)))

(defn get-ns [code]
  (second (first (filter #(= 'ns (first %)) code))))

(defn find-calls
  ([code sym]
     (find-calls code sym []))
  ([code sym matches]
     (cond
      (or (list? code)
          (vector? code)
          (map? code)
          (seq? code))
      (cond (empty? code) matches
            (or (seq? code)
                (list? code)) (recur (rest code)
                                  sym
                                  (if (symbol? (first code))
                                    (if (= (first code) sym)
                                      (conj matches code)
                                      matches)
                                    (find-calls (first code)
                                                sym
                                                matches)))
            (vector? code) (recur (pop code)
                                  sym
                                  (find-calls (peek code)
                                              sym
                                              matches))
            (map? code) (recur (rest code)
                               sym
                               (find-calls (first code)
                                           sym
                                           matches)))
      :else matches)))
