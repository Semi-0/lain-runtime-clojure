(ns run-tests
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as test]))

(defn- test-files []
  (->> (file-seq (io/file "test"))
       (filter #(.isFile %))
       (filter #(str/ends-with? (.getName %) "_test.clj"))
       (sort-by #(.getPath %))))

(defn- test-namespace [file]
  (let [path (.getPath file)
        relative (subs path (count "test/"))]
    (-> relative
        (str/replace #"\.clj$" "")
        (str/replace "_" "-")
        (str/replace "/" ".")
        symbol)))

(defn -main [& _]
  (let [namespaces (mapv test-namespace (test-files))]
    (doseq [namespace namespaces]
      (require namespace))
    (let [{:keys [test pass fail error]} (apply test/run-tests namespaces)]
      (println :tests test :assertions pass :failures fail :errors error)
      (if (zero? (+ fail error))
        nil
        (System/exit 1)))))
