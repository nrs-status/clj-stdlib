(ns clj-stdlib.errh
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.spec.alpha :as s]))

(def when-specfail-throw (fn [spec-kw f-on-test-data-on-throw target]
                           (let [test-data (s/explain-data spec-kw target)]
                             (when (not (nil? test-data))
                               (throw (Exception. (f-on-test-data-on-throw test-data)))))))

(def when-specfail-throw-info
  (fn [spec-kw decl-name target]
    (when-specfail-throw
     spec-kw
     #(str
       "failed spec: " (:clojure.spec.alpha/spec %) "\n"
       "in decl: " decl-name "\n"
       "on data: " (with-out-str (pprint target))
       "with s/explain-data: " (with-out-str (pprint %)))
     target)))
