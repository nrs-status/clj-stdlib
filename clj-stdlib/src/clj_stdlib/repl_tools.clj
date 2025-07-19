(ns clj-stdlib.repl-tools
  (:require 
    [clojure.reflect :refer [reflect]]
    [clojure.java.shell :as sh]
    [clj-stdlib.fp :refer [amp split-whenever-elm-eq-excluding]]
    [clj-stdlib.preds :refer [contains-conseq?]])
  )


(def all-ns-names #(map (fn [x] (.name x)) (all-ns)))

(def all-ns-containing (fn [string] (filter (comp 
                                 #(contains-conseq? % (vec string))
                                 vec str
                                 (partial ns-name))
                               (all-ns))))

(def classpath #(java.lang.System/getProperty "java.class.path"))
(def classpath-list
  #(split-whenever-elm-eq-excluding \: (classpath)))

; introspect jar classes
(def jar-tf
  (fn [target] (sh/sh "jar" "tf" target)))

(def get-java-obj-members-names 
  (fn [java-obj]
    (let [members (:members (reflect java-obj))
          sorted-names (sort (map (comp str :name) members))]
      sorted-names)))


