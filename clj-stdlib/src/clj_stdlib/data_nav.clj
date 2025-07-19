(ns clj-stdlib.data-nav)

(def c1 (constantly 1))
(def id identity)
(def reset-when-contains
  (fn [atm target-coll elm]
    (when (contains? target-coll elm) (reset! atm elm))))


(def introspection-atm (atom []))
(def conj!_ (fn [atm to-conj] (reset! atm (conj @atm to-conj))))
(def iconj! (fn [x] (do (conj!_ introspection-atm x) x)))
