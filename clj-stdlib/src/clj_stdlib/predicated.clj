(ns clj-stdlib.predicated)

(def lift (fn [f x] {:predication (f x) :val x}))
(def lift# (fn [f] (partial lift f)))
(def and_ (fn [f x] (cond
                                (:predication x) (assoc x :predication (f (:val x)))
                                :else x)))
(def and_# (fn [f] (partial and_ f)))
(def or_ (fn [f x] (cond
                               (:predication x) x
                               :else (assoc x :predication (f (:val x))))))
(def or_# (fn [f] (partial or_ f)))
