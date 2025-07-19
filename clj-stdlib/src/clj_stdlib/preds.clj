(ns clj-stdlib.preds
  (:require
    [clj-stdlib.fp :refer [tuplize]]))

;interval predicates
(def lirn (fn [l r] (fn [i] (and (<= l i) (< i r)))))
(def lnrn (fn [l r] (fn [i] (and (< l i) (< i r)))))
(def lnri (fn [l r] (fn [i] (and (< l i) (<= i r)))))
(def liri (fn [l r] (fn [i] (and (<= l i) (<= i r)))))

(def is-contained-by
  (fn [[a b] [a' b']]
    (< a' a b b')))


;on tuplication
(def tuplication-satisfies (fn [n pred] (fn [target]
                                          (every? true? (map pred (tuplize n target))))))
(def strictly-increasing-sequence (tuplication-satisfies #(< (first %) (second %)) 2))
(def monotone-increasing-sequence (tuplication-satisfies #(<= (first %) (second %)) 2))


;conseq
(def contains?# (fn [set_] (partial contains? set_)))

(def contains-conseq?
  (fn [conseq-to-find target-coll]
    (let [tuplication (tuplize (count conseq-to-find) target-coll)]
      ((contains?# (set tuplication)) (seq conseq-to-find)))))
(def contains-conseq?# (fn [conseq-to-find]
                         (partial contains-conseq? conseq-to-find)))
