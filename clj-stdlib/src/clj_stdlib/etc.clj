(ns clj-stdlib.etc
  (:require 
    [clojure.spec.alpha :as s])
  )

(def split-every-n (fn [n target]
                     (let [next-split (split-at n target)]
                       (if (empty? (second next-split))
                         [target]
                         (cons (first next-split) (split-every-n n (second next-split)))))))

(def !
  (fn [& args]
    (s/assert (s/spec #(= 0 (mod % 2))) (count args))
    (let [splitting (split-every-n 2 args)
          _! (s/assert #(s/spec (every? keyword? (map first %))) splitting)]
      (doseq [[kw target] splitting]
        (s/assert (keyword "specs" (apply str (rest (str kw)))) target)))))




(defn split-w-index-list [target is]
  (! :vec target)
  (if (empty? is) [target]
      (let [first-part (vec (take (first is) target))
            second-part (subvec target (first is))
            new-indexes (map #(- % (first is)) (rest is))]
        (reduce conj [first-part] (split-w-index-list second-part new-indexes)))))

(def split-with-rec
  (fn this [pred sq]
    (lazy-seq
     (when-let [s (seq sq)]
       (cons (take-while pred s) (this pred (rest (drop-while pred s))))))))

(def scroll
  (fn [vecarg]
    (let [cons-last (cons (last vecarg) vecarg)]
      (pop (vec cons-last)))))

(def scroll-rev
  (fn [vecarg]
    (reverse (scroll (reverse vecarg)))))

(def foldl-writing
  (fn [foldlaux start-val target run-writer]
    (if (= 0 (count target)) (conj run-writer start-val)
        (let [new-run-writer (conj run-writer start-val)
              acc start-val
              next (first target)
              application (foldlaux acc next)] (foldl-writing foldlaux application (rest target) new-run-writer)))))


(def mk-recursive-app
  (fn [n f target]
    (if (<= n 0) target
      (mk-recursive-app (- n 1) f (f target)))))

(def mk-recursive-app-w-writer
  (fn [n f target run-writer]
    (if (<= n 0) run-writer
      (let [application (f target)
            new-run-writer (conj run-writer application)] 
        (mk-recursive-app-w-writer (- n 1) f application new-run-writer)))))


(def kw->str
  (fn [kw]
    (apply str (rest (str kw)))
  ))
