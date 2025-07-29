(ns clj-stdlib.lazy)

(def r-nbds
  (fn this [sq]
    (lazy-seq
     (when-let [s (seq sq)]
       (cons s (r-nbds (rest s)))))))

(def radii
  (fn this [sq]
    (lazy-seq
     (when-let [s (seq sq)]
       (cons (map #(take (inc %) s) (range)) (this (rest s)))))))

(def r-nbds-dropping-while
  (fn this [p sq]
    (lazy-seq
     (when-let [nxt (first (drop-while p (r-nbds sq)))]
       (cons nxt (this p (rest nxt)))))))

