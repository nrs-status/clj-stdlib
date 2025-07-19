(ns clj-stdlib.permutations)

(def choose-aux
  (fn this [choose-sq target-sq]
    (lazy-seq
     (let [choice (nth target-sq (first choose-sq))
           wo-choice (remove #{choice} target-sq)]
       (cons choice (this (rest choose-sq) wo-choice))))))

(def choose
  (fn [choose-sq target-sq]
    (take (count target-sq) (choose-aux choose-sq target-sq))))

(def int->choose-sq
  (fn this [x sq-len]
    (if
     (zero? sq-len) nil
     (let [modval (mod x sq-len)]
       (cons modval (this (quot x sq-len) (dec sq-len)))))))

(def all-choose-sq
  (fn [sq-len]
      (map #(int->choose-sq % sq-len) (range))))

(def all-permutations
  (fn [sq]
    (let [sq-len (count sq)]
     (map #(choose % sq) (all-choose-sq sq-len)) 
      )
    ))

