(ns clj-stdlib.fp)

(def tuplize
  (fn [n target]
    (let
     [identical-target-copies (repeat n target)
      numbered-copies (map vector (range n) identical-target-copies)
      dropping-mapaux (fn [x] (drop (first x) (second x)))
      dropping (map dropping-mapaux numbered-copies)
      smallest (last dropping)
      shaving (map (fn* [%1] (take (count smallest) %1)) dropping)
      innermap (fn [n] (map (fn* [%1] (nth %1 n)) shaving))
      outermap (map innermap (range (count smallest)))]
      outermap)))

(def enumerate (fn [x] (map vector (range (count x)) x)))

(def split-doing (fn [f g target] [(f target) (g target)]))
(def split-doing# (fn [f g] (partial split-doing f g)))
(def bimap (fn [f g [a b]] [(f a) (g b)]))
(def bimap# (fn [f g] (partial bimap f g)))
(def trimap (fn [f g h [a b c]] [(f a) (g b) (h c)]))
(def trimap# (fn [f g h] (partial trimap f g h)))
(def unwrap first)
(def filter# (fn [p] (partial filter p)))
(def filteruw (fn [p target] ((comp unwrap (filter# p)) target)))
(def filteruw# (fn [p] (partial filteruw p)))
(def map# (fn [f] (partial map f)))
(def sort-by# (fn [f] (partial sort-by f)))
(def reduce# (fn [f] (partial reduce f)))
(defn flip [f]
  (fn [& xs]
    (apply f (reverse xs))))

(def apply# (fn [f] (partial apply f)))
(def amp (fn [x f] (f x)))
(def amp# (fn [x] (fn [f] (f x))))
(def singleton (fn [x] [x]))

(def take# (fn [n] (partial take n)))

(def =# #(partial = %))
(def +# #(partial + %))
(def divby# (fn [x] #(/ % x)))

(def map-nilable
  (fn [f target]
    (let [mapping (map f target)]
      (filter #(not (nil? %)) mapping))))
(def map-nilable# (fn [f] (partial map-nilable f)))

(def assoc-in*
  (fn [kvs target]
    (reduce
     (fn [acc [next-keys next-val]] (assoc-in acc next-keys next-val)) target
     kvs)))

(def assoc-in*#
  (fn [kvs] (partial assoc-in* kvs)))

; deep-merge 
(defn deep-merge-with [f & maps]
  (apply
   (fn m [& maps]
     (if (every? map? maps)
       (apply merge-with m maps)
       (apply f maps)))
   maps))

(def keep-left
  (fn [a _b] a))
(def keep-right
  (fn [_a b] b))

(def deep-merge<-
  (fn [& maps]
    (apply deep-merge-with keep-left maps)))

(def deep-merge<-#
  (fn [first-map]
    (partial deep-merge<- first-map)))

(def ><-$-v-set
  (fn [& maps]
    (apply
     deep-merge-with
     (fn [f x] (if (nil? x) f (f x)))
     maps)))

(def ><-$-v-set#
  (fn [x] (partial ><-$-v-set x)))

(def flatten-aux
  (fn this [unit? acc next_]
    (if (unit? next_)
      (conj acc next_)
      (reduce (partial this unit?) acc next_))))

(def flatten-up-to-unit
  (fn [unit? x]
    (reduce (partial flatten-aux unit?) [] x)))

(def rec-app-until-aux
  (fn [end-cond init]
    (fn this [f' current]
      (let [result (f' current)]
        (if (end-cond init current result) result
            (this f' result))))))

(def rec-app-until
  (fn [f end-cond init]
    ((rec-app-until-aux end-cond init) f init)))

(def rec-app-until-changeless
  (fn [f init] (rec-app-until
                f
                (fn [_init current result] (= current result))
                init)))

(def apply-n-times
  (fn [n f target]
    (reduce #(%2 %1) target (repeat n f))))

(def pairs-of-self
  (fn [x] (map (fn [y] (map (fn [z] [y z]) x))
               x)))

(def split-whenever-elm-eq-excluding
  (fn [toeq str_]
    ((fn this [toeq str_ acc]
       (let [until-toeq (apply str (take-while #(not (= % toeq)) str_))
             at-toeq-and-next (apply str (rest (drop-while #(not (= % toeq)) str_)))]
         (if (empty? at-toeq-and-next)
           (into acc [until-toeq])
           (this toeq at-toeq-and-next (into acc [until-toeq]))))) toeq str_ [])))

(def bim#
  (fn [f] (bimap# identity (map# f))))
(def bif#
  (fn [f] (bimap# identity f)))

(def nmap
  (fn this [fs target]
    (when (not (= (count fs) (count target))) (throw (Exception. "nmap took args of different size")))
    (if (empty? fs) []
        (concat [((first fs) (first target))] (this (rest fs) (rest target))))))
(def nmap#
  (fn [fs] (partial nmap fs)))

(def reset!# (fn [atm]
               (partial reset! atm)))
(def swap!# (fn [atm]
              (partial swap! atm)))

(def conj!!
  (fn [atm x] (swap! atm (fn [y] (conj y x)))))

(def into!!
  (fn [atm x] (swap! atm (fn [y] (into y x)))))

(defn pairs-of-self'
  [x]
  (lazy-seq
   (loop [target (seq x)
          acc ()]
     (if (empty? target) acc
         (let [nxt (first target)
               loop-result
               (loop [target' target
                      acc' []]
                 (if (empty? target') acc'
                     (recur (rest target')
                            (cons (seq [nxt (first target')]) acc'))))]
           (recur (rest target)
                  (cons loop-result acc)))))))

(def act-on-self_pairs
  (fn [action x]
    (let [mutify (mapv atom x)]
      (do (mapv (fn [y] (mapv (fn [z] (action y z)) mutify)) mutify)
          (mapv deref mutify)))))

;do not manage atoms manually


(def act-on-self_pairs'
  (fn [pred f x]
    (filter some? (act-on-self_pairs
     (fn [l-atm r-atm]
       (let [l-deref @l-atm 
             r-deref @r-atm]
         (when (pred l-deref r-deref)
           (reset! r-atm nil)
           (reset! l-atm (f l-deref r-deref))
           )
         )
       )
     x))))

(defn act-on-self_pairs2
  [on-pairs-f on-row-f x]
  (lazy-seq
   (loop [target (seq x)
          acc ()]
     (if (empty? target) acc
         (let [nxt (first target)
               loop-result
               (loop [target' target
                      acc' []]
                 (if (empty? target') acc'
                     (recur (rest target')
                            (cons (on-pairs-f nxt (first target')) acc'))))]
           (recur (rest target)
                  (cons (on-row-f loop-result) acc)))))))

(def clear-nil
  (fn [x] (filter (comp not nil?) x)))

