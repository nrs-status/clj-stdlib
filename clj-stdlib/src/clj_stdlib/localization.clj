(ns clj-stdlib.localization
  (:require
   [clojure.set :refer [intersection]]
   [clj-stdlib.etc :refer [!]]
   [clj-stdlib.fp :refer [act-on-self_pairs clear-nil into!! conj!! =# split-doing# singleton apply-n-times rec-app-until filter# apply# tuplize map-nilable map-nilable# map# enumerate pairs-of-self]]))

(def localize-transformation'
  (fn this [location-maker
            background-maker
            location-transformer
            transformation-keeping
            new-ctx-maker
            concat-method
            target]
    (let [location (location-maker target)
          background (background-maker target)
          transformation (location-transformer location)
          to-keep (transformation-keeping transformation)
          new-context (new-ctx-maker transformation background)]
      (if (empty? background)
        to-keep
        (concat-method to-keep
                       (this
                        location-maker
                        background-maker
                        location-transformer
                        transformation-keeping
                        new-ctx-maker concat-method new-context))))))

(def localize-transformation
  (fn [location-maker
       background-maker
       location-transformer
       transformation-keeping
       new-ctx-maker
       target]
    (loop [location (location-maker target)
           background (background-maker target)
           transformation (location-transformer location)
           to-keep (transformation-keeping transformation)
           new-context (new-ctx-maker transformation background)
           acc (into [] to-keep)]
      (if (empty? background)
        (into ((comp vec reverse rest reverse) acc) transformation)
        (let [location (location-maker new-context)
              background (background-maker new-context)
              transformation (location-transformer location)
              to-keep (transformation-keeping transformation)
              new-context (new-ctx-maker transformation background)]
          (recur
           location
           background
           transformation
           to-keep
           new-context
           (into acc to-keep)))))))

(def ignore-nil
  #(cond
     (and (= %1 [nil]) (= %2 [nil])) []
     (= (vec %1) [nil]) %2
     (= (vec %2) [nil]) %1
     :else (concat %1 %2)))

(def length_based-scanning-transformation
  (fn [length f-on-location transformation-keeping new-ctx-maker target]
    (localize-transformation'
     (partial take length)
     (partial drop length)
     f-on-location
     transformation-keeping
     new-ctx-maker
     ignore-nil
     target)))

(def remove-all-conseq'
  (fn [conseq target]
    (let [len (count conseq)]
      (length_based-scanning-transformation
       len
       #(if (= (vec %) conseq) nil %)
       (partial first)
       #(concat (rest %1) %2)
       target))))

(def replace-conseq'
  (fn [conseq replace-with target]
    (let [transformation-triggered? (atom false)]
      (length_based-scanning-transformation
       (count conseq)
       #(if (= % conseq) (do (reset! transformation-triggered? true) replace-with) %)
       #(if @transformation-triggered?
          %
          [(first %)])
       #(if @transformation-triggered?
          (do (reset! transformation-triggered? false) %2)
          (concat (rest %1) %2))
       target))))

(def replace-conseq
  (fn [conseq replace-with target]
    (let [tfm-triggered? (atom false)]
      (localize-transformation
       (partial take (count conseq))
       (partial drop (count conseq))
       #(if (= (vec %) conseq) (do (reset! tfm-triggered? true) replace-with) %)
       #(if @tfm-triggered?
          %
          [(first %)])
       #(if @tfm-triggered?
          (do (reset! tfm-triggered? false) %2)
          (concat (rest %1) %2))
       target))))

(def when-intersect-concat
  (fn [[l r]]
    (when (not-empty (intersection l r))
      (set (concat l r)))))

; causes stack overflows on big targets
(def split-non-conseq
  (fn [target]
    ((comp
      (partial sort-by first)
      (map# (comp drop-last sort vec))
      set
      (partial
       apply-n-times
       (count target)
       (comp
        (map# (comp
               set
               flatten
               (map# vec)
               (map-nilable# when-intersect-concat)))
        pairs-of-self))
      (map# (fn [x] #{x (inc x)})))
     target)))
(split-non-conseq [0 1 2 5 6 10 11])

; zones must be 2-elm int intervals
(def act-on-zones'
  (fn [f-on-zones zones target]
    (let
     [ranges (map (fn [[l r]] (range l (inc r))) zones)
      out-of-zone (filter #(not ((set (flatten ranges)) %))
                          (range (count target)))
      out-of-zone-locs (split-non-conseq out-of-zone)
      enriched-out-of-zone-locs (map (split-doing# identity (map# (partial get target)))
                                     out-of-zone-locs)
      enriched-ranges (map (split-doing# identity (map# (partial get target))) ranges)
      ranges-w-tfm (map (split-doing# first (comp f-on-zones second)) enriched-ranges)
      all-together (into ranges-w-tfm enriched-out-of-zone-locs)
      sorting (sort-by (comp first first) all-together)
      final ((comp (apply# concat) (map# second)) sorting)]
      final)))

(def get-conseq's-zones
  "defines a consecutive sequence by a collection of predicates on each location of the sequence"
  (fn [conseq-preds target]
    (let [tuplication (tuplize (count conseq-preds) (enumerate target))
          find-conseq (filter (fn [i-tuple] (every? true? (map #(%1 (second %2)) conseq-preds i-tuple))) tuplication)
          final (map (comp
                      (fn [x] [(first x) (last x)])
                      (map# first)) find-conseq)]
      final)))

(def get-conseq's-zones-simple
  (fn [conseq target]
    (let [as-preds (map #(fn [x] (= x %)) conseq)]
      (get-conseq's-zones as-preds target))))

(def replace-with'
  (fn [conseq to-replace-with target]
    (let [conseq-zones (get-conseq's-zones conseq target)]
      (act-on-zones' (constantly to-replace-with) conseq-zones target))))

(def mk-neighborhoods-of-constant
  (fn [constant neighborhood-maker target]
    (let [elm-locs (map first (filter (comp (=# constant) second) (enumerate target)))]
      (map neighborhood-maker elm-locs))))

(def mk-rightwards-nbds-of-const
  (fn [const target]
    (mk-neighborhoods-of-constant
     const
     (fn [x] [x (dec (count target))])
     target)))

(def rightwards-nbds-of-pos-w-split_with
  (fn [pred target const-pos]
    (let [full-rightwards-nbd (filter (comp #(<= const-pos %) first) (enumerate target))]
      (split-with pred full-rightwards-nbd))))

; (def target "xxxx#xxxxxxx\nxxxxx")
; (def z (mk-neighborhoods-of-constant \# singleton target))
; (def zz (rightwards-nbds-of-pos-w-split_with (comp not (=# \newline) second) target 4))

(def conseq?
  (fn [x y] (if (or (nil? x) (nil? y)) false (or (= (inc (last x)) (first y))
                                                 (= (inc (last y)) (first x))))))

(def gen-non_conseq-lists
  (fn [x] (clear-nil
           (act-on-self_pairs
            #(when (conseq? @%1 @%2)
               (do (into!! %1 @%2)
                   (swap! %1 sort)
                   (reset! %2 nil)))
            (map singleton x)))))

; (def mk-zones-of-sym-seq-mod-intermediates
;   (fn [sym-seq-preds intermediate-pred target]
;     (let [enum'd (enumerate target)
;           relevant? #(some true? (map (fn [sym] (% sym)) sym-seq-preds))
;           filtering
;           ]

(def mk-zones-of-sym-seq-mod-irrel
  (fn [sym-seq target]
    (let [enum'd (enumerate target)
          relevant? #(some true? (map (fn [sym] (= sym %)) sym-seq))
          filtering (filter (comp relevant? second) enum'd)
          tuplication (tuplize (count sym-seq) filtering)
          filtering2 (filter (comp (=# sym-seq) vec (map# second)) tuplication)
          mk-intvs (map (fn [x] [(ffirst x) (first (last x))])
                        filtering2)]
      mk-intvs)))

(def act-on-zones
  (fn [f-on-zones zones target]
    (! :vec target)
    (let
     [ranges (map (fn [[l r]] (range l (inc r))) zones)
      out-of-zone (filter #(not ((set (flatten ranges)) %))
                          (range (count target)))
      out-of-zone-locs (gen-non_conseq-lists out-of-zone)
      enriched-out-of-zone-locs (map (split-doing# identity (map# (partial get target)))
                                     out-of-zone-locs)
      enriched-ranges (map (split-doing# identity (map# (partial get target))) ranges)
      ranges-w-tfm (map (split-doing# first (comp f-on-zones second)) enriched-ranges)
      all-together (into ranges-w-tfm enriched-out-of-zone-locs)
      sorting (sort-by (comp first first) all-together)
      final ((comp (apply# concat) (map# second)) sorting)]
      final)))

(def replace-with
  (fn [conseq to-replace-with target]
    (let [conseq-zones (get-conseq's-zones conseq target)]
      (act-on-zones (constantly to-replace-with) conseq-zones target))))

;
; ;
; (def filepath "/home/sieyes/baghdad_plane/flakes/frontArmToPlane/temple_artemis_ephesus/montezuma_circles_scroll/resources/plugins/pluginsCore.nix")
; (def file-as-string' (apply str (concat (slurp filepath) (slurp filepath))))
; ; (def file-as-string (apply str (transform-all-conseq' [\newline \newline] [\newline] file-as-string')))
; (replace-with [\newline \newline] [\newline] file-as-string')
; (def x (get-conseq's-zones [\newline \newline] file-as-string'))
; (def y (act-on-zones (constantly [\newline \newline]) x file-as-string'))
;
; (def ranges (map (fn [[l r]] (range l (inc r))) x))
; (def out-of-zone (filter #(not ((set (flatten ranges)) %))
;                          (range (count file-as-string'))))
; (def x (map (fn [x] (seq [x (inc x)])) out-of-zone))
;
