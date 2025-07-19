(ns clj-stdlib.tree
  (:require
   [clj-stdlib.fp :refer [flip conj!! into!! unwrap]]))

(defrecord node [path head next])

(def lift-to-node
  "expects an argument that has the form of the contents of a :next"
  (fn [unit? non-unit? get-node get-nxt parent-path x]
    (map-indexed
     (fn [i x']
       (cond (unit? x')
             (->node (conj parent-path i) x' nil)

             (non-unit? x')
             (->node (conj parent-path i) (get-node x') (get-nxt x'))

             :else
             (throw (Exception. (str "val is neither unit nor non-unit: " (prn-str x'))))))
     x)))

(def pathify-aux
  (fn this [unit? non-unit? get-node get-nxt x]
    (map
     (fn [x']
       (update x' :next
               #(if
                 (nil? %) nil
                 (map-indexed
                  (fn [i x'']
                    (let [newpath (conj (.path x') i)]
                      (cond
                        (unit? x'')
                        (->node newpath x'' nil)

                        (non-unit? x'')
                        (->node newpath (get-node x'') (this unit? non-unit? get-node get-nxt (lift-to-node unit? non-unit? get-node get-nxt newpath (get-nxt x''))))

                        :else
                        (throw (Exception. (str "val is neither unit nor non-unit: " (prn-str x'')))))))

                  %))))
     x)))

(def pathify
  (fn [unit? non-unit? get-node get-nxt x]
    (cond
      (and (map? x) (every? some? (map #{:head :path :next} (keys x))))
      (pathify-aux unit? non-unit? get-node get-nxt x)

      (non-unit? x)
      (->node (get-node x) [0] 
              (pathify-aux unit? non-unit? get-node get-nxt (lift-to-node unit? non-unit? get-node get-nxt [0] (get-nxt x))))

      (every? true? (map #(or (unit? %) (non-unit? %)) x))
      (pathify-aux unit? non-unit? get-node get-nxt (lift-to-node unit? non-unit? get-node get-nxt [] x))

      :else
      (throw (Exception. (str "pathify; arg is neither a node/leaf, a non-unit, or structured like a :next: " (prn-str x)))))))

(def get-node-mod-unit
  (fn [unit? f]
    (fn [x]
      (if (unit? x) x
          (f x)))))

(def path-get
  (fn this [path current-target]
    (let [path-lengths-in-target (count (.path (first current-target)))
          filtering (filter
                     #(= (:path %) (vec (take path-lengths-in-target path)))
                     current-target)]
      (cond
        (empty? filtering) (throw (Exception. (str "path not found: " (prn-str path))))
        (and (= 1 (count filtering)) (= path-lengths-in-target (count path))) (unwrap filtering)
        :else (this path (apply concat (map :next current-target)))))))

(def path-update
  (fn this [path f target]
    (map
     (fn [node-kvmap]
       (cond
         (= (:path node-kvmap) path)
         (let [[new-node new-nxt] (f (:head node-kvmap) (:next node-kvmap))]
           ((comp
             #(assoc % :head new-node)
             #(assoc % :next new-nxt)) node-kvmap))

         (nil? (:next node-kvmap)) node-kvmap
         :else
         (update node-kvmap :next #(this path f %))))
     target)))

(def leaves
  (fn [target]
    (let [atm (atom [])]
      (mapv
       (fn this [x]
         (if
          (nil? (:next x))
           (conj!! atm x)
           (mapv this (:next x))))
       target)
      @atm)))

(def max-depth
  (fn [x]
    (apply max (map (comp count :path) (leaves x)))))

(def all-paths
  (fn this [x]
    (let [atm (atom [])]
      (mapv
       (fn [x']
         (conj!! atm (:path x'))
         (if (nil? (:next x')) nil
             (into!! atm (this (:next x')))))
       x)
      @atm)))

(def immediate-children-paths
  (fn [path full-tree]
    (let [path-len (count path)
          all-paths (all-paths full-tree)
          potential-children (filter #(= (count %) (inc path-len)) all-paths)]
      (filter
       #(= path (vec (take path-len %)))
       potential-children))))

(def node?
  (fn [x] (= (set (keys x)) #{:head :path :next})))

(def leaf?
  (fn [x] (nil? (:next x))))


(def lift
  (fn [tag target] (map-indexed (fn [i x] {:path [i] :tag tag :prev nil :next x})
                                target)))

(def focus-of-lifted
  (fn [tag target f]
    (apply
     concat
     ((flip map)
      target
      (fn [t'd]
        (map-indexed
         (fn [i elm] {:path (conj (:path t'd) i) :tag tag :prev elm :next (f elm)})
         (:next t'd)))))))

(def clear-nil (fn [x] (filter #(some? (:next %)) x)))

; grab varname to register z

;above set to @g, eval, set mn then go to `e, replace name, eval, then return
; mn@g er`ewwviw" zp er `n


