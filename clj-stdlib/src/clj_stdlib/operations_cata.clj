(ns clj-stdlib.operations-cata
  (:require
   [clj-stdlib.tree :refer [leaf? ->node node?]]
   [clj-stdlib.fp :refer [unwrap conj!! =#]]))

(def operations-cata-aux
  (fn [operations-pool]
    (fn this [x]
      (cond
        (not (node? x)) (throw (Exception. (str "operations-cata-aux; expected node, got: " (prn-str x))))
        (leaf? x) x

        :else
        (let [{:keys [head next path]} x
              lower-recursion (mapv this next)]
          (conj!! operations-pool (->node path head lower-recursion))
          (->node :ref path nil))))))

(def operations-cata
  (fn [x]
    (if (not (node? x)) (throw (Exception. (str "operations-cata; expected node, got: " (prn-str x))))
        (let [operations-pool (atom [])]
          (do ((operations-cata-aux operations-pool) x)
              @operations-pool)))))

(def pull-from-finished-pool
  (fn [path pool]
    (unwrap (filter (comp (=# path) #(.path %)) @pool))))

(def head-or-pull-from-pool
  (fn [pool x]
    (let [head (.head x)]
      (if (= :ref head)
        (:result (pull-from-finished-pool (.path x) pool))
        head))))

(def consume-next-operation
  (fn [finished-pool f]
    (fn [pathful]
      (let [{:keys [path head next]} pathful
            next's-nodes (mapv (partial head-or-pull-from-pool finished-pool) next)]
        (conj!! finished-pool {:path path :result (f (cons head next's-nodes))})))))

(def run-operations-with
  (fn [ops-alg operations-cata-result]
    (let [finished-pool (atom [])]
      (doseq [x (reverse (sort-by :path operations-cata-result))]
        ((consume-next-operation finished-pool ops-alg) x))
      @finished-pool)))

(def run-n-operations-with
  (fn [n ops-alg operations-cata-result]
    (let [finished-pool (atom [])]
      (doseq [x (take n (reverse (sort-by :path operations-cata-result)))]
        ((consume-next-operation finished-pool ops-alg) x))
      @finished-pool)))
;
; (def mytree [1 [1
;                 10
;                 [5 [10 56 [5 [10 10 10]] 5]]
;                 5
;                 9]])
; (def pathified
;   (clj-stdlib.tree/pathify
;    int?
;    #(and (int? (first %)) (coll? (second %)))
;    first
;    second
;    mytree))
;
;
; (def operations-pool (atom []))
; (def opcated ((operations-cata-aux operations-pool) pathified))

