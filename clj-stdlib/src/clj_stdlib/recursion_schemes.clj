(ns clj-stdlib.recursion-schemes
  )

(def fmap
  (fn this [unit? f x]
    (cond
      (unit? x) (f x)
      (coll? x) (map #(this unit? f %) x)
      :else (throw (Exception. "non-uniform argument during fmap")))))

; alg: Functor unit? -> unit?
(def mock-cata
  (fn this [unit? get-node get-next x]
    (if (unit? x) x
      (list 'f (cons (get-node x) (map (partial this unit? get-node get-next) (get-next x)))))
    ))
