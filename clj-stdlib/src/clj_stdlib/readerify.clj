(ns clj-stdlib.readerify)

(def readerify-on
  (fn readerify-on-fn [target distributor]
    (fn n1 [f]
      (let [result ((distributor f) target)]
    {:result result
     :extension (fn [new-distributor] #(distributor (new-distributor %)))}))))

(def rcomp
  (fn rcomp-fn [ran-reader new-distributor]
    (let [total-distributor ((:extension ran-reader) new-distributor)]
      (fn n1 [f]
        (let [result ((total-distributor f) (:result ran-reader))]
    {:result result
     :extension (fn [new-distributor] #(total-distributor (new-distributor %)))
     }
    )))))
