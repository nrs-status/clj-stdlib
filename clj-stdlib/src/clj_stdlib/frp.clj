(ns clj-stdlib.frp
  (:require
   [clojure.spec.alpha :as s]
   [clj-stdlib.specs]
   [clj-stdlib.fp :refer [map# filter# enumerate conj!! bimap#]]
   [clj-stdlib.etc :refer [! split-every-n]]))

(s/check-asserts true)

(defrecord Signal [nm trigger-listeners subscribe trigger private])

(def registry (atom []))

(def mk-trigger-listeners
  (fn [parent-nm parent-out-atm ls-atm]
    (! :atom parent-out-atm :atom ls-atm)
    (fn []
      (mapv
       (fn [[nm listener-out-atm listener's-trigger-listeners f]]
         (! :atom listener-out-atm :fn f)
         (println parent-nm " calling trigger-listeners of " parent-nm ";  currently triggering " nm)
         (f listener-out-atm listener's-trigger-listeners @parent-out-atm))
       @ls-atm))))

(def mk-subscribe
  (fn [emitter-nm emitter-ls-atm]
    (fn [new-listener-nm new-listener-out-atm new-listener's-trigger-listeners new-listener's-on-trigger]
      (! :atom new-listener-out-atm :fn new-listener's-on-trigger)
      (println "subscribing " new-listener-nm " to " emitter-nm)
      (conj!! emitter-ls-atm [new-listener-nm new-listener-out-atm new-listener's-trigger-listeners new-listener's-on-trigger]))))

(def mk-sig
  (fn [nm default-val]
    (let [ls-atm (atom [])
          out-atm (atom default-val)
          trigger-ls (mk-trigger-listeners nm out-atm ls-atm)
          sig (Signal.
               nm
               trigger-ls
               (mk-subscribe nm ls-atm)
               (fn [swap-fn]
                 (! :fn swap-fn)
                 (swap! out-atm swap-fn)
                 (trigger-ls))
               {:ls-atm ls-atm
                :out-atm out-atm})]
      (do (conj!! registry sig)
          sig))))

(def sigfn
  "provided fns here must return a function to be used to swap! the signal resulting from this sigfn"
  (fn [nm fns-n-sigs]
    (let [pairs (split-every-n 2 fns-n-sigs)
          out-sig (mk-sig nm nil)]
      (do (mapv
           #((.subscribe (second %))
             nm
             (get-in out-sig [:private :out-atm])
             (.trigger-listeners out-sig)
             (first %))
           pairs)
          out-sig))))

(def sigfn-w-purefn-result-wrapper
  "higher-order sigfn to make new sigfns where we only need to pass pure functions. used to abstract whether listener's-trigger-listeners is called and how to mod listener's-out-atm"
  (fn [purefn-result-wrapper]
    (fn [nm fns-n-sigs]
      (let [pairs (split-every-n 2 fns-n-sigs)
            wrapping (map
                      (bimap#
                       #(fn [listener's-out-atm listener's-trigger-listeners parent-atm-deref]
                          (purefn-result-wrapper listener's-out-atm listener's-trigger-listeners (% parent-atm-deref)))
                       identity)
                      pairs)]
        (sigfn nm (apply concat wrapping))))))

(def reset-sigfn
  "reset! with purefn result and trigger-listeners"
  (sigfn-w-purefn-result-wrapper
   (fn [listener's-out-atm listener's-trigger-listeners purefn-result]
     (do (reset! listener's-out-atm purefn-result)
         (listener's-trigger-listeners)))))

(def swap-sigfn
  "swap! with purefn result and trigger-listeners"
  (sigfn-w-purefn-result-wrapper
   (fn [listener's-out-atm listener's-trigger-listeners purefn-result]
     (do (swap! listener's-out-atm purefn-result)
         (listener's-trigger-listeners)))))

(def prod
  "make signal out of the product of two signals, triggers whenever either triggers"
  (fn [siga sigb]
    (swap-sigfn
     (str (.nm siga) "×" (.nm sigb))
     [(fn [siga-deref]
        #(if (nil? %)
           [siga-deref @(get-in sigb [:private :out-atm])]
           [siga-deref (second %)])) siga
      (fn [sigb-deref]
        #(if (nil? %)
           [@(get-in siga [:private :out-atm]) sigb-deref]
           [(first %) sigb-deref])) sigb])))

(def prod*
  "does not trigger listeners when the first signal triggers"
  (fn [siga sigb]
    (let [siga-last-val (atom nil)]
      (sigfn
       (str (.nm siga) "×" (.nm sigb))
       [(fn [_ _ siga-deref]
          (reset! siga-last-val siga-deref)) siga
        (fn [listener's-out-atm listener's-trigger-listeners sigb-deref]
          (reset! listener's-out-atm [@siga-last-val sigb-deref])
          (listener's-trigger-listeners)) sigb]))))

(defmacro defsig
  {:clj-kondo/lint-as 'clojure.core/def}
  [nm val_]
  `(def ~nm (mk-sig ~(name nm) ~val_)))

(defmacro sfn
  {:clj-kondo/ignore [:unresolved-symbol :type-mismatch]}
  ([nm constvar sigvar body]
   `(fn ~(vec (concat constvar [(symbol "sig")]))
      (reset-sigfn
       ~(name nm)
       [(fn ~sigvar
          ~body)
        ~'sig])))
  ([constvar sigvar body]
   `(fn ~(vec (concat constvar [(symbol "sig")]))
      (reset-sigfn
       "anon"
       [(fn ~sigvar
          ~body)
        ~'sig])))
  ([sigvar body]
   `(fn [sig#]
      (reset-sigfn
       "anon"
       [(fn ~sigvar
          ~body)
        sig#]))))

(defmacro sfn*
  {:clj-kondo/ignore [:unresolved-symbol :type-mismatch]}
  ([nm constvar sigvar body]
   `(fn ~(vec (concat constvar [(symbol "sig")]))
      (swap-sigfn
       ~(name nm)
       [(fn ~sigvar
          ~body)
        ~'sig])))
  ([constvar sigvar body]
   `(fn ~(vec (concat constvar [(symbol "sig")]))
      (swap-sigfn
       "anon"
       [(fn ~sigvar
          ~body)
        ~'sig])))
  ([sigvar body]
   `(fn [sig#]
      (swap-sigfn
       "anon"
       [(fn ~sigvar
          ~body)
        sig#]))))

(defmacro defsfn
  {:clj-kondo/lint-as 'clj-kondo.lint-as/def-catch-all}
  ([nm constvar sigvar body]
   `(def ~nm (sfn ~nm ~constvar ~sigvar ~body)))
  ([nm sigvar body]
   `(def ~nm (sfn ~nm ~sigvar ~body))))

(defmacro defsfn*
  {:clj-kondo/lint-as 'clj-kondo.lint-as/def-catch-all}
  ([nm constvar sigvar body]
   `(def ~nm (sfn* ~nm ~constvar ~sigvar ~body)))
  ([nm sigvar body]
   `(def ~nm (sfn* ~nm ~sigvar ~body))))
