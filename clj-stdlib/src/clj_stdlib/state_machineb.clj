(ns clj-stdlib.state-machineb
  (:require
   [clojure.spec.alpha :as s]
   [clj-stdlib.specs]
   [clj-stdlib.fp :refer [map# filter# enumerate conj!! bimap#]]
   [clj-stdlib.etc :refer [! split-every-n]]))
(s/check-asserts true)

(defrecord Signal [nm trigger-listeners subscribe trigger private])

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
          trigger-ls (mk-trigger-listeners nm out-atm ls-atm)]
      (Signal.
       nm
       trigger-ls
       (mk-subscribe nm ls-atm)
       (fn [swap-fn]
         (! :fn swap-fn)
         (swap! out-atm swap-fn)
         (trigger-ls))
       {:ls-atm ls-atm
        :out-atm out-atm}))))

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
           [siga-deref nil]
           [siga-deref (second %)])) siga
      (fn [sigb-deref]
        #(if (nil? %)
           [nil sigb-deref]
           [(first %) sigb-deref])) sigb])))

(def igprod
  "does nothing when the first signal triggers"
  (fn [siga sigb]
    (let [siga-last-val (atom nil)]
      (sigfn
       (str (.nm siga) "×" (.nm sigb))
       [(fn [_ _ siga-deref]
          (reset! siga-last-val siga-deref)) siga
        (fn [listener's-out-atm listener's-trigger-listeners sigb-deref]
          (reset! listener's-out-atm [@siga-last-val sigb-deref])
          (listener's-trigger-listeners)) sigb]))))

(def target (vec "ax\" l \" #safksalj\n\"haha\"\"dontstop\" salalaks"))

(def pt
  "used to point at a position on the string"
  (mk-sig "pointer" 0))

(def view-range
  (reset-sigfn
   "view-range"
   [(fn [pt-val] [pt-val (inc pt-val)]) pt]))

(def view-aux
  (fn [[start end]]
    ((comp
      (map# second)
      (filter# (comp #(<= start % end) first))
      (partial enumerate)) target)))

(def view
  "used to view to subsequent characters in the string"
  (reset-sigfn
   "view"
   [view-aux view-range]))

(def classify-aux
  (fn [[fst snd]]
    (cond
      (= fst \") :doublequote
      (= fst \#) :octo
      (= fst \newline) :newline
      (and (= fst \') (= snd \')) :double-singlequote
      :else :irrev)))

(def classify
  (reset-sigfn
   "classify"
   [classify-aux view]))

(def ctx-of-classification
  (fn [x]
    (case x
      :doublequote :doublequote-stringlit
      :octo :comment
      :double-singlequote :double-singlequote-stringlit
      :irrev (throw (Exception. "ctx-of-classification: ctx-of-classification was given the invalid argument :irrev"))
      (throw (Exception. (str "ctx-of-classification: unknown classification: " x))))))

(def ctx-aux
  (let [init-atm (atom true)]
    (fn [[classify-val view-range-val]]
      (fn [[normal-on-next? last-special-start-pos [mode-type mode]]]
        (cond
          (and (or normal-on-next? (= :normal mode))
               (not (or (= :irrev classify-val)
                        (= :newline classify-val))))
          [false view-range-val [:start (ctx-of-classification classify-val)]]

          (and (= :comment mode) (= :newline classify-val))
          [true last-special-start-pos [mode-type mode]]

          (and (= :double-singlequote-stringlit mode)
               (= :double-singlequote classify-val)
               (= 4 (distinct (concat last-special-start-pos view-range-val))))
          [true last-special-start-pos [mode-type mode]]

          (and (= :doublequote-stringlit mode)
               (= :doublequote classify-val))
          [true last-special-start-pos [mode-type mode]]

          normal-on-next?
          [false last-special-start-pos [:start :normal]]

          @init-atm
          (do (reset! init-atm false) [false last-special-start-pos [:start :normal]])

          :else
          [normal-on-next? last-special-start-pos [:in mode]])))))

(def ctx
  "determine when a stringlit/comment context starts and when it ends"
  (swap-sigfn
   "ctx"
   [ctx-aux (igprod classify view-range)]))

(def out-aux
(fn [[[_ _ mode] pt-val]]
      #(if (nil? %)
         [[pt-val mode]]
         (conj % [pt-val mode]))))
(def out
  "assign a context to each position in the string"
  (swap-sigfn
   "out"
   [out-aux (igprod ctx pt)]))

(def run-machine
  (fn [n-steps]
    ((.trigger pt) identity)
    (mapv (fn [_] ((.trigger pt) inc)) (range n-steps))))

((comment
   ((.trigger pt) identity)
   ((.trigger pt) inc)
   (repeatedly 2 #(println "hi"))

   (.nm view)
   (get-in ctx [:private :out-atm])
   (do
     (run-machine (count target))
     (println (get-in view-range [:private :out-atm]))
     (println (get-in view [:private :out-atm]))
     (println (get-in classify [:private :out-atm]))
     (println (get-in ctx [:private :out-atm])))
   (map vector target (map second @(get-in out [:private :out-atm])))
   (get-in out [:private :out-atm])
   (take 10 (enumerate target))))
