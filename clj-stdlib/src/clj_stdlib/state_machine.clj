(ns clj-stdlib.state-machine
  (:require
   [clojure.core.match :refer [match]]
   [clojure.spec.alpha :as s]
   [clj-stdlib.specs]
   [clj-stdlib.fp :refer [unwrap map# filter# enumerate conj!! bimap#]]
   [clj-stdlib.etc :refer [! split-every-n]]
   [clojure.string :as cstr]))
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

; the following looks like bad form
; (def observer
;   "discards the sig's value and uses it only for trigger timing"
;   (fn [nm on-trigger sig] (swap-sigfn nm [(fn [_] on-trigger) sig])))

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
(def svec-aux
  (fn [sigs]
    (map
     (fn [[i sig]]
       [(fn [sig-deref]
          #(if (nil? %)
             (assoc
              (vec (repeat (count sigs) nil))
              i
              sig-deref)
             (vec (assoc % i sig-deref)))) sig])
     (enumerate sigs))))

(def svec
  (fn [sigs]
    (swap-sigfn
     (str "svec of: " (clojure.string/join " " (map #(.nm %) sigs)))
     (apply concat (svec-aux sigs)))))

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

  ([x sigvar body]
   (cond
     (symbol? x)
     `(fn [sig#]
        (swap-sigfn
         ~(name x)
         [(fn ~sigvar
            ~body)
          sig#]))

     (coll? x)
     `(fn ~(vec (concat x [(symbol "sig")]))
        (swap-sigfn
         "anon"
         [(fn ~sigvar
            ~body)
          ~'sig]))))

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

  ([x sigvar body]
   (cond
     (symbol? x)
     `(fn [sig#]
        (swap-sigfn
         ~(name x)
         [(fn ~sigvar
            ~body)
          sig#]))

     (coll? x)
     `(fn ~(vec (concat x [(symbol "sig")]))
        (swap-sigfn
         "anon"
         [(fn ~sigvar
            ~body)
          ~'sig]))))
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

(defsfn evt [bool-atm] [t_]
  (if @bool-atm
    t_
    nil))

(defsfn* last-evt [nm-sig-pairs]
  (fn [prev]
    (let [who-triggered? (filter (comp not nil? second) nm-sig-pairs)]
      (cond
        (< 1 (count who-triggered?)) (throw (Exception. "last-evt: unexpected conjunction"))
        (= 1 (count who-triggered?)) (first (unwrap who-triggered?))
        :else prev))))

;parser

; (defsig pt 0)
; (def target "saS\"saa\\fa#\n'a'n\nsf\"sdaf")
;
; (defsfn view [target] [pt_]
;   (get target pt_))
;
; (defsfn* classify [target] [nsym] ;next
;   (fn [[csym ckind :as current]] ;current
;     (if (nil? current)  [(get target 0) (do (println "in first match!") (match [nsym]
;                                [(:or \" \' \#)] :start
;                                :else :no-ctx))]
;         [nsym (do (println "in second match!") (match [[csym ckind]]
;                 [(:or [\" :start] [_ :in-dq-ctx])] (match nsym
;                                                      \" :end
;                                                      \\ :in-dq-esc
;                                                      :else :in-dq-ctx)
;                 [[\\ :in-dq-esc]] :in-dq-ctx
;
;                 [[\' :start]] :start-2
;                 [[\' :start-2]] (match nsym
;                                   \' :in-dsq-sq
;                                   \\ :in-dsq-esc
;                                   :else :in-dsq-ctx)
;                 [[\\ :in-dsq-esc]] :in-dsq-ctx
;                 [[\' :in-dsq-sq]] (match nsym
;                                     \' :end
;                                     \\ :in-dsq-esc
;                                     :else :in-dsq-ctx)
;
;                 [[\# :start]] (match nsym
;                                 \newline :end
;                                 :else :in-cmt-ctx)
;
;                 [[_ (:or :end :no-ctx)]] (match nsym
;                                                \' :start
;                                                \" :start
;                                                \# :start
;                                                :else :no-ctx)))])))
;
; (defsfn* out [[classify-val pt-val]]
;   #(if (nil? %)
;      [[pt-val classify-val]]
;      (conj % [pt-val classify-val])))
;
; (def machine-as-term
;   (out (prod* (classify target (view target pt)) pt)))
;
; (comment
;   ((.trigger pt) identity)
;   (mapv (fn [_] ((.trigger pt) inc)) (range (count target)))
;   target
;   (get-in machine-as-term [:private :out-atm])
;   registry)
;
; (def filepath "/home/sieyes/baghdad_plane/flakes/frontArmToPlane/temple_artemis_ephesus/montezuma_circles_scroll/resources/plugins/pluginsCore.nix")
; (def file-as-string (slurp filepath))

; reload:
; determine current projects dependencies
; kill all repls
; lein install in correct order
; restart repl

; one clojure project can depend on another; both may be in simultaneous dev
; neovim depends on fennel

(def environments [:fennel :clj-stdlib :clj-formtools :neovim])
(def dependencies
  [:neovim [:fennel]
   :clj-formtools [:clj-stdlib :neovim]
   :clj-stdlib [:neovim]])

; what about the interaction between neovim and fennel?

(def fennel-env
  [:fennel-editor ;neovim in which we're editing fennel
   :fennel-tester ;neovim in which we're testing fennel
   ])
;an env has capabilities, a capability has a source
; :fennel-editor edits :fennel-tester's source, and then :fennel-tester as a capability changes
; therefore, a capability may be desynchronized with its source
;
; (def editor-ready? (atom false)) ; the user uses the fennel-editor until they feel they should proceed to commit this change
; (defsig committing? (rand-int 100))
; (defsig resynchronizing? (rand-int 100))
; ; foldp f synchronized last-desync last-sync
;
; (defsig t 0)
;
; (defsfn evt [bool-atm] [t_]
;   (if @bool-atm
;     t_
;     nil))
; (def desync-atm (atom false))
; (def desync-evt (evt desync-atm t))
;
; (def resync-atm (atom false))
; (def resync-evt (evt resync-atm t))
;
; (def sync?
;   (sfn* [init?] [[resync-evt desync-evt]]
;         (fn [prev]
;           (cond
;             (and resync-evt desync-evt) (throw (Exception. "unexpected conjunction"))
;             resync-evt true
;             desync-evt false
;             @init? (do (reset! init? false) true)
;             :else prev))))
;
; (def init-atm (atom true))
; (def synced? (sync? init-atm (prod resync-evt desync-evt)))
;
; (def evt1-t (atom false))
; (def evt1 (evt evt1-t t))
;
; (def evt2-t (atom false))
; (def evt2 (evt evt2-t t))
;
; (def evt3-t (atom false))
; (def evt3 (evt evt3-t t))
;
; (def x (svec-aux [evt1 evt2 evt3]))
; (def z (map #(.nm %) [evt1 evt2 evt3]))
; (def asvec (svec [evt1 evt2 evt3]))
;
; (def tagged-evt1
;   ((sfn [x] [:evt1 x]) evt1))
;
; (def tagged-evt2
;   ((sfn [x] [:evt2 x]) evt2))
;
; (def tagged-evt3
;   ((sfn [x] [:evt3 x]) evt3))
;
; (def mysvec (svec [tagged-evt1 tagged-evt2 tagged-evt3]))
; (def wholast (last-evt mysvec))
;
; (comment
;   ((.trigger t) identity)
;   (swap! desync-atm not)
;   (swap! resync-atm not)
;   (swap! evt1-t not)
;   (swap! evt2-t not)
;   (swap! evt3-t not)
;   (get-in evt3 [:private :out-atm])
;   (get-in tagged-evt3 [:private :out-atm])
;   (do
;     ((.trigger t) inc)
;     (println (map deref [evt1-t evt2-t evt3-t]))
;     (println (get-in mysvec [:private :out-atm]))
;     (println (get-in wholast [:private :out-atm])))
;   (do
;     ((.trigger t) inc)
;     (println @evt1-t)
;     (println @evt2-t)
;     (println @evt3-t)
;     (println (get-in wholast [:private :out-atm])))
;   (do
;     ((.trigger t) inc)
;     (println (get-in t [:private :out-atm]))
;     (println (get-in wholast [:private :out-atm]))
;     (println (get-in desync-evt [:private :out-atm]))
;     (println (get-in resync-evt [:private :out-atm]))
;     (println (get-in synced? [:private :out-atm]))))
;
; ;new parse alg idea
; (declare nearest)
; (def paths
;   (fn [current [dq dsq h nl :as upcoming]]
;     (case current
;       [\" _ :start] [dq :end]
;       [\' \' :start] [dsq :end]
;       [\# _ :start] [nl :end]
;       [_ _ :end] [(nearest dq dsq h) :start])))
;
; ; focusing on the fennel vs neovim loop
; (defsfn tagsig [tag] [sig]
;   [tag sig])
;
; ; (def fennel-env
; ;   [:edit-fennel
; ;    :commit-to-test-editor-src
; ;    :test])
; ;
; ;
; ; (def commit-to-test-editor-src
; ;   {:consists-of {:at-editor-env [:pre-compile-commit
; ;                                  :compile
; ;                                  :post-compile-commit
; ;                                  :push]
; ;                  :at-source [:begin-github-watcher
; ;                              :on-detect-push-flake-update
; ;                              :close-test-editor
; ;                              :start-test-editor]}})
; ;
; ; ;how do we initialize a capability?
; ; ;declare the capabilities
; ; ;while env is running: check if capability is available
; ;
; ; ;the entire thing is a daemon that wraps nix. gives a name to a shell, adds the shell to a global registry. so it must run as a service.
; ;
; ; ;the registry is out-of-machine
; ; ;there is a local instance of the registry made locally
; ; ;the external registry is synced with the local instance. 
; ;
; ; (defrecord RegistryEntry-decl [nm venvs])
; ; (defrecord Venv-decl [nm capabilities])
; ; (defrecord Venv-state [id nm status path])
; ; (defrecord Cap-decl [])
; ; (defrecord Cap-state [id nm status])
; ;
; ; (def list-venvs
; ;   (fn [])
; ;   )
; ;
; ; (def commands
; ;   :query
; ;   )
; ;
; ; (def fennel-env 
; ;   [:edit-fennel :fennel-to-lua :test])
; ;
; ;
; ; ; tfm-cap-env 
; ;
; ; ; fennel flake dir contains:
; ; ; - transformation capability source
; ; ; - fennel source dir
; ; ; - output dir
; ;
; ; ; main editor (edit-fennel capability)
; ; ; start container by running nvim from fennel flake
; ;
; ; ; test editor (test capability)
; ; ; start container by running nvim from flake
; ;
; ;
; ;
; ; ;why can we not have the transformation capability in the same directory as the fennel source? 
; ; ;  first, because it acts on the env. it is a function that takes an env. we call this function to create the contents of the output dir.
; ; ; second, because it acts on the env, it can turn a working env into a broken one, yet it must work for the test capability to work.
; ; ; the fennel-env is the "production environment", so the transformation-capability must only be accessible within it as a fully working thing
; ;
; ;
; ; (def deps
; ;   {:fennel-env [:tfm-cap-env]}
; ;   )
; ;
; ; (def tfm-fennel-to-lua-env
; ;   [{:edit [:nvim]} 
; ;    :test [[:fennel-src :editor-to-read-it]
; ;           [:fennel-env]]]
; ;   )
; ;
; ; ;the transformation: takes fennel source, transforms it into lua. but to test it, we need an editor to run the output lua
; ; ; but this means we must know how we go from some lua source to an updated working test-editor 
; ;
; ; ; push -> test-editor-env listens and updates
; ;
; ; (def test-editor-env-cmpnts
; ;   [:listener :listener-on-trigger]
; ;   )
; ;
; ; (def test-editor-env-listener
; ;   [:flake-url :diff-every-x-secs [:on-diff :trigger] [:trigger-is :flake-update] [:reset-proc]]
; ;   )
; ;
; ; ;def x, take two git revs and produce what??
; ; (declare run)
; ; (def listener-sigfn
; ;   (fn [remote-rev]
; ;     (fn [local-env] 
; ;       (let [local-rev (run :get-local-rev local-env)] 
; ;         (when (not (= local-rev remote-rev))
; ;           (run :update local-env)
; ;           ))
; ;     )))
; ; (def listener-sigfn
; ;   (fn [remote-rev local-rev]
; ;     (= remote-rev local-rev)))
; ; (declare remote-rev local-rev)
; ;
; ; (def sync? (listener-sigfn remote-rev local-rev))
; ;
; ; (def env
; ;   {
; ;    :datafiles [:editor-src :clojure-src :flake-lock] ;clojure src to test the editor with
; ;    :procs [:nvim]
; ;    }
; ;   )
; ;
; ; (def local-rev
; ;   (run :get-local-rev)
; ;   )
; ; ; sync? -> resync
; ;
; ; (declare f g h)
; ; (f sync? env) ; this must produce a new env
; ;
; ; (def provide-env' (f sync? env))
; ;
; ; (def provide-env
; ;   (fn [env-decl sync?]
; ;     (cond
; ;       (not sync?) :update!
; ;       :else :throw!
; ; )))
; ;
; ; (f :remote-lua-src)
; ;
; ; ; capability: availability predicate
; ; (def nvim-availability-predicate :nvim-is-running)
; ;
; ;
; ; (def provide-fs (f :open-conf-constant :potentially-remote-lua-src-signal)) ; returns fs location
; ; (def provide-nvim (f provide-fs)) ;takes a fs, provides a process
; ; (def provide-nvim-result
; ;   [:proc-id :proc-state]
; ;   )
; ;
;
;
;
;
;
