(ns clj-stdlib.parser-debug
  (:require
   [clojure.string :as cljstr]
   [clj-stdlib.fp :refer [p enum conj!!]]
   [clj-stdlib.parser :as p]))

(def log (atom []))

(def id-atm (atom 0))

(def reset-logs!
  #(do
     (reset! log [])
     (reset! id-atm 0)))

(defrecord parser [path nm whenfn])

(def run
  (fn [pathed-parser target]
    (let [r ((.whenfn pathed-parser) target)]
      (conj!! log [pathed-parser r])
      r)))

(def start 
  (fn [unpathed-parser target]
    (run (unpathed-parser [0]) target)))

(defn nm<$> [f parser]
  (->parser (.path parser) (f (.nm parser)) (.whenfn parser)))

(defn fn<$> [f parser]
  (->parser
   (.path parser)
   (.nm parser)
   (fn [charseq]
     (when-let [[l r] (parser charseq)]
       [(f l) r]))))

(defn path<$> [f parser]
  (->parser
   (f (.path parser))
   (.nm parser)
   (.whenfn parser)))

(defn charp [c]
  (fn [path]
    (->parser path (str "charp " c) (p/charp c))))

(defn not-charp [c]
  (fn [path]
    (->parser path (str "not-charp " c) (p/not-charp [c]))))

(def digitp
  (fn [path]
    (->parser path "digitp" p/digitp)))

(def pathcharp
  (fn [path]
    (->parser path "pathcharp" p/pathcharp)))

(def nullp
  (fn [path]
    (->parser path "nullp" p/nullp)))

(def truep
  (fn [path]
    (->parser path "truep" p/truep)))

(def falsep
  (fn [path]
    (->parser path "falsep" p/falsep)))

(def ansi-escp
  (fn [path]
    (->parser path "ansi-escp" p/ansi-escp)))

(def spacesp
  (fn [path]
    (->parser path "spacesp" p/spacesp)))

(def intp
  (fn [path]
    (->parser path "intp" p/intp)))

(def lr-renaming
  (fn [new-parser-nm new-parser-whenfn pathless-parsera pathless-parserb]
    (fn [path]
      (let [parsera (pathless-parsera (conj path 0))
            parserb (pathless-parserb (conj path 1))]
        (->parser
         path
         (str "(" new-parser-nm " " (.nm parsera) " " (.nm parserb) ")")
         (new-parser-whenfn
          (p run
             (nm<$> (fn [nm] (str "(" new-parser-nm " left: " nm ")")) parsera))
          (p run
             (nm<$> (fn [nm] (str "(" new-parser-nm " right: " nm ")")) parserb))))))))

(defn *> [unpathed-parsera unpathed-parserb]
  (lr-renaming "*>" p/*> unpathed-parsera unpathed-parserb))

(defn <* [unpathed-parsera unpathed-parserb]
  (lr-renaming "<*" p/<* unpathed-parsera unpathed-parserb))

(defn <*> [unpathed-parsera unpathed-parserb]
  (lr-renaming "<*>" p/<*> unpathed-parsera unpathed-parserb))

(defn <? [unpathed-parsera unpathed-parserb]
  (lr-renaming "<?" p/<? unpathed-parsera unpathed-parserb))

(defn ?> [unpathed-parsera unpathed-parserb]
  (lr-renaming "?>" p/?> unpathed-parsera unpathed-parserb))

(defn *?> [unpathed-parsera unpathed-parserb]
  (lr-renaming "*?>" p/*?> unpathed-parsera unpathed-parserb))

(defn <|> [unpathed-parsera unpathed-parserb]
  (lr-renaming "<|>" p/<|> unpathed-parsera unpathed-parserb))

(defn successive-elms [coll]
  (fn [path]
    (->parser
     path
     (str "successive-elms " (prn-str coll))
     (p/successive-elms coll))))

(defn successive-ps-aux [pathed-parsers]
  (if-let [nxt (next pathed-parsers)]
    (<*> (first pathed-parsers)
         (successive-ps-aux nxt))
    (first pathed-parsers)))

(defn successive-ps [unpathed-parsers]
  (fn [path]
    (let [pathed-parsers (map (fn [[i x]] (x (conj path i))) (enum unpathed-parsers))]
      (->parser
       path
       (cljstr/join " " (cons "(successive-ps " (conj (vec (map #(.nm %) pathed-parsers)) ")")))
       (p run (successive-ps-aux (map #(nm<$> (fn [nm] (str "(in successive-ps: " nm ")")) %) pathed-parsers)))))))

(defn somep-aux [pathed-parsers]
  (if-let [nxt (next pathed-parsers)]
    (<|> (first pathed-parsers)
         (somep-aux nxt))
    (first pathed-parsers)))

(defn somep [unpathed-parsers]
  (fn [path]
    (let [pathed-parsers (map (fn [[i x]] (x (conj path i))) (enum unpathed-parsers))]
      (->parser
       path
       (cljstr/join " " (cons "(somep " (conj (vec (map #(.nm %) pathed-parsers)) ")")))
       (p run (somep-aux (map #(nm<$> (fn [nm] (str "(in somep: " nm ")")) %) pathed-parsers)))))))

(defn between [unpathed-start unpathed-middle unpathed-end]
  (fn [path]
    (let [pathed-start (unpathed-start (conj path 0))
          pathed-middle (unpathed-middle (conj path 1))
          pathed-end (unpathed-end (conj path 2))]
      (->parser
       path
       (str "(between; " (.nm pathed-start) "; " (.nm pathed-middle) "; " (.nm pathed-end) ")")
       (p run ((<* (*> unpathed-start unpathed-middle) unpathed-end) (conj path 0)))))))


(defn many-aux [unpathed-parser]
  (fn this [path charseq]
    (when-let [[l r] (run (nm<$> (fn [nm] (str "(within many: " nm ")")) (path unpathed-parser)) charseq)]
      (let [next-path (conj (drop-last path) (inc (last path)))]
      (if-let [_ (run (nm<$> (fn [nm] (str "(within many; testing and discarding: " nm ")")) (unpathed-parser next-path)) r)]
        (let [[l' r'] (this next-path r)]
          [(into [l] l') r'])
        [l r])))))

(defn many [unpathed-parser]
  (fn [path]
  (->parser
    path
   (str "(many " (.nm (unpathed-parser path)) ")")
   (p (many-aux unpathed-parser) path))))

(defn separated-by [unpathed-separee unpathed-separator]
  (fn [path]
    (nm<$> #(str "(separated-by: " % ")") ((<*> (many (<* unpathed-separee unpathed-separator)) unpathed-separee) path))))

(def logs-at-path
  (fn [path logs]
    (filter #(= path (:path (first %))) logs)))

(def logs-from-path
  (fn [path logs]
    (filter #(= path (take (count path) (:path (first %)))) logs)))

(def immediate-children-logs-of-path
  (fn [path logs]
    (filter #(= (inc (count path)) (count (:path (first %)))) (logs-from-path path logs))))

(comment
  (def mpa (*> (charp \a) (charp \b)))
  (def mpb (between (charp \a) (charp \b) (charp \c)))
  (def mpc (successive-ps [(charp \a) (charp \b) (charp \c)]))
  (def mpd (some-fn digitp (charp \;)))
  (def mpe (many (somep [spacesp (charp \=)])))
  (logs-from-path [0 0] @log)
  (immediate-children-logs-of-path [0 0] @log)
  ; (def mpc (
  (def x (start mpa (enum "ab")))
  (def xa (start mpb (enum "abc")))
  (def xc (start mpc (enum "abc")))
  (def xd (start mpd (enum "7")))
  (def xe (start mpe (enum " = 8")))
  ; (def xb (run _ _))
  (reset-logs!)
  (sort-by first @log))

