(ns clj-stdlib.parser-debug
  (:require
   [clojure.string :as cljstr]
   [clj-stdlib.fp :refer [p enum conj!!]]
   [clj-stdlib.parser :as p]))

(def log (atom []))

(def id-atm (atom 0))
(def id! #(let [atm-deref @id-atm]
            (swap! id-atm inc)
            atm-deref))

(def reset-logs!
  #(do
     (reset! log [])
     (reset! id-atm 0)))

(defrecord parser [nm whenfn])

(def run
  (fn [parser target]
    (let [i (id!)]
      (conj!! log [i :attempt parser :target target])
      (let [r ((.whenfn parser) target)]
        (conj!! log [i :result (.nm parser) r])
        r
        ))))

(defn l<$> [f parser]
  (->parser (f (.nm parser)) (.whenfn parser)))

(defn r<$> [f parser]
  (->parser
   (.nm parser)
   (fn [charseq]
     (when-let [[l r] (parser charseq)]
       [(f l) r]))))

(defn bi<$> [f g parser]
  (l<$> f (r<$> g parser)))

(defn charp [c]
  (->parser (str "charp " c) (p/charp c)))

(defn not-charp [c]
  (->parser (str "not-charp " c) (p/not-charp [c])))

(def digitp
  (->parser "digitp" p/digitp))

(def pathcharp
  (->parser "pathcharp" p/pathcharp))

(def nullp
  (->parser "nullp" p/nullp))

(def truep
  (->parser "truep" p/truep))

(def falsep
  (->parser "falsep" p/falsep))

(def ansi-escp
  (->parser "ansi-escp" p/ansi-escp))

(def spacesp
  (->parser "spacesp" p/spacesp))

(def intp
  (->parser "intp" p/intp))

(def lr-renaming
  (fn [new-parser-nm new-parser-whenfn parsera parserb]
    (->parser
     (str "(" new-parser-nm " " (.nm parsera) " " (.nm parserb) ")")
     (new-parser-whenfn
      (p run
         (l<$> (fn [nm] (str "(" new-parser-nm " left: " nm ")")) parsera))
      (p run
         (l<$> (fn [nm] (str "(" new-parser-nm " right: " nm ")")) parserb))))))

(defn *> [parsera parserb]
  (lr-renaming "*>" p/*> parsera parserb))

(defn <* [parsera parserb]
  (lr-renaming "<*" p/<* parsera parserb))

(defn <*> [parsera parserb]
  (lr-renaming "<*>" p/<*> parsera parserb))

(defn <? [parsera parserb]
  (lr-renaming "<?" p/<? parsera parserb))

(defn ?> [parsera parserb]
  (lr-renaming "?>" p/?> parsera parserb))

(defn *?> [parsera parserb]
  (lr-renaming "*?>" p/*?> parsera parserb))

(defn <|> [parsera parserb]
  (lr-renaming "<|>" p/<|> parsera parserb))

(defn successive-elms [coll]
  (->parser
   (str "successive-elms " (prn-str coll))
   (p/successive-elms coll)))

(defn successive-ps-aux [coll]
  (if-let [nxt (next coll)]
    (<*> (first coll)
         (successive-ps-aux nxt))
    (first coll)))

(defn successive-ps [coll]
  (->parser
   (cljstr/join " " (cons "(successive-ps " (conj (vec (map #(.nm %) coll)) ")")))
   (p run (successive-ps-aux (map #(l<$> (fn [nm] (str "(in successive-ps: " nm ")")) %) coll)))))

(defn somep-aux [coll]
  (if-let [nxt (next coll)]
    (<|> (first coll)
         (somep-aux nxt))
    (first coll)))

(defn somep [coll]
  (->parser
   (cljstr/join " " (cons "(somep " (conj (vec (map #(.nm %) coll)) ")")))
   (p run (somep-aux (map #(l<$> (fn [nm] (str "(in somep: " nm ")")) %) coll)))))

(defn between [start middle end]
  (let [start' (l<$> (fn [nm] (str "(between start: " nm ")")) start)
        middle' (l<$> (fn [nm] (str "(between middle: " nm ")")) middle)
        end' (l<$> (fn [nm] (str "(between end: " nm ")")) end)]
    (->parser
     (str "(between; " (.nm start) "; " (.nm middle) "; " (.nm end) ")")
     (p run (<* (*> start' middle') end')))))

(defn many-aux [parser]
  (fn this [charseq]
    (when-let [[l r] (run (l<$> (fn [nm] (str "(within many: " nm ")")) parser) charseq)]
      (if-let [_ (run (l<$> (fn [nm] (str "(within many; testing and discarding: " nm ")")) parser) r)]
        (let [[l' r'] (this r)]
          [(into [l] l') r'])
        [l r]))))

(defn many [parser]
  (->parser
   (str "(many " (.nm parser) ")")
   (many-aux parser)))

(defn separated-by [separee separator]
  (let [separee' (l<$> #(str "(separated-by separee: " % ")") separee)
        separator' (l<$> #(str "(separated-by separator: " % ")") separator)]
    (l<$> #(str "(separated-by: " % ")") (<*> (many (<* separee' separator')) separee'))))


(comment
  (def mpa (*> (charp \a) (charp \b)))
  (def mpb (between (charp \a) (charp \b) (charp \c)))
  (def mpc (successive-ps [(charp \a) (charp \b) (charp \c)]))
  (def mpd (some-fn digitp (charp \;)))
  (def mpe (many (somep [spacesp (charp \=)])))
  ; (def mpc (
  (def x (run mpa (enum "ab")))
  (def xa (run mpb (enum "abc")))
  (def xc (run mpc (enum "abc")))
  (def xd (run mpd (enum "7")))
  (def xe (run mpe (enum " = 8")))
  ; (def xb (run _ _))
  (reset-logs!)
  (sort-by first @log))
