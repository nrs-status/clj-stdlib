(ns clj-stdlib.parser
  (:require [clj-stdlib.fp :refer [p enum]]))

(def zerolit-int (int (char \0)))
(def digit (set (map char (range zerolit-int (+ 10 zerolit-int)))))
(def a-lit-int (int \a))
(def lowercase (set (map #(char (+ a-lit-int %)) (range 26))))
(def A-lit-int (int \A))
(def uppercase (set (map #(char (+ A-lit-int %)) (range 26))))
(def alpha (set (concat lowercase uppercase)))
(def alphanum (set (concat alpha digit)))
(def special-id-chars #{\- \. \_})
(def id-chars (set (concat alphanum special-id-chars)))

(def special-path-chars #{\. \/ \- \_})
(def path-chars (set (concat alphanum special-path-chars)))

(defn satisfy->char [pred]
  (fn [charseq]
    (when (pred charseq)
      (seq [(first charseq) (rest charseq)]))))

(def satisfy-of-set
  (fn [set_]
    (satisfy->char #(some? (set_ (second (first %)))))))

(defn charp [c] (satisfy->char #(= c (second (first %)))))

(defn not-charp [c] (satisfy->char #(and (not-empty %) (not= c (second (first %))))))

(def digitp (satisfy-of-set digit))

(def pathcharp  (satisfy-of-set path-chars))

(defn *> [parsera parserb]
  (fn [charseq]
    (when-let [result (parsera charseq)]
      (parserb (second result)))))

(defn <* [parsera parserb]
  (fn [charseq]
    (when-let [result (parsera charseq)]
      (when-let [resultb (parserb (second result))]
        [(first result) (second resultb)]))))

(defn <*> [parsera parserb]
  (fn [charseq]
    (when-let [resulta (parsera charseq)]
      (when-let [resultb (parserb (second resulta))]
        [[(first resulta) (first resultb)] (second resultb)]))))

(defn <? [parsera parserb]
  (fn <?1 [charseq]
    (when-let [resulta (parsera charseq)]
      (if-let [resultb (parserb (second resulta))]
        [[:both (first resulta) (first resultb)] (second resultb)]
        [[:one (first resulta)] (second resulta)]))))

(defn ?> [parsera parserb]
  (fn ?>1 [charseq]
    (if-let [resulta (parsera charseq)]
      (when-let [resultb (parserb (second resulta))]
        [[:both (first resulta) (first resultb)] (second resultb)])
      (when-let [result (parserb charseq)]
        [[:one (first result)] (second result)]))))

(defn successive-elms [coll]
  (if-let [nxt (next coll)]
    (<*> (satisfy->char #(= (first coll) (second (first %))))
         (successive-elms nxt))
    (satisfy->char #(= (first coll) (second (first %))))))

(defn successive-ps [coll]
  (if-let [nxt (next coll)]
    (<*> (first coll)
         (successive-ps nxt))
    (first coll)))

;defined for the debug version, otherwise we can just use `some-fn`
(defn <|> [parsera parserb]
  (fn [charseq]
    (if-let [r (parsera charseq)]
      r
      (when-let [r (parserb charseq)]
        r))))

(defn somep [coll]
  (if-let [nxt (next coll)]
    (<|>
     (first coll)
     (somep nxt))
    (first coll)))

(defn <$> [f parser]
  (fn [charseq]
    (when-let [[l r] (parser charseq)]
      [(f l) r])))

(def nullp (<$> (constantly nil) (successive-elms "null")))

(def truep (<$> (constantly true) (successive-elms "true")))
(def falsep (<$> (constantly false) (successive-elms "false")))

(defn between [start middle end]
  (<* (*> start middle) end))

(defn assertp [pred]
  (fn [charseq]
    (when (pred charseq)
      [nil charseq])))

(defn ensurep [parser]
  (fn [charseq]
    (when-let [r (parser charseq)]
      [nil charseq])))

(defn many [parser]
  (fn this [charseq]
    (when-let [result (parser charseq)]
      (if-let [rec (this (second result))]
        [(cons (first result) (first rec)) (second rec)]
        [[(first result)] (second result)]))))

(defn intp [charseq]
  (when-let [[l r] ((many digitp) charseq)]
    [(Integer/parseInt (apply str (map second l))) r]))

(def pathp
  (<*> (some-fn (charp \.) (charp \/)) (many pathcharp)))

(defn separated-by [target-parser separator]
  (fn separated-by1 [charseq]
    (when-let [r ((<? target-parser (many (*> separator target-parser))) charseq)]
      (case (ffirst r)
        :both [(apply cons (rest (first r))) (second r)]
        :one [(rest (first r)) (second r)]
        (throw (Exception. (str "separated-by; unexpected result for `(ffirst r)`: " (prn-str (ffirst r)))))))))

(def special-syms
  #{\« \» \' \" \/ \$ \{ \} \@ \! \? \# \% \^ \& \* \( \) \- \= \+ \. \; \: \~ \, \space \newline \< \> \[ \] \_})

(def whitespacep
  (satisfy->char #(some? (#{\space \newline} (second (first %))))))

(def spacesp
  (many whitespacep))

(defn *?> [parsera parserb]
  (fn *?>1 [charseq]
    (if-let [r (parsera charseq)]
      (parserb (second r))
      (parserb charseq))))

(def split_nm->kvmap
  (fn this [finalizer dotsplit-nm]
    {(keyword (first dotsplit-nm))
     (if-let [nxt (next dotsplit-nm)]
       (this finalizer nxt)
       finalizer)}))

(def ansi-escp
  (successive-ps [(satisfy->char #(= 27 (int (second (first %)))))
                  (charp \[)
                  (many (some-fn digitp (charp \;)))
                  (satisfy-of-set alpha)]))


