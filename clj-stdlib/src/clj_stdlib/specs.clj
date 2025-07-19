(ns clj-stdlib.specs
  (:require [clojure.spec.alpha :as s]))

(s/def :specs/str string?)
(s/def :specs/atom (partial instance? clojure.lang.Atom))
(s/def :specs/kw keyword?)
(s/def :specs/bool boolean?)

(s/def :specs/notfn #(not (fn? %)))
(s/def :specs/fn fn?)


(s/def :specs/map map?)
(s/def :specs/vec vector?)
(s/def :specs/notcoll #(not (coll? %)))
(s/def :specs/coll coll?)

(s/def :specs/w-sig-itf (s/keys :req-un [::get ::subscribe]))
(s/def :specs/w-token-itf (s/keys :req-un [::token ::expiration-dt ::validation-dt]))

(s/def :specs/int int?)
(s/def :specs/num number?)
(s/def :specs/even (s/and int? even?))

(s/def :specs/known-view-style-kw
  #{:height :width
    :justifyContent :alignItems
    :backgroundColor
    :flex :flexDirection
    :padding :paddingHorizontal :paddingVertical :paddingTop :paddingBottom :paddingRight :paddingLeft
    :margin :marginHorizontal :marginVertical :marginTop :marginBottom :marginRight :marginLeft

    :borderRadius
    :borderWidth :borderBottomWidth
    :borderColor})

(s/def :specs/know-text-style-kw
  #{:color
    :fontFamily
    :fontSize
    :fontStyle
    :fontWeight})
