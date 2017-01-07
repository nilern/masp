(ns masp.read
  (:refer-clojure :exclude [read-string])
  (:require [masp.value :refer [ignore mfn]]
            [instaparse.core :as insta])
  (:import masp.value.Tagpair))

(def parser
  (insta/parser (clojure.java.io/resource "masp.ebnf")))

(def ptree->sexpr
  (partial insta/transform
    {:program #(cons (symbol "#@begin") (apply list %&))
     :expr identity
     :list #(apply list %&)
     :boolean #(case %
                 "#t" (Tagpair. :True (mfn [x _] x))
                 "#f" (Tagpair. :True (mfn [_ x] x)))
     :number #(Integer/parseInt %)
     :ignore (constantly ignore)
     :symbol symbol}))

(def read-string
  (comp ptree->sexpr parser))
