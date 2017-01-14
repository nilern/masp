(ns masp.read
  (:refer-clojure :exclude [read-string])
  (:require [masp.value :refer [ignore mfn inject-bool]]
            [instaparse.core :as insta])
  (:import masp.value.Tagpair))

(def parser
  (insta/parser (clojure.java.io/resource "masp.ebnf")))

(def ptree->sexpr
  (partial insta/transform
    {:program #(list (list* (symbol "#@op") ignore ignore ignore
                            (apply list %&)))
     :expr identity
     :list #(apply list %&)
     :boolean #(inject-bool (case % "#t" true "#f" false))
     :number #(Integer/parseInt %)
     :ignore (constantly ignore)
     :keyword keyword
     :symbol symbol}))

(def read-string
  (comp ptree->sexpr parser))
