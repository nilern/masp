(ns masp.read.sexpr
  (:refer-clojure :exclude [read-string])
  (:require [instaparse.core :as insta]))

(def parser
  (insta/parser (clojure.java.io/resource "masp.ebnf")))

(def ptree->sexpr
  (partial insta/transform
    {:expr identity
     :list #(apply list %&)
     :number #(Integer/parseInt %)
     :symbol symbol}))

(def read-string
  (comp ptree->sexpr parser))
