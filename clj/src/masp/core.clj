(ns masp.core
  (:refer-clojure :exclude [read-string eval])
  (:require [masp.read.sexpr :as r]
            [masp.eval.veccont-labels :as e]))

(def read-string r/read-string)

(def eval e/eval)
