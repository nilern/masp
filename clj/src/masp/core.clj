(ns masp.core
  (:refer-clojure :exclude [read-string eval])
  (:require [masp.value :refer [mfn mfn*]]
            [masp.read :as r]
            [masp.eval :as e])
  (:import [masp.value Tagpair Ignore
                       PrimOp Applicative Continuation CompoundOp]))

(defn- op [[name formal eformal body] env cont]
  [:continue (CompoundOp. name formal eformal body env) nil nil cont])

(defn- begin [stmts env cont]
  (if (seq stmts)
    (let [[stmt & stmts*] stmts
          cont* (into cont [env stmts* :stmt])]
      [:eval stmt nil env cont*])
    [:continue nil nil nil cont]))

(def default-env
  {(symbol "#@op") (PrimOp. op)
   (symbol "#@begin") (PrimOp. begin)

   (symbol "#%wrap") (mfn [f] (Applicative. f))
   (symbol "#%unwrap") (mfn [f] (.op f))
   (symbol "#%eval") (mfn* [[expr env] _ cont] [:eval expr nil env cont])
   (symbol "#%apply") (mfn* [[f args] env cont] [:apply f args env cont])
   (symbol "#%call/cc") (mfn* [[f] env cont]
                          (let [k (Applicative. (Continuation. cont))]
                            [:apply f k env cont]))
   (symbol "#%cont/env") (mfn* [[value env] _ cont]
                           [:continue-env value nil env cont])
   (symbol "#%err") (mfn* [[value] _ cont]
                      [:continue value nil nil [:err]])

   (symbol "#%type") (mfn [val]
                       (condp instance? val
                         java.lang.Long :int
                         clojure.lang.Keyword :keyword
                         clojure.lang.Symbol :symbol
                         Ignore :ignore

                         clojure.lang.IPersistentList :list
                         clojure.lang.IPersistentMap :map
                         clojure.lang.IPersistentVector :vector
                         Tagpair (.tag val)

                         PrimOp :operative
                         CompoundOp :operative
                         Applicative :applicative
                         Continuation :continuation))

   (symbol "#%assoc") (mfn [coll k v] (assoc coll k v))
   (symbol "#%assoc!") (mfn [coll k v]
                         (update coll k
                           (fn [ov]
                             (if (and (instance? clojure.lang.Atom ov)
                                      (= @ov nil))
                               (do (reset! ov v) ov)
                               v))))
   (symbol "#%reserve") (mfn [coll k] (assoc coll k (atom nil)))

   (symbol "#%head") (mfn [[x]] x)
   (symbol "#%tail") (mfn [[_ & xs]] xs)
   (symbol "#%cons") (mfn [x xs] (conj xs x))

   (symbol "#%tnew") (mfn [t v] (Tagpair. t v))
   (symbol "#%ttag") (mfn [tv] (.tag tv))
   (symbol "#%tval") (mfn [tv] (.value tv))})

(def read-string r/read-string)

(defn eval
  ([ctrl]
    (eval ctrl default-env))
  ([ctrl env]
    (e/eval ctrl env [:halt])))
