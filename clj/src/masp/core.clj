(ns masp.core
  (:refer-clojure :exclude [read-string eval])
  (:require [masp.value :refer [mfn mfn* inject-bool]]
            [masp.read :as r]
            [masp.eval :as e]
            [masp.env :as env]
            [masp.cont :as k])
  (:import [masp.value Tagpair Ignore
                       PrimOp Applicative Continuation CompoundOp]
           [masp.env SDAEnv]))

;;; FIXME: eval(*) should be [Env Expr] -> Any instead of [Expr Env] -> Any
;;; TODO: use CPS to handle #@ and #% errors, i.e. (#%iadd a b (@fn err ...))

(def default-bindings
  {(symbol "#@op") (PrimOp.
                     (fn [[name formal eformal & body] env cont]
                       [:continue (CompoundOp. name formal eformal body env)
                                  nil nil cont]))

   (symbol "#%wrap") (mfn [f] (Applicative. f))
   (symbol "#%unwrap") (mfn [f] (.op f))
   (symbol "#%eval") (mfn* [[expr env] _ cont] [:eval expr nil env cont])
   (symbol "#%apply") (mfn* [[f args] env cont] [:combine f args env cont])
   (symbol "#%cont?") (mfn [v] (inject-bool (instance? Continuation v)))
   (symbol "#%call/cc") (mfn* [[f] env cont]
                          (let [k (Continuation. cont)]
                            [:combine f [k] env cont]))
   (symbol "#%throw") (mfn* [[k v] _ _] [:continue v nil nil (.cont k)])
   (symbol "#%cont/env") (mfn* [[value env*] _ cont]
                           (let [[label env stmts] (k/peek-frame cont)]
                             (if (and (= label :stmt) (env/scope= env env*))
                               (let [cont*
                                     (k/replace-frame cont [:stmt env* stmts])]
                                 [:continue value nil nil cont*])
                               [:err [:scope label env env*]])))
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
                         Continuation :continuation

                         SDAEnv :environment))

   (symbol "#%extend") (mfn [env k v] (env/extend env k v))
   (symbol "#%reserve") (mfn [env k] (env/reserve env k))
   (symbol "#%claim!") (mfn* [[env k v] _ cont]
                         (if-let [env* (env/claim! env k v)]
                           [:continue env nil nil cont]
                           [:err [:unsettable k]]))

   (symbol "#%head") (mfn [[x]] x)
   (symbol "#%tail") (mfn [[_ & xs]] xs)
   (symbol "#%cons") (mfn [x xs] (conj xs x))

   (symbol "#%tnew") (mfn [t v] (Tagpair. t v))
   (symbol "#%ttag") (mfn [tv] (.tag tv))
   (symbol "#%tval") (mfn [tv] (.value tv))})

(def read-string r/read-string)

(defn eval
  ([ctrl]
    (eval ctrl (env/environment default-bindings)))
  ([ctrl env]
    (e/eval ctrl env (k/cont-stack))))
