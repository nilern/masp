(ns masp.value)

(deftype Tagpair [tag value])

(deftype PrimOp [f])

(deftype CompoundOp [name formal eformal body lex-env])

(deftype Applicative [op])

(deftype Continuation [cont])

(defmacro mfn* [formals & body]
  `(Applicative. (PrimOp. (fn ~formals ~@body))))

(defmacro mfn [formals & body]
  (let [cont (gensym 'cont), ignore (gensym)]
    `(mfn* [~formals ~ignore ~cont] [:continue (do ~@body) nil nil ~cont])))

(deftype Ignore [])

(def ignore (Ignore.))

(defn inject-bool [v]
  (if v
    (Tagpair. :true (mfn [x _] x))
    (Tagpair. :false (mfn [_ x] x))))
