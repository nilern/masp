(ns masp.value)

(defprotocol Operative
  (operate [op operand dyn-env cont]))

(deftype Tagpair [tag value])

(deftype PrimOp [f]
  Operative
  (operate [_ operand dyn-env cont]
    (f operand dyn-env cont)))

(deftype CompoundOp [name formal eformal body lex-env]
  Operative
  (operate [op operand dyn-env cont]
    (let [assoc-symbol (fn [env k v] (if (symbol? k) (assoc env k v) env))
          env (-> lex-env
                  (assoc-symbol name op)
                  (assoc-symbol formal operand)
                  (assoc-symbol eformal dyn-env))]
      [:eval body env cont])))

(deftype Applicative [op]
  Operative
  (operate [_ operand dyn-env cont]
    (if (seq operand)
      (let [[ctrl & operands] operand
            cont* (into cont [dyn-env op operands 0 :arg])]
        [:eval ctrl dyn-env cont*])
      [:apply op operand dyn-env cont])))

(deftype Continuation [cont]
  Operative
  (operate [_ operand _ _]
    [:continue operand cont]))

(defmacro mfn* [formals & body]
  `(Applicative. (PrimOp. (fn ~formals ~@body))))

(defmacro mfn [formals & body]
  (let [cont (gensym 'cont), ignore (gensym)]
    `(mfn* [~formals ~ignore ~cont] [:continue (do ~@body) ~cont])))

(deftype Ignore [])

(def ignore (Ignore.))
