(ns masp.eval.vecstack-trampoline
  (:refer-clojure :exclude [eval]))

(declare -eval -continue -apply default-env)

(defprotocol Operative
  (operate [op operand dyn-env cont]))

(deftype PrimOp [f]
  Operative
  (operate [_ operand dyn-env cont]
    (f operand dyn-env cont)))

(deftype CompoundOp [name formal eformal body lex-env]
  Operative
  (operate [op operand dyn-env cont]
    (let [env (-> lex-env
                  (assoc name op)
                  (assoc formal operand)
                  (assoc eformal dyn-env))]
      #(-eval body env cont))))

(deftype Applicative [op]
  Operative
  (operate [_ operand dyn-env cont]
    (if (seq operand)
      (let [[ctrl & operands] operand
            cont* (into cont [dyn-env op operands 0 :arg])]
        #(-eval ctrl dyn-env cont*))
      #(-apply op operand dyn-env cont))))

(defmacro mfn [formals body]
  (let [cont (gensym 'cont), ignore (gensym)]
    `(Applicative. (PrimOp. (fn [~formals ~ignore ~cont]
                              #(-continue ~body ~cont))))))

(defn -eval [ctrl env cont]
  (cond
    (and (seq? ctrl) (seq ctrl))
    (let [[op & operand] ctrl
          cont* (into cont [operand env :op])]
      #(-eval op env cont*))

    (symbol? ctrl)
    (if-let [value (get env ctrl)]
      #(-continue value cont)
      [:err [:unbound ctrl]])

    :else
    #(-continue ctrl cont)))

(defn -continue [value cont]
  (case (peek cont)
    :op
    (let [[operand env _] (take-last 3 cont)
          cont* (subvec cont 0 (- (count cont) 3))]
      #(-apply value operand env cont*))

    :arg
    (let [[n _] (take-last 2 cont)
          [env op operands] (take-last 3 (drop-last (+ n 2) cont))]
      (if (seq operands)
        (let [[ctrl & operands*] operands
              cont* (-> cont
                        (assoc (- (count cont) 2 n 1) operands*)
                        (assoc (- (count cont) 2) value)
                        (assoc (- (count cont) 1) (inc n))
                        (conj :arg))]
          #(-eval ctrl env cont*))
        (let [args (take-last n (drop-last 2 cont))
              arg (reverse (cons value args))
              cont* (subvec cont 0 (- (count cont) 2 n 3))]
          #(-apply op arg env cont*))))

    :halt
    [:ok value]))

(defn -apply [op operand env cont]
  #(operate op operand env cont))

(defn eval
  ([ctrl]
    (eval ctrl default-env))
  ([ctrl env]
    (eval ctrl env [:halt]))
  ([ctrl env cont]
    (trampoline -eval ctrl env cont)))

(defn op [[name formal eformal body] env cont]
  #(-continue (CompoundOp. name formal eformal body env) cont))

(def default-env
  {(symbol "##sf#op") (PrimOp. op)

   (symbol "##intr#head") (mfn [[x]] x)
   (symbol "##intr#tail") (mfn [[_ & xs]] xs)
   (symbol "##intr#cons") (mfn [x xs] (cons x xs))})
