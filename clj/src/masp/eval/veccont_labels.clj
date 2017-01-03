(ns masp.eval.veccont-labels
  (:refer-clojure :exclude [eval]))

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
      [:eval body env cont])))

(deftype Applicative [op]
  Operative
  (operate [_ operand dyn-env cont]
    (if (seq operand)
      (let [[ctrl & operands] operand
            cont* (into cont [dyn-env op operands 0 :arg])]
        [:eval ctrl dyn-env cont*])
      [:apply op operand dyn-env cont])))

(defmacro mfn [formals body]
  (let [cont (gensym 'cont), ignore (gensym)]
    `(Applicative. (PrimOp. (fn [~formals ~ignore ~cont]
                              [:continue ~body ~cont])))))

(declare default-env)

(defn eval
  ([ctrl]
    (eval ctrl default-env))
  ([ctrl env]
    (eval ctrl env [:halt]))
  ([ctrl env cont]
    (loop [state [:eval ctrl env cont]]
      (case (first state)
        :eval
        (let [[_ ctrl env cont] state]
          (cond
            (and (seq? ctrl) (seq ctrl))
            (let [[op & operand] ctrl
                  cont* (into cont [operand env :op])]
              (recur [:eval op env cont*]))

            (symbol? ctrl)
            (if-let [value (get env ctrl)]
              (recur [:continue value cont])
              [:err [:unbound ctrl]])

            :else
            (recur [:continue ctrl cont])))

        :continue
        (let [[_ value cont] state]
          (recur [(peek cont) value cont]))

        :op
        (let [[_ value cont] state
              [operand env _] (take-last 3 cont)
              cont* (subvec cont 0 (- (count cont) 3))]
          (recur [:apply value operand env cont*]))

        :arg
        (let [[_ value cont] state
              [n _] (take-last 2 cont)
              [env op operands] (take-last 3 (drop-last (+ n 2) cont))]
          (if (seq operands)
            (let [[ctrl & operands*] operands
                  cont* (-> cont
                            (assoc (- (count cont) 2 n 1) operands*)
                            (assoc (- (count cont) 2) value)
                            (assoc (- (count cont) 1) (inc n))
                            (conj :arg))]
              (recur [:eval ctrl env cont*]))
            (let [args (take-last n (drop-last 2 cont))
                  arg (reverse (cons value args))
                  cont* (subvec cont 0 (- (count cont) 2 n 3))]
              (recur [:apply op arg env cont*]))))

        :halt
        (let [[_ value] state]
          [:ok value])

        :apply
        (let [[_ op operand env cont] state]
          (recur (operate op operand env cont)))))))

(defn op [[name formal eformal body] env cont]
  [:continue (CompoundOp. name formal eformal body env) cont])

(def default-env
  {(symbol "##sf#op") (PrimOp. op)

   (symbol "##intr#head") (mfn [[x]] x)
   (symbol "##intr#tail") (mfn [[_ & xs]] xs)
   (symbol "##intr#cons") (mfn [x xs] (cons x xs))})
