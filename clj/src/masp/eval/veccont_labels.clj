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

(deftype Continuation [cont]
  Operative
  (operate [_ operand _ _]
    [:continue operand cont]))

(defmacro mfn* [formals & body]
  `(Applicative. (PrimOp. (fn ~formals ~@body))))

(defmacro mfn [formals & body]
  (let [cont (gensym 'cont), ignore (gensym)]
    `(mfn* [~formals ~ignore ~cont] [:continue (do ~@body) ~cont])))

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

        :continue-env
        ;; TODO: fail if (not= (peek cont) :stmt)
        ;; OR?: always have an env at (get cont (- (count cont) 2))
        (let [[_ value env cont] state
              cont* (assoc cont (- (count cont) 3) env)]
          (recur [:continue value cont*]))

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
            (let [args (subvec cont (- (count cont) n 2) (- (count cont) 2))
                  arg (conj args value)
                  cont* (subvec cont 0 (- (count cont) 2 n 3))]
              (recur [:apply op arg env cont*]))))

        :stmt
        (let [[_ value cont] state
              [env stmts _] (take-last 3 cont)]
          (if (seq stmts)
            (let [[stmt & stmts*] stmts
                  cont* (assoc cont (- (count cont) 2) stmts*)]
              (recur [:eval stmt env cont*]))
            (let [cont* (subvec cont 0 (- (count cont) 3))]
              (recur [:continue value cont*]))))

        :halt
        (let [[_ value _] state]
          [:ok value])

        :apply
        (let [[_ op operand env cont] state]
          (recur (operate op operand env cont)))))))

(defn op [[name formal eformal body] env cont]
  [:continue (CompoundOp. name formal eformal body env) cont])

(defn begin [stmts env cont]
  (if (seq stmts)
    (let [[stmt & stmts*] stmts
          cont* (into cont [env stmts* :stmt])]
      [:eval stmt env cont*])
    [:continue nil cont]))

(def default-env
  {(symbol "#@op") (PrimOp. op)
   (symbol "#@begin") (PrimOp. begin)

   (symbol "#%wrap") (mfn [f] (Applicative. f))
   (symbol "#%unwrap") (mfn [f] (.op f))
   (symbol "#%eval") (mfn* [[expr env] _ cont] [:eval expr env cont])
   (symbol "#%apply") (mfn* [[f args] env cont] [:apply f args env cont])
   (symbol "#%call/cc") (mfn* [[f] env cont]
                          (let [k (Applicative. (Continuation. cont))]
                            [:apply f k env cont]))
   (symbol "#%cont/env") (mfn* [[value env] _ cont]
                           [:continue-env value env cont])

   (symbol "#%assoc") (mfn [coll k v] (assoc coll k v))

   (symbol "#%head") (mfn [[x]] x)
   (symbol "#%tail") (mfn [[_ & xs]] xs)
   (symbol "#%cons") (mfn [x xs] (cons x xs))})
