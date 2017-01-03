(ns masp.eval.framelist
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
      [:eval (first operand) dyn-env
       (cons [:arg dyn-env op '() (rest operand)] cont)]
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
    (eval ctrl env (list [:halt])))
  ([ctrl env cont]
    (loop [state [:eval ctrl env cont]]
      (case (first state)
        :eval
        (let [[_ ctrl env cont] state]
          (cond
            (and (seq? ctrl) (seq ctrl))
            (recur [:eval (first ctrl) env (cons [:op env (rest ctrl)] cont)])

            (symbol? ctrl)
            (if-let [value (get env ctrl)]
              (recur [:continue value cont])
              [:err [:unbound ctrl]])

            :else
            (recur [:continue ctrl cont])))

        :continue
        (let [[_ value cont] state]
          (case (ffirst cont)
            :arg
            (let [[_ env op args operands] (first cont)]
              (if (seq operands)
                (recur
                  [:eval (first operands) env
                   (cons [:arg env op (cons value args) (rest operands)]
                         (rest cont))])
                (recur
                  [:apply op (reverse (cons value args)) env (rest cont)])))

            :op
            (let [[_ env operand] (first cont)]
              (recur [:apply value operand env (rest cont)]))

            :halt
            [:ok value]))

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
