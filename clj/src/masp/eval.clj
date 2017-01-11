(ns masp.eval
  (:refer-clojure :exclude [eval])
  (:import [masp.value PrimOp Applicative Continuation CompoundOp]))

(defn- -eval [ctrl env cont]
  (cond
    (and (seq? ctrl) (seq ctrl))
    (let [[op & operand] ctrl
          cont* (into cont [operand env :op])]
      [:eval op nil env cont*])

    (symbol? ctrl)
    (if-let [value (get env ctrl)]
      (if (instance? clojure.lang.Atom value)
        (if-let [value* @value]
          [:continue value* nil nil cont]
          [:err [:unbound ctrl]])
        [:continue value nil nil cont])
      [:err [:unbound ctrl]])

    :else
    [:continue ctrl nil nil cont]))

(defn- -continue [value cont]
  (case (peek cont)
    :op
    (let [[operand env _] (take-last 3 cont)
          cont* (subvec cont 0 (- (count cont) 3))]
      [:apply value operand env cont*])

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
          [:eval ctrl nil env cont*])
        (let [args (subvec cont (- (count cont) n 2) (- (count cont) 2))
              arg (conj args value)
              cont* (subvec cont 0 (- (count cont) 2 n 3))]
          [:apply op arg env cont*])))

    :stmt
    (let [[env stmts _] (take-last 3 cont)]
      (if (seq stmts)
        (let [[stmt & stmts*] stmts
              cont* (assoc cont (- (count cont) 2) stmts*)]
          [:eval stmt nil env cont*])
        (let [cont* (subvec cont 0 (- (count cont) 3))]
          [:continue value nil nil cont*])))

    :halt
    [:ok value]

    :err
    [:err value]))

(defn- -apply [op operand env cont]
  (condp instance? op
    PrimOp       ((.f op) operand env cont)
    CompoundOp   (let [assoc-symbol (fn [env k v]
                                      (if (symbol? k)
                                        (assoc env k v)
                                        env))
                       env* (-> (.lex-env op)
                                (assoc-symbol (.name op) op)
                                (assoc-symbol (.formal op) operand)
                                (assoc-symbol (.eformal op) env))]
                   [:eval (.body op) nil env* cont])
    Applicative  (let [op (.op op)]
                   (if (seq operand)
                     (let [[ctrl & operands] operand
                           cont* (into cont [env op operands 0 :arg])]
                       [:eval ctrl nil env cont*])
                     [:apply op operand env cont]))
    Continuation [:continue operand nil nil (.cont op)]))

(defn eval [ctrl env cont]
  (loop [label :eval, ctrl ctrl, operand nil, env env, cont cont]
    (let [[label* :as state*]
          (case label
            :eval         (-eval ctrl env cont)
            :continue     (-continue ctrl cont)
            :apply        (-apply ctrl operand env cont))]
      (if (or (= label* :ok) (= label* :err))
        state*
        (let [[label* ctrl* operand* env* cont*] state*]
          (recur label* ctrl* operand* env* cont*))))))
