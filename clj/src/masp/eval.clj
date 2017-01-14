(ns masp.eval
  (:refer-clojure :exclude [subvec eval])
  (:require [clojure.core.rrb-vector :refer [subvec]]
            [masp.env :as e])
  (:import [masp.value PrimOp Applicative Continuation CompoundOp]))

;;; TODO: delimited continuations (?)
;;; QUESTION: should 'evlis' be extensible too (esp. wrt. f:(a b)/arr:[i] etc.)?

(defn- -eval [ctrl env cont]
  (cond
    (and (seq? ctrl) (seq ctrl))
    (let [op (first ctrl)
          operand (or (next ctrl) '())
          cont* (into cont [operand env :op])]
      [:eval op nil env cont*])

    (symbol? ctrl)
    (if-let [value (e/lookup env ctrl)]
      [:continue value nil nil cont]
      [:err [:unbound ctrl]])

    :else
    (if-let [ext-eval (e/lookup env 'eval*)]
      [:combine (.op ext-eval) [ctrl env] env cont]
      [:continue ctrl nil env cont])))

(defn- -continue [value cont]
  (case (peek cont)
    :op
    (let [[operand env _] (take-last 3 cont)
          cont* (subvec cont 0 (- (count cont) 3))]
      [:combine value operand env cont*])

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
          [:combine (.op op) arg env cont*])))

    :stmt
    (let [[env [stmt & stmts] _] (take-last 3 cont)]
      (if stmt
        (if stmts
          (let [cont* (assoc cont (- (count cont) 2) stmts)]
            [:eval stmt nil env cont*])
          (let [cont* (subvec cont 0 (- (count cont) 3))]
            [:eval stmt nil env cont*]))
        (let [cont* (subvec cont 0 (- (count cont) 3))]
          [:continue value nil nil cont*])))

    :halt
    [:ok value]

    :err
    [:err value]))

(defn- -combine [op operand env cont]
  (condp instance? op
    PrimOp       ((.f op) operand env cont)
    CompoundOp   (let [assoc-symbol (fn [bindings name value]
                                      (if (symbol? name)
                                        (assoc bindings name value)
                                        bindings))
                       ctrl* (.body op)
                       bindings* (-> {}
                                     (assoc-symbol (.formal op) operand)
                                     (assoc-symbol (.eformal op) env))
                       env* (e/environment (.lex-env op) bindings* (.name op))]
                   [:eval ctrl* nil env* cont])
    Applicative  (if (seq operand)
                   (let [[ctrl & operands] operand
                         cont* (into cont [env op operands 0 :arg])]
                     [:eval ctrl nil env cont*])
                   [:combine (.op op) [] env cont])
    (if-let [ext-combine (e/lookup env 'combine*)]
      [:combine (.op ext-combine) [op operand] env cont]
      [:err [:noncombiner op]])))

(defn eval [ctrl env cont]
  (loop [label :eval, ctrl ctrl, operand nil, env env, cont cont]
    (let [[label* :as state*]
          (case label
            :eval     (-eval ctrl env cont)
            :continue (-continue ctrl cont)
            :combine  (-combine ctrl operand env cont))]
      (if (or (= label* :ok) (= label* :err))
        state*
        (let [[label* ctrl* operand* env* cont*] state*]
          (recur label* ctrl* operand* env* cont*))))))
