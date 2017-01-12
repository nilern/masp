(ns masp.eval
  (:refer-clojure :exclude [subvec eval])
  (:require [clojure.core.rrb-vector :refer [subvec]]
            [masp.util :refer [assoc-symbol]])
  (:import [masp.value PrimOp Applicative Continuation CompoundOp]))

;;; TODO: make -eval and -combine extensible

(defn- -eval [ctrl env cont]
  (cond
    (and (seq? ctrl) (seq ctrl))
    (let [op (first ctrl)
          operand (or (next ctrl) '())
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

(defn- -combine [op operand env cont]
  (condp instance? op
    PrimOp       ((.f op) operand env cont)
    CompoundOp   (let [ctrl* (.body op)
                       env* (-> (.lex-env op)
                                (assoc-symbol (.formal op) operand)
                                (assoc-symbol (.eformal op) env))]
                   [:eval ctrl* nil env* cont])
    Applicative  (if (seq operand)
                   (let [[ctrl & operands] operand
                         cont* (into cont [env op operands 0 :arg])]
                     [:eval ctrl nil env cont*])
                   [:combine (.op op) [] env cont])
    Continuation [:continue operand nil nil (.cont op)]))

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
