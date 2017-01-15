(ns masp.eval
  (:refer-clojure :exclude [subvec eval])
  (:require [clojure.core.rrb-vector :refer [subvec]]
            [masp.env :as e]
            [masp.cont :as k])
  (:import [masp.value PrimOp Applicative Continuation CompoundOp]))

;;; QUESTION: should 'evlis' be extensible too (esp. wrt. f:(a b)/arr:[i] etc.)?

(defn- -eval [ctrl env cont]
  (cond
    (and (seq? ctrl) (seq ctrl))
    (let [op (first ctrl)
          operand (or (next ctrl) '())
          cont* (k/push-frame cont [:op env operand])]
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
  (let [frame (k/peek-frame cont)]
    (case (first frame)
      :op
      (let [[_ env operand] frame
            cont* (k/pop-frame cont)]
        [:combine value operand env cont*])

      :arg
      (let [[_ env op operands args] frame
            args* (conj args value)]
        (if (seq operands)
          (let [[ctrl & operands*] operands
                cont* (k/replace-frame cont [:arg env op operands* args*])]
            [:eval ctrl nil env cont*])
          (let [cont* (k/pop-frame cont)]
            [:combine (.op op) args* env cont*])))

      :stmt
      (let [[_ env [stmt & stmts]] frame]
        (if stmt
          (if stmts
            (let [cont* (k/replace-frame cont [:stmt env stmts])]
              [:eval stmt nil env cont*])
            (let [cont* (k/pop-frame cont)]
              [:eval stmt nil env cont*]))
          (let [cont* (k/pop-frame cont)]
            [:continue value nil nil cont*])))

      :halt
      [:ok value]

      :err
      [:err value])))

(defn- -combine [op operand env cont]
  (condp instance? op
    PrimOp       ((.f op) operand env cont)
    CompoundOp   (let [assoc-symbol (fn [bindings name value]
                                      (if (symbol? name)
                                        (assoc bindings name value)
                                        bindings))
                       [stmt & stmts] (.body op)
                       bindings* (-> {}
                                     (assoc-symbol (.formal op) operand)
                                     (assoc-symbol (.eformal op) env))
                       env* (e/environment (.lex-env op) bindings* (.name op))]
                   (if stmt
                     (if stmts
                       (let [cont* (k/push-frame cont [:stmt env* stmts])]
                         [:eval stmt nil env* cont*])
                       [:eval stmt nil env* cont])
                     [:continue () nil nil cont]))
    Applicative  (if (seq operand)
                   (let [[ctrl & operands] operand
                         cont* (k/push-frame cont [:arg env op operands []])]
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
