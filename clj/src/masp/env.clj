(ns masp.env
  (:refer-clojure :exclude [extend]))

;;; FIXME: cancel all reservations on -push-frame

(defprotocol IEnvironment
  (lookup [self name]
    "Look up value of variable. Returns nil if name is unbound.")
  (-push-frame [self bindings op-name op-id]
    "Create an extended environment upon entry to the operative named op-name,
     merging bindings into the bindings in self and using op-id as the id of
     this activation of the operative.")
  (extend [self name value]
    "Add a new binding from name to value.")
  (reserve [self names]
    "Make every variable in names unbound and settable by claim!.")
  (claim! [self name value]
    "Destructively add a binding from name to value. For most environments can
     only be used after name has been reserve:d and even then only once. Returns
     self on success and nil when name was not settable.")
  (scope= [self other]
    "Were both self and other created in the scope of the same activation?")
  (op-name [self]
    "What is the name of the operative for whose activation this environment was
     created?"))

(deftype SDAEnv [bindings op-name op-id]
  IEnvironment
  (lookup [_ name]
    (when-let [v (get bindings name)]
      (if (instance? clojure.lang.Atom v)
        @v
        v)))
  (-push-frame [self bindings* op-name* op-id*]
    (SDAEnv. (merge bindings bindings*) op-name* op-id*))
  (extend [_ name value]
    (SDAEnv. (assoc bindings name value) op-name op-id))
  (reserve [self names]
    (SDAEnv. (merge bindings (zipmap names (repeatedly #(atom nil))))
             op-name op-id))
  (claim! [self name value]
    (when-let [loc (get bindings name)]
      (when (and (instance? clojure.lang.Atom loc) (nil? @loc))
        (reset! loc value)
        self)))
  (scope= [self other]
    (= op-id (.-op_id other)))
  (op-name [self]
    (.-op_name self)))

(def environment
  (let [id-counter (atom 0)]
    (fn
      ([]
       (environment {}))
      ([bindings]
       (SDAEnv. bindings nil (swap! id-counter inc)))
      ([parent bindings op-name]
       (-push-frame parent bindings op-name (swap! id-counter inc))))))
