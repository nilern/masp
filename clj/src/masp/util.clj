(ns masp.util)

(defn assoc-symbol [env k v]
  (if (symbol? k)
    (assoc env k v)
    env))
