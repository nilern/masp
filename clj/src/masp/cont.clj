(ns masp.cont)

;;; TODO: delimited continuations

(defprotocol IContinuation
  (peek-frame [self]
    "Return the topmost frame of self.")
  (pop-frame [self]
    "Return a continuation of self with the topmost frame removed.")
  (push-frame [self frame]
    "Return a new continuation with frame on top of self.")
  (replace-frame [self frame]
    "Like push-frame, but replaces topmost frame instead of appending."))

(deftype ContStack [data]
  IContinuation
  (peek-frame [_] (peek data))
  (pop-frame [_] (ContStack. (pop data)))
  (push-frame [_ frame] (ContStack. (conj data frame)))
  (replace-frame [_ frame] (ContStack. (-> data pop (conj frame)))))

(defn cont-stack []
  (ContStack. [[:halt]]))
