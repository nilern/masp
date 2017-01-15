(ns masp.cont)

(defprotocol IContinuation
  (peek-frame [self]
    "Return the topmost frame of self.")
  (pop-frame [self]
    "Return a continuation of self with the topmost frame removed.")
  (push-frame [self frame]
    "Return a new continuation with frame on top of self.")
  (replace-frame [self frame]
    "Like push-frame, but replaces topmost frame instead of appending.")

  (subcont [self prompt]
    "Return a subcontinuation up to the topmost appearance of prompt.")
  (pop-subcont [self prompt]
    "Remove frames up to the topmost appearance of prompt.")
  (append-subcont [self subk]
    "Extend self with subk.")

  (push-mark [self key value]
    "Add a mark with key and value to the topmost frame."))

(deftype ContStack [frames]
  IContinuation
  (peek-frame [_] (first frames))
  (pop-frame [_] (ContStack. (rest frames)))
  (push-frame [_ frame] (ContStack. (cons frame frames)))
  (replace-frame [_ frame] (ContStack. (->> frames rest (cons frame))))

  (subcont [_ prompt]
    (ContStack. (take-while (fn [frame] (not (contains? (meta frame) prompt)))
                            frames)))
  (pop-subcont [_ prompt]
    (ContStack. (drop-while (fn [frame] (not (contains? (meta frame) prompt)))
                            frames)))
  (append-subcont [_ subk]
    (ContStack. (concat (.frames subk) frames)))

  (push-mark [self k v]
    (replace-frame self (vary-meta (first frames) assoc k v))))

(defn cont-stack []
  (ContStack. (list [:halt])))
