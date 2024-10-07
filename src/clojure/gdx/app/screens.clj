(in-ns 'clojure.gdx)

(declare ^:private screen-k
         ^:private screens)

(defn current-screen []
  [screen-k (screen-k screens)])

(defsystem screen-enter "FIXME" [_])
(defmethod screen-enter :default [_])

(defsystem screen-exit  "FIXME" [_])
(defmethod screen-exit :default  [_])

(defn change-screen
  "Calls `screen-exit` on the current-screen (if there is one).
  Calls `screen-enter` on the new screen."
  [new-k]
  (when-let [v (and (bound? #'screen-k) (screen-k screens))]
    (screen-exit [screen-k v]))
  (let [v (new-k screens)]
    (assert v (str "Cannot find screen with key: " new-k))
    (bind-root #'screen-k new-k)
    (screen-enter [new-k v])))

(defsystem ^:private screen-render! "FIXME" [_])

(defsystem screen-render "FIXME" [_])
(defmethod screen-render :default [_])

(defn create-vs
  "Creates a map for every component with map entries `[k (->mk [k v])]`."
  [components]
  (reduce (fn [m [k v]]
            (assoc m k (->mk [k v])))
          {}
          components))

(defcomponent :context/screens
  {:data :some
   :let screen-ks}
  (->mk [_]
    (bind-root #'screens (create-vs (zipmap screen-ks (repeat nil))))
    (change-screen (ffirst screens)))

  (destroy! [_]
    ; TODO screens not disposed https://github.com/damn/core/issues/41
    ))
