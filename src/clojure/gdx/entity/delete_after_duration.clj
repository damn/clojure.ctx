(in-ns 'clojure.gdx)

(defcomponent :entity/delete-after-duration
  {:let counter}
  (->mk [[_ duration]]
    (->counter duration))

  (info-text [_]
    (str "[LIGHT_GRAY]Remaining: " (readable-number (finished-ratio counter)) "/1[]"))

  (tick [_ eid]
    (when (stopped? counter)
      [[:e/destroy eid]])))
