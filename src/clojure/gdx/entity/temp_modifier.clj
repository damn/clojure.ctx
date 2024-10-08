(in-ns 'clojure.gdx)

(defc :entity/temp-modifier
  {:let {:keys [counter modifiers]}}
  (info-text [_]
    (str "[LIGHT_GRAY]Spiderweb - remaining: " (readable-number (finished-ratio counter)) "/1[]"))

  (tick [[k _] eid]
    (when (stopped? counter)
      [[:e/dissoc eid k]
       [:tx/reverse-modifiers eid modifiers]]))

  (render-above [_ entity*]
    (draw-filled-circle (:position entity*) 0.5 [0.5 0.5 0.5 0.4])))
