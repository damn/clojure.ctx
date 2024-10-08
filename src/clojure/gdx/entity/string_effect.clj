(in-ns 'clojure.gdx)

(defc :entity/string-effect
  (tick [[k {:keys [counter]}] eid]
    (when (stopped? counter)
      [[:e/dissoc eid k]]))

  (render-above [[_ {:keys [text]}] entity*]
    (let [[x y] (:position entity*)]
      (draw-text {:text text
                  :x x
                  :y (+ y (:half-height entity*) (pixels->world-units hpbar-height-px))
                  :scale 2
                  :up? true}))))

(defc :tx/add-text-effect
  (do! [[_ entity text]]
    [[:e/assoc
      entity
      :entity/string-effect
      (if-let [string-effect (:entity/string-effect @entity)]
        (-> string-effect
            (update :text str "\n" text)
            (update :counter reset))
        {:text text
         :counter (->counter 0.4)})]]))

