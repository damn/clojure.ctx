(in-ns 'clojure.gdx)

(defc :entity/clickable
  (render [[_ {:keys [text]}]
           {:keys [entity/mouseover?] :as entity*}]
    (when (and mouseover? text)
      (let [[x y] (:position entity*)]
        (draw-text {:text text
                    :x x
                    :y (+ y (:half-height entity*))
                    :up? true})))))
