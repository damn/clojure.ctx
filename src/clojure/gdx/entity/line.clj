(in-ns 'clojure.gdx)

(defc :entity/line-render
  {:let {:keys [thick? end color]}}
  (render [_ entity*]
    (let [position (:position entity*)]
      (if thick?
        (with-shape-line-width 4 #(draw-line position end color))
        (draw-line position end color)))))

(defc :tx/line-render
  (do! [[_ {:keys [start end duration color thick?]}]]
    [[:e/create
      start
      effect-body-props
      #:entity {:line-render {:thick? thick? :end end :color color}
                :delete-after-duration duration}]]))
