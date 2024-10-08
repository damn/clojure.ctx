(in-ns 'clojure.gdx)

(defc :entity/image
  {:data :image
   :let image}
  (render [_ entity*]
    (draw-rotated-centered-image image
                                 (or (:rotation-angle entity*) 0)
                                 (:position entity*))))
