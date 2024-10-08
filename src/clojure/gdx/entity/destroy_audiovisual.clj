(in-ns 'clojure.gdx)

(defc :entity/destroy-audiovisual
  {:let audiovisuals-id}
  (destroy [_ entity]
    [[:tx/audiovisual (:position @entity) audiovisuals-id]]))
