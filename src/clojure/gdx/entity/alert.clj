(in-ns 'clojure.gdx)

(def ^:private shout-radius 4)

(defn- friendlies-in-radius [position faction]
  (->> {:position position
        :radius shout-radius}
       (circle->entities world-grid)
       (map deref)
       (filter #(= (:entity/faction %) faction))
       (map :entity/id)))

(defc :entity/alert-friendlies-after-duration
  {:let {:keys [counter faction]}}
  (tick [_ eid]
    (when (stopped? counter)
      (cons [:e/destroy eid]
            (for [friendly-eid (friendlies-in-radius (:position @eid) faction)]
              [:tx/event friendly-eid :alert])))))

(defc :tx/shout
  (do! [[_ position faction delay-seconds]]
    [[:e/create
      position
      effect-body-props
      {:entity/alert-friendlies-after-duration
       {:counter (->counter delay-seconds)
        :faction faction}}]]))
