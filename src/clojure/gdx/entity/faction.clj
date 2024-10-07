(in-ns 'clojure.gdx)

(defn enemy-faction [{:keys [entity/faction]}]
  (case faction
    :evil :good
    :good :evil))

(defn friendly-faction [{:keys [entity/faction]}]
  faction)

(defcomponent :entity/faction
  {:let faction
   :data [:enum [:good :evil]]}
  (info-text [_]
    (str "[SLATE]Faction: " (name faction) "[]")))
