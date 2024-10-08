(in-ns 'clojure.gdx)

(def-property-type :properties/projectiles
  {:schema [:entity/image
            :projectile/max-range
            :projectile/speed
            :projectile/piercing?
            :entity-effects]
   :overview {:title "Projectiles"
              :columns 16
              :image/scale 2}})

(defc :entity/projectile-collision
  {:let {:keys [entity-effects already-hit-bodies piercing?]}}
  (->mk [[_ v]]
    (assoc v :already-hit-bodies #{}))

  ; TODO probably belongs to body
  (tick [[k _] entity]
    ; TODO this could be called from body on collision
    ; for non-solid
    ; means non colliding with other entities
    ; but still collding with other stuff here ? o.o
    (let [entity* @entity
          cells* (map deref (rectangle->cells world-grid entity*)) ; just use cached-touched -cells
          hit-entity (find-first #(and (not (contains? already-hit-bodies %)) ; not filtering out own id
                                       (not= (:entity/faction entity*) ; this is not clear in the componentname & what if they dont have faction - ??
                                             (:entity/faction @%))
                                       (:collides? @%)
                                       (collides? entity* @%))
                                 (cells->entities cells*))
          destroy? (or (and hit-entity (not piercing?))
                       (some #(blocked? % (:z-order entity*)) cells*))
          id (:entity/id entity*)]
      [(when hit-entity
         [:e/assoc-in id [k :already-hit-bodies] (conj already-hit-bodies hit-entity)]) ; this is only necessary in case of not piercing ...
       (when destroy?
         [:e/destroy id])
       (when hit-entity
         [:tx/effect {:effect/source id :effect/target hit-entity} entity-effects])])))

; TODO speed is 10 tiles/s but I checked moves 8 tiles/sec ... after delta time change ?

; -> range needs to be smaller than potential field range (otherwise hitting someone who can't get back at you)
; -> first range check then ray ! otherwise somewhere in contentfield out of sight
(defc :projectile/max-range {:data :pos-int})
(defc :projectile/speed     {:data :pos-int})

(defc :projectile/piercing? {:data :boolean}
  (info-text [_] "[LIME]Piercing[]"))

(defn projectile-size [projectile]
  {:pre [(:entity/image projectile)]}
  (first (:world-unit-dimensions (:entity/image projectile))))

(defc :tx/projectile
  (do! [[_
         {:keys [position direction faction]}
         {:keys [entity/image
                 projectile/max-range
                 projectile/speed
                 entity-effects
                 projectile/piercing?] :as projectile}]]
    (let [size (projectile-size projectile)]
      [[:e/create
        position
        {:width size
         :height size
         :z-order :z-order/flying
         :rotation-angle (v-get-angle-from-vector direction)}
        {:entity/movement {:direction direction
                           :speed speed}
         :entity/image image
         :entity/faction faction
         :entity/delete-after-duration (/ max-range speed)
         :entity/destroy-audiovisual :audiovisuals/hit-wall
         :entity/projectile-collision {:entity-effects entity-effects
                                       :piercing? piercing?}}]])))
