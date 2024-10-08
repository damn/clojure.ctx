(in-ns 'clojure.gdx)

(defprotocol Animation
  (^:private anim-tick [_ delta])
  (^:private restart [_])
  (^:private anim-stopped? [_])
  (^:private current-frame [_]))

(defrecord ImmutableAnimation [frames frame-duration looping? cnt maxcnt]
  Animation
  (anim-tick [this delta]
    (let [maxcnt (float maxcnt)
          newcnt (+ (float cnt) (float delta))]
      (assoc this :cnt (cond (< newcnt maxcnt) newcnt
                             looping? (min maxcnt (- newcnt maxcnt))
                             :else maxcnt))))

  (restart [this]
    (assoc this :cnt 0))

  (anim-stopped? [_]
    (and (not looping?) (>= cnt maxcnt)))

  (current-frame [this]
    (frames (min (int (/ (float cnt) (float frame-duration)))
                 (dec (count frames))))))

(defn- ->animation [frames & {:keys [frame-duration looping?]}]
  (map->ImmutableAnimation
    {:frames (vec frames)
     :frame-duration frame-duration
     :looping? looping?
     :cnt 0
     :maxcnt (* (count frames) (float frame-duration))}))

(defn- edn->animation [{:keys [frames frame-duration looping?]}]
  (->animation (map edn->image frames)
               :frame-duration frame-duration
               :looping? looping?))


(defmethod edn->value :data/animation [_ animation]
  (edn->animation animation))

; looping? - click on widget restart
; frame-duration
; frames ....
; hidden actor act tick atom animation & set current frame image drawable
(defmethod ->widget :data/animation [_ animation]
  (->table {:rows [(for [image (:frames animation)]
                        (->image-widget (edn->image image) {}))]
               :cell-defaults {:pad 1}}))

(defn- tx-assoc-image-current-frame [eid animation]
  [:e/assoc eid :entity/image (current-frame animation)])

(defc :entity/animation
  {:data :data/animation
   :let animation}
  (create [_ eid]
    [(tx-assoc-image-current-frame eid animation)])

  (tick [[k _] eid]
    [(tx-assoc-image-current-frame eid animation)
     [:e/assoc eid k (anim-tick animation world-delta)]]))

(defc :entity/delete-after-animation-stopped?
  (create [_ entity]
    (-> @entity :entity/animation :looping? not assert))

  (tick [_ entity]
    (when (anim-stopped? (:entity/animation @entity))
      [[:e/destroy entity]])))

(def-property-type :properties/audiovisuals
  {:schema [:tx/sound
            :entity/animation]
   :overview {:title "Audiovisuals"
              :columns 10
              :image/scale 2}})

(defc :tx/audiovisual
  (do! [[_ position id]]
    (let [{:keys [tx/sound
                  entity/animation]} (build-property id)]
      [[:tx/sound sound]
       [:e/create
        position
        effect-body-props
        {:entity/animation animation
         :entity/delete-after-animation-stopped? true}]])))
