(in-ns 'clojure.gdx)

(defn- move-position [position {:keys [direction speed delta-time]}]
  (mapv #(+ %1 (* %2 speed delta-time)) position direction))

(defn- move-body [body movement]
  (-> body
      (update :position    move-position movement)
      (update :left-bottom move-position movement)))

(defn- valid-position? [{:keys [entity/id z-order] :as body}]
  {:pre [(:collides? body)]}
  (let [cells* (into [] (map deref) (rectangle->cells world-grid body))]
    (and (not-any? #(blocked? % z-order) cells*)
         (->> cells*
              cells->entities
              (not-any? (fn [other-entity]
                          (let [other-entity* @other-entity]
                            (and (not= (:entity/id other-entity*) id)
                                 (:collides? other-entity*)
                                 (collides? other-entity* body)))))))))

(defn- try-move [body movement]
  (let [new-body (move-body body movement)]
    (when (valid-position? new-body)
      new-body)))

; TODO sliding threshold
; TODO name - with-sliding? 'on'
; TODO if direction was [-1 0] and invalid-position then this algorithm tried to move with
; direection [0 0] which is a waste of processor power...
(defn- try-move-solid-body [body {[vx vy] :direction :as movement}]
  (let [xdir (Math/signum (float vx))
        ydir (Math/signum (float vy))]
    (or (try-move body movement)
        (try-move body (assoc movement :direction [xdir 0]))
        (try-move body (assoc movement :direction [0 ydir])))))

(defc :entity/movement
  {:let {:keys [direction speed rotate-in-movement-direction?] :as movement}}
  (tick [_ eid]
    (assert (m/validate movement-speed-schema speed))
    (assert (or (zero? (v-length direction))
                (v-normalised? direction)))
    (when-not (or (zero? (v-length direction))
                  (nil? speed)
                  (zero? speed))
      (let [movement (assoc movement :delta-time world-delta)
            body @eid]
        (when-let [body (if (:collides? body) ; < == means this is a movement-type ... which could be a multimethod ....
                          (try-move-solid-body body movement)
                          (move-body body movement))]
          [[:e/assoc eid :position    (:position    body)]
           [:e/assoc eid :left-bottom (:left-bottom body)]
           (when rotate-in-movement-direction?
             [:e/assoc eid :rotation-angle (v-get-angle-from-vector direction)])
           [:tx/position-changed eid]])))))

(defc :tx/set-movement
  (do! [[_ entity movement]]
    (assert (or (nil? movement)
                (nil? (:direction movement))
                (and (:direction movement) ; continue schema of that ...
                     #_(:speed movement)))) ; princess no stats/movement-speed, then nil and here assertion-error
    [(if (or (nil? movement)
             (nil? (:direction movement)))
       [:e/dissoc entity :entity/movement]
       [:e/assoc entity :entity/movement movement])]))
