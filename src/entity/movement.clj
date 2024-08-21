(ns entity.movement
  (:require [malli.core :as m]
            [math.vector :as v]
            [math.geom :as geom]
            [core.component :as component :refer [defcomponent]]
            [core.data :as data]
            [api.context :as ctx]
            [api.entity :as entity]
            [api.effect :as effect]
            [api.graphics :as g]
            [gdx.graphics.color :as color]
            [api.world.cell :as cell]
            [api.world.grid :as world-grid]
            [context.game.time :refer [max-delta-time]]))

; # :z-order/flying has no effect for now

; * entities with :z-order/flying are not flying over water,etc. (movement/air)
; because using potential-field for z-order/ground
; -> would have to add one more potential-field for each faction for z-order/flying
; * they would also (maybe) need a separate occupied-cells if they don't collide with other
; * they could also go over ground units and not collide with them
; ( a test showed then flying OVER player entity )
; -> so no flying units for now

; set max speed so small entities are not skipped by projectiles
; could set faster than max-speed if I just do multiple smaller movement steps in one frame
(def max-speed (/ entity/min-solid-body-size
                  max-delta-time))

(def movement-speed-schema [:and number? [:>= 0] [:<= max-speed]])
(def ^:private movement-speed-schema* (m/schema movement-speed-schema))

(defn- move-position [position {:keys [direction speed delta-time]}]
  (mapv #(+ %1 (* %2 speed delta-time)) position direction))

(defn- move-body [body movement]
  (-> body
      (update :position    move-position movement)
      (update :left-bottom move-position movement)))

(defn- valid-position? [grid body]
  (let [{:keys [entity/id z-order solid?]} body
        ; similar =code to set-cells! body->calculate-touched-cells.
        ; some places maybe use cached-touched-cells ....
        cells* (into [] (map deref) (world-grid/rectangle->cells grid body))]
    (and (not-any? #(cell/blocked? % z-order) cells*)
         (or (not solid?) ; this not needed as we call this only for valid-position foo
             (->> cells*
                  cell/cells->entities ; could add new field to Cell solid-entities, here checking all entities
                  ; also effects, items, .... etc.
                  (not-any? (fn [other-entity]
                              ; TODO move out fn - entity/same-id?
                              (let [other-entity* @other-entity]
                                (and (not= (:entity/id other-entity*) id)
                                     ; fn entity/colliding? which checks solid?
                                     (:solid? other-entity*)
                                     (geom/collides? other-entity* body))))))))))

(defn- try-move [grid body movement]
  (let [new-body (move-body body movement)]
    (when (valid-position? grid new-body)
      new-body)))

; TODO sliding threshold
; TODO name - with-sliding? 'on'
; TODO if direction was [-1 0] and invalid-position then this algorithm tried to move with
; direection [0 0] which is a waste of processor power...
(defn- try-move-solid-body [grid body {[vx vy] :direction :as movement}]
  (let [xdir (Math/signum (float vx))
        ydir (Math/signum (float vy))]
    (or (try-move grid body movement)
        (try-move grid body (assoc movement :direction [xdir 0]))
        (try-move grid body (assoc movement :direction [0 ydir])))))

(def ^:private show-body-bounds false)

(defn- draw-bounds [g {[x y] :left-bottom :keys [width height solid?]}]
  (when show-body-bounds
    (g/draw-rectangle g x y width height (if solid? color/white color/gray))))

(defcomponent :entity/movement {}

  ;TODO validate at component create?
  ; call component create on assoc ...
  ; check if we still need assoc-in/dissoc-in

  ; TODO where to put this?
  #_(entity/render-debug [_ entity* g _ctx]
    (draw-bounds g entity*))

  (entity/tick [[_ {:keys [direction speed rotate-in-movement-direction?] :as movement}] eid ctx]
    (assert (m/validate movement-speed-schema* speed))
    (assert (or (zero? (v/length direction))
                (v/normalised? direction)))
    (when-not (or (zero? (v/length direction))
                  (nil? speed)
                  (zero? speed))
      (let [movement (assoc movement :delta-time (ctx/delta-time ctx))
            body @eid]
        (when-let [body (if (:solid? body)
                          (try-move-solid-body (ctx/world-grid ctx) body movement)
                          (move-body body movement))]
          [[:tx.entity/assoc eid :position    (:position    body)]
           [:tx.entity/assoc eid :left-bottom (:left-bottom body)]
           (when rotate-in-movement-direction?
             [:tx.entity/assoc eid :rotation-angle (v/get-angle-from-vector direction)])
           [:tx/position-changed eid]])))))

(defcomponent :tx.entity/set-movement {}
  (effect/do! [[_ entity movement] ctx]
    {:pre [(or (nil? movement)
               (and (:direction movement) ; continue schema of that ...
                    #_(:speed movement)))]} ; princess no stats/movement-speed, then nil and here assertion-error
    [[:tx.entity/assoc entity :entity/movement movement]]))

; TODO add teleport effect ~ or tx
