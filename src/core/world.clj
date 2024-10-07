(ns core.world
  (:require [clojure.gdx :refer :all]
            [clojure.gdx.rand :refer [get-rand-weighted-item]]
            [clojure.gdx.tiled :as t]
            [clojure.string :as str]
            [data.grid2d :as g])
  (:load "world/modules"
         "world/generators"
         "world/tiled_map_renderer"
         "world/editor_screen"
         "world/spawn"
         "world/widgets"))

(defn- init-world-time! []
  (bind-root #'elapsed-time 0)
  (bind-root #'logic-frame 0))

(defn update-time [delta]
  (bind-root #'world-delta delta)
  (alter-var-root #'elapsed-time + delta)
  (alter-var-root #'logic-frame inc))

(defn- ->explored-tile-corners [width height]
  (atom (g/create-grid width height (constantly false))))

(defn- world-grid-position->value-fn [tiled-map]
  (fn [position]
    (case (t/movement-property tiled-map position)
      "none" :none
      "air"  :air
      "all"  :all)))

(declare entity-tick-error)

(defn- cleanup-last-world! []
  (when (bound? #'world-tiled-map)
    (dispose! world-tiled-map)))

(defn- init-new-world! [{:keys [tiled-map start-position]}]
  (bind-root #'entity-tick-error nil)
  (init-world-time!)
  (bind-root #'world-widgets (->world-widgets))
  (init-uids-entities!)

  (bind-root #'world-tiled-map tiled-map)
  (let [w (t/width  tiled-map)
        h (t/height tiled-map)
        grid (init-world-grid! w h (world-grid-position->value-fn tiled-map))]
    (init-world-raycaster! grid blocks-vision?)
    (init-content-grid! :cell-size 16 :width w :height h)
    (bind-root #'explored-tile-corners (->explored-tile-corners w h)))

  (spawn-creatures! tiled-map start-position))

; TODO  add-to-world/assoc/dissoc uid from entity move together here
; also core.screens/world ....
(bind-root #'clojure.gdx/add-world-ctx
           (fn [world-property-id]
             (cleanup-last-world!)
             (init-new-world! (generate-level world-property-id))))

(defcomponent :tx/add-to-world
  (do! [[_ entity]]
    (content-grid-update-entity! entity)
    ; https://github.com/damn/core/issues/58
    ;(assert (valid-position? grid @entity)) ; TODO deactivate because projectile no left-bottom remove that field or update properly for all
    (grid-add-entity! entity)
    nil))

(defcomponent :tx/remove-from-world
  (do! [[_ entity]]
    (content-grid-remove-entity! entity)
    (grid-remove-entity! entity)
    nil))

(defcomponent :tx/position-changed
  (do! [[_ entity]]
    (content-grid-update-entity! entity)
    (grid-entity-position-changed! entity)
    nil))
