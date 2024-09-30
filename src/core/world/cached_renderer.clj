(in-ns 'core.world)

(defn render!
  "Renders tiled-map using world-view at world-camera position and with world-unit-scale.
  Color-setter is a (fn [color x y]) which is called for every tile-corner to set the color.
  Can be used for lights & shadows.
  The map-renderers are created and cached internally.
  Renders only visible layers."
  [{g :context/graphics
    cached-map-renderer :context/tiled-map-renderer
    :as ctx}
   tiled-map
   color-setter]
  (t/render-tm! (cached-map-renderer g tiled-map)
                color-setter
                (world-camera ctx)
                tiled-map))

(defn ->tiled-map-renderer [{:keys [batch] :as g} tiled-map]
  (t/->orthogonal-tiled-map-renderer tiled-map
                                     (world-unit-scale g)
                                     batch))

(defcomponent :context/tiled-map-renderer
  {:data :some}
  (->mk [_ _ctx]
    (memoize ->tiled-map-renderer)))

(def ^:private explored-tile-color (->color 0.5 0.5 0.5 1))

(def ^:private ^:dbg-flag see-all-tiles? false)

(comment
 (def ^:private count-rays? false)

 (def ray-positions (atom []))
 (def do-once (atom true))

 (count @ray-positions)
 2256
 (count (distinct @ray-positions))
 608
 (* 608 4)
 2432
 )

(defn- ->tile-color-setter [light-cache light-position raycaster explored-tile-corners]
  (fn tile-color-setter [_color x y]
    (let [position [(int x) (int y)]
          explored? (get @explored-tile-corners position) ; TODO needs int call ?
          base-color (if explored? explored-tile-color black)
          cache-entry (get @light-cache position :not-found)
          blocked? (if (= cache-entry :not-found)
                     (let [blocked? (fast-ray-blocked? raycaster light-position position)]
                       (swap! light-cache assoc position blocked?)
                       blocked?)
                     cache-entry)]
      #_(when @do-once
          (swap! ray-positions conj position))
      (if blocked?
        (if see-all-tiles? white base-color)
        (do (when-not explored?
              (swap! explored-tile-corners assoc (->tile position) true))
            white)))))

(defn render-map [{:keys [context/tiled-map] :as ctx} light-position]
  (render! ctx
           tiled-map
           (->tile-color-setter (atom nil)
                                light-position
                                (:context/raycaster ctx)
                                (:context/explored-tile-corners ctx)))
  #_(reset! do-once false))
