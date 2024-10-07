(in-ns 'core.world)

; TODO map-coords are clamped ? thats why showing 0 under and left of the map?
; make more explicit clamped-map-coords ?

; TODO
; leftest two tiles are 0 coordinate x
; and rightest is 16, not possible -> check clamping
; depends on screen resize or something, changes,
; maybe update viewport not called on resize sometimes

(defn- show-whole-map! [camera tiled-map]
  (camera-set-position! camera
                        [(/ (t/width  tiled-map) 2)
                         (/ (t/height tiled-map) 2)])
  (set-zoom! camera
             (calculate-zoom camera
                             :left [0 0]
                             :top [0 (t/height tiled-map)]
                             :right [(t/width tiled-map) 0]
                             :bottom [0 0])))

(defn- current-data []
  (-> (current-screen)
      (get 1)
      :sub-screen
      (get 1)))

(def ^:private infotext
  "L: grid lines
M: movement properties
zoom: shift-left,minus
ESCAPE: leave
direction keys: move")

(defn- map-infos ^String []
  (let [tile (->tile (world-mouse-position))
        {:keys [tiled-map
                area-level-grid]} @(current-data)]
    (->> [infotext
          (str "Tile " tile)
          (when-not area-level-grid
            (str "Module " (mapv (comp int /)
                                 (world-mouse-position)
                                 [module-width module-height])))
          (when area-level-grid
            (str "Creature id: " (t/property-value tiled-map :creatures tile :id)))
          (when area-level-grid
            (let [level (get area-level-grid tile)]
              (when (number? level)
                (str "Area level:" level))))
          (str "Movement properties " (t/movement-property tiled-map tile) "\n"
               (apply vector (t/movement-properties tiled-map tile)))]
         (remove nil?)
         (str/join "\n"))))

; same as debug-window
(defn- ->info-window []
  (let [label (->label "")
        window (->window {:title "Info" :rows [[label]]})]
    (add-actor! window (->actor {:act #(do
                                        (.setText label (map-infos))
                                        (.pack window))}))
    (set-position! window 0 (gui-viewport-height))
    window))

(defn- adjust-zoom [camera by] ; DRY context.game
  (set-zoom! camera (max 0.1 (+ (zoom camera) by))))

; TODO movement-speed scales with zoom value for big maps useful
(def ^:private camera-movement-speed 1)
(def ^:private zoom-speed 0.1)

; TODO textfield takes control !
; TODO PLUS symbol shift & = symbol on keyboard not registered
(defn- camera-controls [camera]
  (when (key-pressed? :keys/shift-left)
    (adjust-zoom camera    zoom-speed))
  (when (key-pressed? :keys/minus)
    (adjust-zoom camera (- zoom-speed)))
  (let [apply-position (fn [idx f]
                         (camera-set-position! camera
                                               (update (camera-position camera)
                                                       idx
                                                       #(f % camera-movement-speed))))]
    (if (key-pressed? :keys/left)  (apply-position 0 -))
    (if (key-pressed? :keys/right) (apply-position 0 +))
    (if (key-pressed? :keys/up)    (apply-position 1 +))
    (if (key-pressed? :keys/down)  (apply-position 1 -))))

#_(def ^:private show-area-level-colors true)
; TODO unused
; TODO also draw numbers of area levels big as module size...

(defn- render-on-map []
  (let [{:keys [tiled-map
                area-level-grid
                start-position
                show-movement-properties
                show-grid-lines]} @(current-data)
        visible-tiles (visible-tiles (world-camera))
        [x y] (->tile (world-mouse-position))]
    (draw-rectangle x y 1 1 :white)
    (when start-position
      (draw-filled-rectangle (start-position 0) (start-position 1) 1 1 [1 0 1 0.9]))
    ; TODO move down to other doseq and make button
    (when show-movement-properties
      (doseq [[x y] visible-tiles
              :let [movement-property (t/movement-property tiled-map [x y])]]
        (draw-filled-circle [(+ x 0.5) (+ y 0.5)] 0.08 :black)
        (draw-filled-circle [(+ x 0.5) (+ y 0.5)]
                            0.05
                            (case movement-property
                              "all"   :green
                              "air"   :orange
                              "none"  :red))))
    (when show-grid-lines
      (draw-grid 0 0 (t/width  tiled-map) (t/height tiled-map) 1 1 [1 1 1 0.5]))))

(def ^:private world-id :worlds/modules)

(defn- generate-screen-ctx [properties]
  (let [;{:keys [tiled-map area-level-grid start-position]} (generate-modules context properties)
        {:keys [tiled-map start-position]} (generate-level world-id)
        atom-data (current-data)]
    (dispose! (:tiled-map @atom-data))
    (swap! atom-data assoc
           :tiled-map tiled-map
           ;:area-level-grid area-level-grid
           :start-position start-position)
    (show-whole-map! (world-camera) tiled-map)
    (.setVisible (t/get-layer tiled-map "creatures") true)))

(defn ->generate-map-window [level-id]
  (->window {:title "Properties"
             :cell-defaults {:pad 10}
             :rows [[(->label (with-out-str
                               (clojure.pprint/pprint
                                (build-property level-id))))]
                    [(->text-button "Generate" #(try (generate-screen-ctx (build-property level-id))
                                                     (catch Throwable t
                                                       (error-window! t)
                                                       (println t))))]]
             :pack? true}))

(defcomponent ::sub-screen
  {:let current-data}
  ; TODO ?
  #_(dispose [_]
      (dispose! (:tiled-map @current-data)))

  (screen-enter [_]
    (show-whole-map! (world-camera) (:tiled-map @current-data)))

  (screen-exit [_]
    (reset-zoom! (world-camera)))

  (screen-render [_]
    (draw-tiled-map (:tiled-map @current-data) (constantly white))
    (render-world-view! render-on-map)
    (if (key-just-pressed? :keys/l)
      (swap! current-data update :show-grid-lines not))
    (if (key-just-pressed? :keys/m)
      (swap! current-data update :show-movement-properties not))
    (camera-controls (world-camera))
    (when (key-just-pressed? :keys/escape)
      (change-screen :screens/main-menu))))

(derive :screens/map-editor :screens/stage)
(defcomponent :screens/map-editor
  (->mk [_]
    {:sub-screen [::sub-screen
                  (atom {:tiled-map (t/load-map modules-file)
                         :show-movement-properties false
                         :show-grid-lines false})]
     :stage (->stage [(->generate-map-window world-id)
                      (->info-window)])}))
