(ns core.world
  (:require [clojure.gdx :refer :all]
            [clojure.gdx.tiled :as t]
            [clojure.gdx.rand :refer [get-rand-weighted-item]]
            [clojure.string :as str]
            [data.grid2d :as g]))

(def modules-file "maps/modules.tmx")
(def module-width  32)
(def module-height 20)
(def modules-scale [module-width module-height])

(def ^:private number-modules-x 8)
(def ^:private number-modules-y 4)
(def ^:private module-offset-tiles 1)
(def ^:private transition-modules-row-width 4)
(def ^:private transition-modules-row-height 4)
(def ^:private transition-modules-offset-x 4)
(def ^:private floor-modules-row-width 4)
(def ^:private floor-modules-row-height 4)
(def ^:private floor-idxvalue 0)

(defn- module-index->tiled-map-positions [[module-x module-y]]
  (let [start-x (* module-x (+ module-width  module-offset-tiles))
        start-y (* module-y (+ module-height module-offset-tiles))]
    (for [x (range start-x (+ start-x module-width))
          y (range start-y (+ start-y module-height))]
      [x y])))

(defn- floor->module-index []
  [(rand-int floor-modules-row-width)
   (rand-int floor-modules-row-height)])

(defn- transition-idxvalue->module-index [idxvalue]
  [(+ (rem idxvalue transition-modules-row-width)
      transition-modules-offset-x)
   (int (/ idxvalue transition-modules-row-height))])

(defn- place-module [scaled-grid
                     unscaled-position
                     & {:keys [transition?
                               transition-neighbor?]}]
  (let [idxvalue (if transition?
                   (t/transition-idx-value unscaled-position transition-neighbor?)
                   floor-idxvalue)
        tiled-map-positions (module-index->tiled-map-positions
                             (if transition?
                               (transition-idxvalue->module-index idxvalue)
                               (floor->module-index)))
        offsets (for [x (range module-width)
                      y (range module-height)]
                  [x y])
        offset->tiled-map-position (zipmap offsets tiled-map-positions)
        scaled-position (mapv * unscaled-position modules-scale)]
    (reduce (fn [grid offset]
              (assoc grid
                     (mapv + scaled-position offset)
                     (offset->tiled-map-position offset)))
            scaled-grid
            offsets)))

(defn place-modules [modules-tiled-map
                     scaled-grid
                     unscaled-grid
                     unscaled-floor-positions
                     unscaled-transition-positions]
  (let [_ (assert (and (= (t/width modules-tiled-map)
                          (* number-modules-x (+ module-width module-offset-tiles)))
                       (= (t/height modules-tiled-map)
                          (* number-modules-y (+ module-height module-offset-tiles)))))
        scaled-grid (reduce (fn [scaled-grid unscaled-position]
                              (place-module scaled-grid unscaled-position :transition? false))
                            scaled-grid
                            unscaled-floor-positions)
        scaled-grid (reduce (fn [scaled-grid unscaled-position]
                              (place-module scaled-grid unscaled-position :transition? true
                                            :transition-neighbor? #(#{:transition :wall}
                                                                    (get unscaled-grid %))))
                            scaled-grid
                            unscaled-transition-positions)]
    (t/grid->tiled-map modules-tiled-map scaled-grid)))

; TODO generates 51,52. not max 50
; TODO can use different turn-ratio/depth/etc. params
; (printgrid (:grid (->cave-grid :size 800)))
(defn- ->cave-grid [& {:keys [size]}]
  (let [{:keys [start grid]} (t/cave-gridgen (java.util.Random.) size size :wide)
        grid (t/fix-not-allowed-diagonals grid)]
    (assert (= #{:wall :ground} (set (g/cells grid))))
    {:start start
     :grid grid}))

; TODO HERE
; * unique max 16 modules, not random take @ #'floor->module-index, also special start, end modules, rare modules...

; * at the beginning enemies very close, different area different spawn-rate !

; beginning slow enemies low hp low dmg etc.

; * flood-fill gets 8 neighbour posis -> no NADs on modules ! assert !

; * assuming bottom left in floor module is walkable

; whats the assumption here? => or put extra borders around? / assert!

(defn- adjacent-wall-positions [grid]
  (filter (fn [p] (and (= :wall (get grid p))
                       (some #(= :ground (get grid %))
                             (g/get-8-neighbour-positions p))))
          (g/posis grid)))

(defn- flood-fill [grid start walk-on-position?]
  (loop [next-positions [start]
         filled []
         grid grid]
    (if (seq next-positions)
      (recur (filter #(and (get grid %)
                           (walk-on-position? %))
                     (distinct
                      (mapcat g/get-8-neighbour-positions
                              next-positions)))
             (concat filled next-positions)
             (t/assoc-ks grid next-positions nil))
      filled)))

(comment
 (let [{:keys [start grid]} (t/->cave-grid :size 15)
       _ (println "BASE GRID:\n")
       _ (printgrid grid)
       ;_ (println)
       ;_ (println "WITH START POSITION (0) :\n")
       ;_ (printgrid (assoc grid start 0))
       ;_ (println "\nwidth:  " (g/width  grid)
       ;           "height: " (g/height grid)
       ;           "start " start "\n")
       ;_ (println (g/posis grid))
       _ (println "\n\n")
       filled (flood-fill grid start (fn [p] (= :ground (get grid p))))
       _ (printgrid (reduce #(assoc %1 %2 nil) grid filled))])
 )


(defn- creatures-with-level [creature-properties level]
  (filter #(= level (:creature/level %)) creature-properties))

(def ^:private creature->tile
  (memoize
   (fn [{:keys [property/id] :as prop}]
     (assert id)
     (let [image (prop->image prop)
           tile (t/->static-tiled-map-tile (:texture-region image))]
       (t/put! (t/m-props tile) "id" id)
       tile))))

(def ^:private spawn-creatures? true)

(defn- place-creatures! [spawn-rate tiled-map spawn-positions area-level-grid]
  (let [layer (t/add-layer! tiled-map :name "creatures" :visible false)
        creature-properties (all-properties :properties/creatures)]
    (when spawn-creatures?
      (doseq [position spawn-positions
              :let [area-level (get area-level-grid position)]
              :when (and (number? area-level)
                         (<= (rand) spawn-rate))]
        (let [creatures (creatures-with-level creature-properties area-level)]
          (when (seq creatures)
            (t/set-tile! layer position (creature->tile (rand-nth creatures)))))))))

(defn generate-modules
  "The generated tiled-map needs to be disposed."
  [{:keys [world/map-size
           world/max-area-level
           world/spawn-rate]}]
  (assert (<= max-area-level map-size))
  (let [{:keys [start grid]} (->cave-grid :size map-size)
        ;_ (printgrid grid)
        ;_ (println " - ")
        grid (reduce #(assoc %1 %2 :transition) grid (adjacent-wall-positions grid))
        ;_ (printgrid grid)
        ;_ (println " - ")
        _ (assert (or
                   (= #{:wall :ground :transition} (set (g/cells grid)))
                   (= #{:ground :transition} (set (g/cells grid))))
                  (str "(set (g/cells grid)): " (set (g/cells grid))))
        scale modules-scale
        scaled-grid (t/scale-grid grid scale)
        tiled-map (place-modules (t/load-map modules-file)
                                 scaled-grid
                                 grid
                                 (filter #(= :ground     (get grid %)) (g/posis grid))
                                 (filter #(= :transition (get grid %)) (g/posis grid)))
        start-position (mapv * start scale)
        can-spawn? #(= "all" (t/movement-property tiled-map %))
        _ (assert (can-spawn? start-position)) ; assuming hoping bottom left is movable
        spawn-positions (flood-fill scaled-grid start-position can-spawn?)
        ;_ (println "scaled grid with filled nil: '?' \n")
        ;_ (printgrid (reduce #(assoc %1 %2 nil) scaled-grid spawn-positions))
        ;_ (println "\n")
        {:keys [steps area-level-grid]} (t/->area-level-grid :grid grid
                                                             :start start
                                                             :max-level max-area-level
                                                             :walk-on #{:ground :transition})
        ;_ (printgrid area-level-grid)
        _ (assert (or
                   (= (set (concat [max-area-level] (range max-area-level)))
                      (set (g/cells area-level-grid)))
                   (= (set (concat [:wall max-area-level] (range max-area-level)))
                      (set (g/cells area-level-grid)))))
        scaled-area-level-grid (t/scale-grid area-level-grid scale)
        get-free-position-in-area-level (fn [area-level]
                                          (rand-nth
                                           (filter
                                            (fn [p]
                                              (and (= area-level (get scaled-area-level-grid p))
                                                   (#{:no-cell :undefined}
                                                    (t/property-value tiled-map :creatures p :id))))
                                            spawn-positions)))]
    (place-creatures! spawn-rate tiled-map spawn-positions scaled-area-level-grid)
    {:tiled-map tiled-map
     :start-position (get-free-position-in-area-level 0)
     :area-level-grid scaled-area-level-grid}))

(defn uf-transition [position grid]
  (t/transition-idx-value position (= :transition (get grid position))))

(defn rand-0-3 []
  (get-rand-weighted-item {0 60 1 1 2 1 3 1}))

(defn rand-0-5 []
  (get-rand-weighted-item {0 30 1 1 2 1 3 1 4 1 5 1}))

; TODO zoomed out see that line of sight raycast goes x screens away
; only becuz of zoom?

; Level:
; * ground textures
; * wall textures
; * doodads ?
; * creatures w. that lvl
; (skills/items)
; * level-name (Goblin Lair , Halfling Village, Demon Kingdom)
; * spawn-rate
; * level-size
; * to finish lvl maybe find 3-4 signs to activate (takes some time) to open a portal
; every sign will increase spawn rate (maybe 0 at beginning -> can keep spawning)

; can use different algorithms(e.g. cave, module-gen-uf-terrain, room-gen? , differnt cave algorithm ...)

(defn- uf-place-creatures! [spawn-rate tiled-map spawn-positions]
  (let [layer (t/add-layer! tiled-map :name "creatures" :visible false)
        creatures (all-properties :properties/creatures)
        level (inc (rand-int 6))
        creatures (creatures-with-level creatures level)]
    ;(println "Level: " level)
    ;(println "Creatures with level: " (count creatures))
    (doseq [position spawn-positions
            :when (<= (rand) spawn-rate)]
      (t/set-tile! layer position (creature->tile (rand-nth creatures))))))

(def ^:private ->tm-tile
  (memoize
   (fn ->tm-tile [texture-region movement]
     {:pre [#{"all" "air" "none"} movement]}
     (let [tile (t/->static-tiled-map-tile texture-region)]
       (t/put! (t/m-props tile) "movement" movement)
       tile))))

(def ^:private sprite-size 48)

(defn- terrain-texture-region []
  (->texture-region "maps/uf_terrain.png"))

(defn- ->uf-tile [& {:keys [sprite-x sprite-y movement]}]
  (->tm-tile (->texture-region (terrain-texture-region)
                               [(* sprite-x sprite-size)
                                (* sprite-y sprite-size)
                                sprite-size
                                sprite-size])
             movement))

; TODO unused
(def ^:private ground-sprites [1 (range 5 11)])

(def ^:private uf-grounds
  (for [x [1 5]
        y (range 5 11)
        :when (not= [x y] [5 5])] ;wooden
    [x y]))

(def ^:private uf-walls
  (for [x [1]
        y [13,16,19,22,25,28]]
    [x y]))

(defn- ->ground-tile [[x y]]
  (->uf-tile :sprite-x (+ x (rand-0-3)) :sprite-y y :movement "all"))

(defn- ->wall-tile [[x y]]
  (->uf-tile :sprite-x (+ x (rand-0-5)) :sprite-y y :movement "none"))

(defn- ->transition-tile [[x y]]
  (->uf-tile :sprite-x (+ x (rand-0-5)) :sprite-y y :movement "none"))

(defn- transition? [grid [x y]]
  (= :ground (get grid [x (dec y)])))

(def ^:private uf-caves-scale 4)

(defn uf-caves [{:keys [world/map-size world/spawn-rate]}]
  (let [{:keys [start grid]} (->cave-grid :size map-size)
        ;_ (println "Start: " start)
        ;_ (printgrid grid)
        ;_ (println)
        scale uf-caves-scale
        grid (t/scalegrid grid scale)
        ;_ (printgrid grid)
        ;_ (println)
        start-position (mapv #(* % scale) start)
        grid (reduce #(assoc %1 %2 :transition) grid
                     (adjacent-wall-positions grid))
        _ (assert (or
                   (= #{:wall :ground :transition} (set (g/cells grid)))
                   (= #{:ground :transition}       (set (g/cells grid))))
                  (str "(set (g/cells grid)): " (set (g/cells grid))))
        ;_ (printgrid grid)
        ;_ (println)
        ground-idx (rand-nth uf-grounds)
        {wall-x 0 wall-y 1 :as wall-idx} (rand-nth uf-walls)
        transition-idx  [wall-x (inc wall-y)]
        position->tile (fn [position]
                         (case (get grid position)
                           :wall (->wall-tile wall-idx)
                           :transition (if (transition? grid position)
                                         (->transition-tile transition-idx)
                                         (->wall-tile wall-idx))
                           :ground (->ground-tile ground-idx)))
        tiled-map (t/wgt-grid->tiled-map grid position->tile)
        can-spawn? #(= "all" (t/movement-property tiled-map %))
        _ (assert (can-spawn? start-position)) ; assuming hoping bottom left is movable
        spawn-positions (flood-fill grid start-position can-spawn?)
        ]
    ; TODO don't spawn my faction vampire w. items ...
    ; TODO don't spawn creatures on start position
    ; (all check have HP/movement..../?? ?) (breaks potential field, targeting, ...)
    (uf-place-creatures! spawn-rate tiled-map spawn-positions)
    {:tiled-map tiled-map
     :start-position start-position}))

(defc :world/player-creature {:data :some #_[:one-to-one :properties/creatures]})

(defc :world/map-size {:data :pos-int})
(defc :world/max-area-level {:data :pos-int}) ; TODO <= map-size !?
(defc :world/spawn-rate {:data :pos}) ; TODO <1 !

(defc :world/tiled-map {:data :string})

(defc :world/components {:data [:map []]})

(defc :world/generator {:data [:enum [:world.generator/tiled-map
                                      :world.generator/modules
                                      :world.generator/uf-caves]]})

(def-property-type :properties/worlds
  {:schema [:world/generator
            :world/player-creature
            [:world/tiled-map {:optional true}]
            [:world/map-size {:optional true}]
            [:world/max-area-level {:optional true}]
            [:world/spawn-rate {:optional true}]]
   :overview {:title "Worlds"
              :columns 10}})

(defmulti generate (fn [world] (:world/generator world)))

(defmethod generate :world.generator/tiled-map [world]
  {:tiled-map (t/load-map (:world/tiled-map world))
   :start-position [32 71]})

(defmethod generate :world.generator/modules [world]
  (generate-modules world))

(defmethod generate :world.generator/uf-caves [world]
  (uf-caves world))

(defn generate-level [world-id]
  (let [prop (build-property world-id)]
    (assoc (generate prop) :world/player-creature (:world/player-creature prop))))

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

(defc ::sub-screen
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
(defc :screens/map-editor
  (->mk [_]
    {:sub-screen [::sub-screen
                  (atom {:tiled-map (t/load-map modules-file)
                         :show-movement-properties false
                         :show-grid-lines false})]
     :stage (->stage [(->generate-map-window world-id)
                      (->info-window)])}))
