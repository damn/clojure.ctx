(ns clojure.gdx.tiled
  "API for [com.badlogic.gdx.maps.tiled](https://javadoc.io/doc/com.badlogicgames.gdx/gdx/latest/com/badlogic/gdx/maps/tiled/package-summary.html) plus grid2d level generation."
  (:require [clojure.gdx.rand :refer [sshuffle srand srand-int]]
            [data.grid2d :as g])
  (:import com.badlogic.gdx.graphics.g2d.TextureRegion
           (com.badlogic.gdx.maps MapLayer MapLayers MapProperties)
           (com.badlogic.gdx.maps.tiled TmxMapLoader TiledMap TiledMapTile TiledMapTileLayer TiledMapTileLayer$Cell)
           com.badlogic.gdx.maps.tiled.tiles.StaticTiledMapTile
           (gdl OrthogonalTiledMapRenderer ColorSetter)))

(defn load-map
  "Has to be disposed."
  [file]
  (.load (TmxMapLoader.) file))

; implemented by: TiledMap, TiledMapTile, TiledMapTileLayer
(defprotocol HasProperties
  (m-props ^MapProperties [_] "Returns instance of com.badlogic.gdx.maps.MapProperties")
  (get-property [_ key] "Pass keyword key, looks up in properties."))

(defn layer-name ^String [layer]
  (if (keyword? layer)
    (name layer)
    (.getName ^MapLayer layer)))

(comment
 ; could do this but slow -> fetch directly necessary properties
 (defn properties [obj]
   (let [^MapProperties ps (.getProperties obj)]
     (zipmap (map keyword (.getKeys ps)) (.getValues ps))))

 )

(defn- lookup [has-properties key]
  (.get (m-props has-properties) (name key)))

(extend-protocol HasProperties
  TiledMap
  (m-props [tiled-map] (.getProperties tiled-map))
  (get-property [tiled-map key] (lookup tiled-map key))

  MapLayer
  (m-props [layer] (.getProperties layer))
  (get-property [layer key] (lookup layer key))

  TiledMapTile
  (m-props [tile] (.getProperties tile))
  (get-property [tile key] (lookup tile key)))

(defprotocol TMap
  (width [_])
  (height [_])
  (layers ^MapLayers [_] "Returns instance of com.badlogic.gdx.maps.MapLayers of the tiledmap")
  (layer-index [_ layer]
               "Returns nil or the integer index of the layer.
               Layer can be keyword or an instance of TiledMapTileLayer.")
  (get-layer [_ layer-name]
             "Returns the layer with name (string).")
  (remove-layer! [_ layer]
                 "Removes the layer, layer can be keyword or an actual layer object.")
  (cell-at [_ layer position] "Layer can be keyword or layer object.
                              Position vector [x y].
                              If the layer is part of tiledmap, returns the TiledMapTileLayer$Cell at position.")
  (property-value [_ layer position property-key]
                  "Returns the property value of the tile at the cell in layer.
                  If there is no cell at this position in the layer returns :no-cell.
                  If the property value is undefined returns :undefined.
                  Layer is keyword or layer object.")
  (map-positions [_] "Returns a sequence of all [x y] positions in the tiledmap.")
  (positions-with-property [_ layer property-key]
                           "If the layer (keyword or layer object) does not exist returns nil.
                           Otherwise returns a sequence of [[x y] value] for all tiles who have property-key."))

(extend-type TiledMap
  TMap
  (width  [tiled-map] (get-property tiled-map :width))
  (height [tiled-map] (get-property tiled-map :height))

  (layers [tiled-map]
    (.getLayers tiled-map))

  (layer-index [tiled-map layer]
    (let [idx (.getIndex (layers tiled-map) (layer-name layer))]
      (when-not (= idx -1)
        idx)))

  (get-layer [tiled-map layer-name]
    (.get (layers tiled-map) ^String layer-name))

  (remove-layer! [tiled-map layer]
    (.remove (layers tiled-map)
             (int (layer-index tiled-map layer))))

  (cell-at [tiled-map layer [x y]]
    (when-let [layer (get-layer tiled-map (layer-name layer))]
      (.getCell ^TiledMapTileLayer layer x y)))

  ; we want cell property not tile property
  ; so why care for no-cell ? just return nil
  (property-value [tiled-map layer position property-key]
    (assert (keyword? property-key))
    (if-let [cell (cell-at tiled-map layer position)]
      (if-let [value (get-property (.getTile ^TiledMapTileLayer$Cell cell) property-key)]
        value
        :undefined)
      :no-cell))

  (map-positions [tiled-map]
    (for [x (range (width  tiled-map))
          y (range (height tiled-map))]
      [x y]))

  (positions-with-property [tiled-map layer property-key]
    (when (layer-index tiled-map layer)
      (for [position (map-positions tiled-map)
            :let [[x y] position
                  value (property-value tiled-map layer position property-key)]
            :when (not (#{:undefined :no-cell} value))]
        [position value]))))

(def
  ^{:doc "Memoized function.
         Tiles are usually shared by multiple cells.
         https://libgdx.com/wiki/graphics/2d/tile-maps#cells
         No copied-tile for AnimatedTiledMapTile yet (there was no copy constructor/method)"}
  copy-tile
  (memoize
   (fn [^StaticTiledMapTile tile]
     (assert tile)
     (StaticTiledMapTile. tile))))

(defn ->static-tiled-map-tile [texture-region]
  (assert texture-region)
  (StaticTiledMapTile. ^TextureRegion texture-region))

(defn set-tile! [^TiledMapTileLayer layer [x y] tile]
  (let [cell (TiledMapTileLayer$Cell.)]
    (.setTile cell tile)
    (.setCell layer x y cell)))

(defn cell->tile [cell]
  (.getTile ^TiledMapTileLayer$Cell cell))

(defn add-layer! [tiled-map & {:keys [name visible properties]}]
  (let [layer (TiledMapTileLayer. (width  tiled-map)
                                  (height tiled-map)
                                  (get-property tiled-map :tilewidth)
                                  (get-property tiled-map :tileheight))]
    (.setName layer name)
    (when properties
      (.putAll ^MapProperties (m-props layer) properties))
    (.setVisible layer visible)
    (.add ^MapLayers (layers tiled-map) layer)
    layer))

(defn ->empty-tiled-map []
  (TiledMap.))

(defn put! [^MapProperties properties key value]
  (.put properties key value))

(defn put-all! [^MapProperties properties other-properties]
  (.putAll properties other-properties))

(defn visible? [^TiledMapTileLayer layer]
  (.isVisible layer))

(defn ->orthogonal-tiled-map-renderer
  "OrthogonalTiledMapRenderer extends BatchTiledMapRenderer
  and when a batch is passed to the constructor
  we do not need to dispose the renderer"
  [tiled-map unit-scale batch]
  (OrthogonalTiledMapRenderer. tiled-map (float unit-scale) batch))

(defn- set-color-setter! [^OrthogonalTiledMapRenderer map-renderer color-setter]
  (.setColorSetter map-renderer (reify ColorSetter
                                  (apply [_ color x y]
                                    (color-setter color x y)))))

(defn render-tm!
  "Renders tiled-map using camera position and with unit-scale.
  Color-setter is a (fn [color x y]) which is called for every tile-corner to set the color.
  Can be used for lights & shadows.
  Renders only visible layers."
  [^OrthogonalTiledMapRenderer map-renderer color-setter camera tiled-map]
  (set-color-setter! map-renderer color-setter)
  (.setView map-renderer camera)
  (->> tiled-map
       layers
       (filter visible?)
       (map (partial layer-index tiled-map))
       int-array
       (.render map-renderer)))

; TODO performance bottleneck -> every time getting same layers
; takes 600 ms to read movement-properties
; lazy seqs??

(defn- tile-movement-property [tiled-map layer position]
  (let [value (property-value tiled-map layer position :movement)]
    (assert (not= value :undefined)
            (str "Value for :movement at position "
                 position  " / mapeditor inverted position: " [(position 0)
                                                               (- (dec (height tiled-map))
                                                                  (position 1))]
                 " and layer " (layer-name layer) " is undefined."))
    (when-not (= :no-cell value)
      value)))

(defn- movement-property-layers [tiled-map]
  (filter #(get-property % :movement-properties)
          (reverse
           (layers tiled-map))))

(defn movement-properties [tiled-map position]
  (for [layer (movement-property-layers tiled-map)]
    [(layer-name layer)
     (tile-movement-property tiled-map layer position)]))

(defn movement-property [tiled-map position]
  (or (->> tiled-map
           movement-property-layers
           (some #(tile-movement-property tiled-map % position)))
      "none"))

(defn grid->tiled-map
  "Creates an empty new tiled-map with same layers and properties as schema-tiled-map.
  The size of the map is as of the grid, which contains also the tile information from the schema-tiled-map."
  [schema-tiled-map grid]
  (let [tiled-map (->empty-tiled-map)
        properties (m-props tiled-map)]
    (put-all! properties (m-props schema-tiled-map))
    (put! properties "width"  (g/width  grid))
    (put! properties "height" (g/height grid))
    (doseq [layer (layers schema-tiled-map)
            :let [new-layer (add-layer! tiled-map
                                          :name (layer-name layer)
                                          :visible (visible? layer)
                                          :properties (m-props layer))]]
      (doseq [position (g/posis grid)
              :let [local-position (get grid position)]
              :when local-position]
        (when (vector? local-position)
          (when-let [cell (cell-at schema-tiled-map layer local-position)]
            (set-tile! new-layer
                         position
                         (copy-tile (cell->tile cell)))))))
    tiled-map))

(defn wgt-grid->tiled-map [grid position->tile]
  (let [tiled-map (->empty-tiled-map)
        properties (m-props tiled-map)]
    (put! properties "width"  (g/width  grid))
    (put! properties "height" (g/height grid))
    (put! properties "tilewidth" 48)
    (put! properties "tileheight" 48)
    (let [layer (add-layer! tiled-map :name "ground" :visible true)
          properties (m-props layer)]
      (put! properties "movement-properties" true)
      (doseq [position (g/posis grid)
              :let [value (get grid position)
                    cell (cell-at tiled-map layer position)]]
        (set-tile! layer position (position->tile position))))
    tiled-map))

(defn scale-grid [grid [w h]]
  (g/create-grid (* (g/width grid)  w)
                 (* (g/height grid) h)
                 (fn [[x y]]
                   (get grid
                        [(int (/ x w))
                         (int (/ y h))]))))

(defn scalegrid [grid factor]
  (g/create-grid (* (g/width grid) factor)
                 (* (g/height grid) factor)
                 (fn [posi]
                   (get grid (mapv #(int (/ % factor)) posi)))))
; TODO other keys in the map-grid are lost -> look where i transform grids like this
; merge with ld-grid?

(defn create-borders-positions [grid] ; TODO not distinct -> apply distinct or set
  (let [w (g/width grid),h (g/height grid)]
    (concat
      (mapcat (fn [x] [[x 0] [x (dec h)]]) (range w))
      (mapcat (fn [y] [[0 y] [(dec w) y]]) (range h)))))

(defn get-3x3-cellvalues [grid posi]
  (map grid (cons posi (g/get-8-neighbour-positions posi))))

(defn not-border-position? [[x y] grid]
  (and (>= x 1) (>= y 1)
       (< x (dec (g/width grid)))
       (< y (dec (g/height grid)))))

(defn border-position? [p grid] (not (not-border-position? p grid)))

(defn wall-at? [grid posi]
  (= :wall (get grid posi)))

(defn undefined-value-behind-walls
  "also border positions set to undefined where there are nil values"
  [grid]
  (g/transform grid
               (fn [posi value]
                 (if (and (= :wall value)
                          (every? #(let [value (get grid %)]
                                     (or (= :wall value) (nil? value)))
                                  (g/get-8-neighbour-positions posi)))
                   :undefined
                   value))))

; if no tile
; and some has tile at get-8-neighbour-positions
; -> should be a wall
; -> paint in tiled editor set tile at cell and layer
; -> texture ?
; spritesheet already available ?!

(defn fill-single-cells [grid] ; TODO removes single walls without adjacent walls
  (g/transform grid
               (fn [posi value]
                 (if (and (not-border-position? posi grid)
                          (= :wall value)
                          (not-any? #(wall-at? grid %)
                                    (g/get-4-neighbour-positions posi)))
                   :ground
                   value))))

(defn- print-cell [celltype]
  (print (if (number? celltype)
           celltype
           (case celltype
             nil               "?"
             :undefined        " "
             :ground           "_"
             :wall             "#"
             :airwalkable      "."
             :module-placement "X"
             :start-module     "@"
             :transition       "+"))))

; print-grid in data.grid2d is y-down
(defn printgrid
  "Prints with y-up coordinates."
  [grid]
  (doseq [y (range (dec (g/height grid)) -1 -1)]
    (doseq [x (range (g/width grid))]
      (print-cell (grid [x y])))
    (println)))

(let [idxvalues-order [[1 0] [-1 0] [0 1] [0 -1]]]
  (assert (= (g/get-4-neighbour-positions [0 0])
             idxvalues-order)))

(comment
  ; Values for every neighbour:
  {          [0 1] 1
   [-1 0]  8          [1 0] 2
             [0 -1] 4 })

; so the idxvalues-order corresponds to the following values for a neighbour tile:
(def ^:private idxvalues [2 8 1 4])

(defn- calculate-index-value [position->transition? idx position]
  (if (position->transition? position)
    (idxvalues idx)
    0))

(defn transition-idx-value [position position->transition?]
  (->> position
       g/get-4-neighbour-positions
       (map-indexed (partial calculate-index-value
                             position->transition?))
       (apply +)))

(defn- nad-corner? [grid [fromx fromy] [tox toy]]
  (and
    (= :ground (get grid [tox toy])) ; also filters nil/out of map
    (wall-at? grid [tox fromy])
    (wall-at? grid [fromx toy])))

(def ^:private diagonal-steps [[-1 -1] [-1 1] [1 -1] [1 1]])

; TODO could be made faster because accessing the same posis oftentimes at nad-corner? check
(defn get-nads [grid]
  (loop [checkposis (filter (fn [{y 1 :as posi}]
                              (and (even? y)
                                   (= :ground (get grid posi))))
                            (g/posis grid))
         result []]
    (if (seq checkposis)
      (let [position (first checkposis)
            diagonal-posis (map #(mapv + position %) diagonal-steps)
            nads (map (fn [nad] [position nad])
                      (filter #(nad-corner? grid position %) diagonal-posis))]
        (recur
          (rest checkposis)
          (doall (concat result nads)))) ; doall else stackoverflow error
      result)))

(defn- get-tiles-needing-fix-for-nad [grid [[fromx fromy]
                                           [tox toy]]]
  (let [xstep (- tox fromx)
        ystep (- toy fromy)
        cell1x (+ fromx xstep)
        cell1y fromy
        cell1 [cell1x cell1y]
        cell11 [(+ cell1x xstep) (+ cell1y (- ystep))]
        cell2x (+ cell1x xstep)
        cell2y cell1y
        cell2 [cell2x cell2y]
        cell21 [(+ cell2x xstep) (+ cell2y ystep)]
        cell3 [cell2x (+ cell2y ystep)]]
;    (println "from: " [fromx fromy] " to: " [tox toy])
;    (println "xstep " xstep " ystep " ystep)
;    (println "cell1 " cell1)
;    (println "cell11 " cell11)
;    (println "cell2 " cell2)
;    (println "cell21 " cell21)
;    (println "cell3 " cell3)
    (if-not (nad-corner? grid cell1 cell11)
      [cell1]
      (if-not (nad-corner? grid cell2 cell21)
        [cell1 cell2]
        [cell1 cell2 cell3]))))

(defn assoc-ks [m ks v]
  (if (empty? ks)
    m
    (apply assoc m (interleave ks (repeat v)))))

(defn mark-nads [grid nads label]
  (assoc-ks grid (mapcat #(get-tiles-needing-fix-for-nad grid %) nads) label))

(defn fix-not-allowed-diagonals [grid]
  (mark-nads grid (get-nads grid) :ground))

;; TEST

(comment
  (def found (atom false))

  (defn search-buggy-nads []
    (println "searching buggy nads")
    (doseq [n (range 100000)
            :when (not @found)]
      (println "try " n)
      (let [grid (cellular-automata-gridgen 100 80 :fillprob 62 :generations 0 :wall-borders true)
            nads (get-nads grid)
            fixed-grid (mark-nads grid nads :ground)]
        (when
          (and
            (not (zero? (count nads)))
            (not (zero? (count (get-nads fixed-grid)))))
          (println "found!")
          (reset! found [grid fixed-grid]))))
    (println "found buggy nads? " @found)))

;Cave Algorithmus.
;http://properundead.com/2009/03/cave-generator.html
;http://properundead.com/2009/07/procedural-generation-3-cave-source.html
;http://forums.tigsource.com/index.php?topic=5174.0

(defn- create-order [random]
  (sshuffle (range 4) random))

(defn- get-in-order [v order]
  (map #(get v %) order))

(def ^:private current-order (atom nil))

(def ^:private turn-ratio 0.25)

(defn- create-rand-4-neighbour-posis [posi n random] ; TODO does more than 1 thing
  (when (< (srand random) turn-ratio)
    (reset! current-order (create-order random)))
  (take n
        (get-in-order (g/get-4-neighbour-positions posi)
                      @current-order)))

(defn- get-default-adj-num [open-paths random]
  (if (= open-paths 1)
    (case (int (srand-int 4 random))
      0 1
      1 1
      2 1
      3 2
      1)
    (case (int (srand-int 4 random))
      0 0
      1 1
      2 1
      3 2
      1)))

(defn- get-thin-adj-num [open-paths random]
  (if (= open-paths 1)
    1
    (case (int (srand-int 7 random))
      0 0
      1 2
      1)))

(defn- get-wide-adj-num [open-paths random]
  (if (= open-paths 1)
    (case (int (srand-int 3 random))
      0 1
      2)
    (case (int (srand-int 4 random))
      0 1
      1 2
      2 3
      3 4
      1)))

(def ^:private get-adj-num
  {:wide    get-wide-adj-num
   :thin    get-thin-adj-num    ; h�hle mit breite 1 �berall nur -> turn-ratio verringern besser
   :default get-default-adj-num}) ; etwas breiter als 1 aber immernoch zu d�nn f�r m ein game -> turn-ratio verringern besser

; gute ergebnisse: :wide / 500-4000 max-cells / turn-ratio 0.5
; besser 150x150 anstatt 100x100 w h
; TODO glaubich einziger unterschied noch: openpaths wird bei jeder cell neu berechnet?
; TODO max-tries wenn er nie �ber min-cells kommt? -> im let dazu definieren vlt max 30 sekunden -> in tries umgerechnet??
(defn cave-gridgen [random min-cells max-cells adjnum-type]
  ; move up where its used only
  (reset! current-order (create-order random))
  (let [start [0 0]
        start-grid (assoc {} start :ground) ; grid of posis to :ground or no entry for walls
        finished (fn [grid end cell-cnt]
                   ;(println "Reached cells: " cell-cnt) ; TODO cell-cnt stimmt net genau
                   ; TODO already called there down ... make mincells check there
                   (if (< cell-cnt min-cells)
                     (cave-gridgen random min-cells max-cells adjnum-type) ; recur?
                     (let [[grid convert] (g/mapgrid->vectorgrid grid #(if (nil? %) :wall :ground))]
                       {:grid  grid
                        :start (convert start)
                        :end   (convert end)})))]
    (loop [posi-seq [start]
           grid     start-grid
           cell-cnt 0]
      ; TODO min cells check !?
      (if (>= cell-cnt max-cells)
        (finished grid
                  (last posi-seq)
                  cell-cnt)
        (let [try-carve-posis (create-rand-4-neighbour-posis
                                (last posi-seq) ; TODO take random ! at corner ... hmm
                                ((get-adj-num adjnum-type) (count posi-seq) random)
                                random)
              carve-posis (filter #(nil? (get grid %)) try-carve-posis)
              new-pos-seq (concat (drop-last posi-seq) carve-posis)]
          (if (not-empty new-pos-seq)
            (recur new-pos-seq
                   (if (seq carve-posis)
                     (assoc-ks grid carve-posis :ground)
                     grid)
                   (+ cell-cnt (count carve-posis)))
            ; TODO here min-cells check ?
            (finished grid (last posi-seq) cell-cnt)))))))

; can adjust:
; * split percentage , for higher level areas may scale faster (need to be more careful)
; * not 4 neighbors but just 1 tile randomwalk -> possible to have lvl 9 area next to lvl 1 ?
; * adds metagame to the game , avoid/or fight higher level areas, which areas to go next , etc...
; -> up to the player not step by step level increase like D2
; can not only take first of added-p but multiples also
; can make parameter how fast it scales
; area-level-grid works better with more wide grids
; if the cave is very straight then it is just a continous progression and area-level-grid is useless
(defn ->area-level-grid
  "Expands from start position by adding one random adjacent neighbor.
  Each random walk is a step and is assigned a level as of max-level.
  (Levels are scaled, for example grid has 100 ground cells, so steps would be 0 to 100(99?)
  and max-level will smooth it out over 0 to max-level.
  The point of this is to randomize the levels so player does not have a smooth progression
  but can encounter higher level areas randomly around but there is always a path which goes from
  level 0 to max-level, so the player has to decide which areas to do in which order."
  [& {:keys [grid start max-level walk-on]}]
  (let [maxcount (->> grid
                      g/cells
                      (filter walk-on)
                      count)
        ; -> assume all :ground cells can be reached from start
        ; later check steps count == maxcount assert
        level-step (/ maxcount max-level)
        step->level #(int (Math/ceil (/ % level-step)))
        walkable-neighbours (fn [grid position]
                              (filter #(walk-on (get grid %))
                                      (g/get-4-neighbour-positions position)))]
    (loop [next-positions #{start}
           steps          [[0 start]]
           grid           (assoc grid start 0)]
      (let [next-positions (set
                            (filter #(seq (walkable-neighbours grid %))
                                    next-positions))]
        (if (seq next-positions)
          (let [p (rand-nth (seq next-positions))
                added-p (rand-nth (walkable-neighbours grid p))]
            (if added-p
              (let [area-level (step->level (count steps))]
                (recur (conj next-positions added-p)
                       (conj steps [area-level added-p])
                       (assoc grid added-p area-level)))
              (recur next-positions
                     steps
                     grid)))
          {:steps steps
           :area-level-grid grid})))))
