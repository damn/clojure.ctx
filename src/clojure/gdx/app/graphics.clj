(in-ns 'clojure.gdx)

(defn delta-time
  "the time span between the current frame and the last frame in seconds.

  `returns float`

  [javadoc](https://javadoc.io/static/com.badlogicgames.gdx/gdx/1.12.1/com/badlogic/gdx/Graphics.html#getDeltaTime())"
  []
  (.getDeltaTime Gdx/graphics))

(defn frames-per-second
  "the average number of frames per second

  `returns int`

  [javadoc](https://javadoc.io/static/com.badlogicgames.gdx/gdx/1.12.1/com/badlogic/gdx/Graphics.html#getFramesPerSecond())"
  []
  (.getFramesPerSecond Gdx/graphics))

(defn ->color
  "[javadoc](https://javadoc.io/static/com.badlogicgames.gdx/gdx/1.12.1/com/badlogic/gdx/graphics/Color.html#%3Cinit%3E(float,float,float,float))"
  ([r g b]
   (->color r g b 1))
  ([r g b a]
   (Color. (float r) (float g) (float b) (float a))))

(defn camera-position
  "Returns camera position as [x y] vector."
  [^Camera camera]
  [(.x (.position camera))
   (.y (.position camera))])

(defn camera-set-position!
  "Sets x and y and calls update on the camera."
  [^Camera camera [x y]]
  (set! (.x (.position camera)) (float x))
  (set! (.y (.position camera)) (float y))
  (.update camera))

(defn frustum [^Camera camera]
  (let [frustum-points (for [^Vector3 point (take 4 (.planePoints (.frustum camera)))
                             :let [x (.x point)
                                   y (.y point)]]
                         [x y])
        left-x   (apply min (map first  frustum-points))
        right-x  (apply max (map first  frustum-points))
        bottom-y (apply min (map second frustum-points))
        top-y    (apply max (map second frustum-points))]
    [left-x right-x bottom-y top-y]))

(defn visible-tiles [camera]
  (let [[left-x right-x bottom-y top-y] (frustum camera)]
    (for  [x (range (int left-x)   (int right-x))
           y (range (int bottom-y) (+ 2 (int top-y)))]
      [x y])))

(defn calculate-zoom
  "calculates the zoom value for camera to see all the 4 points."
  [^Camera camera & {:keys [left top right bottom]}]
  (let [viewport-width  (.viewportWidth  camera)
        viewport-height (.viewportHeight camera)
        [px py] (camera-position camera)
        px (float px)
        py (float py)
        leftx (float (left 0))
        rightx (float (right 0))
        x-diff (max (- px leftx) (- rightx px))
        topy (float (top 1))
        bottomy (float (bottom 1))
        y-diff (max (- topy py) (- py bottomy))
        vp-ratio-w (/ (* x-diff 2) viewport-width)
        vp-ratio-h (/ (* y-diff 2) viewport-height)
        new-zoom (max vp-ratio-w vp-ratio-h)]
    new-zoom))

(defn zoom [^OrthographicCamera camera]
  (.zoom camera))

(defn set-zoom!
  "Sets the zoom value and updates."
  [^OrthographicCamera camera amount]
  (set! (.zoom camera) amount)
  (.update camera))

(defn reset-zoom!
  "Sets the zoom value to 1."
  [camera]
  (set-zoom! camera 1))

(defn- ->camera ^OrthographicCamera [] (OrthographicCamera.))

; touch coordinates are y-down, while screen coordinates are y-up
; so the clamping of y is reverse, but as black bars are equal it does not matter
(defn- unproject-mouse-posi
  "Returns vector of [x y]."
  [^Viewport viewport]
  (let [mouse-x (clamp (.getX Gdx/input)
                       (.getLeftGutterWidth viewport)
                       (.getRightGutterX viewport))
        mouse-y (clamp (.getY Gdx/input)
                       (.getTopGutterHeight viewport)
                       (.getTopGutterY viewport))
        coords (.unproject viewport (Vector2. mouse-x mouse-y))]
    [(.x coords) (.y coords)]))

(defn- ->gui-viewport [world-width world-height]
  (FitViewport. world-width world-height (->camera)))

(defn- ->world-viewport [world-width world-height unit-scale]
  (let [world-width  (* world-width  unit-scale)
        world-height (* world-height unit-scale)
        camera (->camera)
        y-down? false]
    (.setToOrtho camera y-down? world-width world-height)
    (FitViewport. world-width world-height camera)))

(defn- vp-world-width  [^Viewport vp] (.getWorldWidth  vp))
(defn- vp-world-height [^Viewport vp] (.getWorldHeight vp))
(defn- vp-camera       [^Viewport vp] (.getCamera      vp))
(defn- vp-update!      [^Viewport vp [w h] & {:keys [center-camera?]}]
  (.update vp w h (boolean center-camera?)))

(defn ->texture-region
  ([path-or-texture]
   (let [^Texture tex (if (string? path-or-texture)
                        (asset path-or-texture)
                        path-or-texture)]
     (TextureRegion. tex)))

  ([^TextureRegion texture-region [x y w h]]
   (TextureRegion. texture-region (int x) (int y) (int w) (int h))))

(defn texture-region-dimensions [^TextureRegion texture-region]
  [(.getRegionWidth  texture-region)
   (.getRegionHeight texture-region)])

(defn- ->sprite-batch [] (SpriteBatch.))

; TODO [x y] is center or left-bottom ?
; why rotation origin calculations ?!
(defn- draw-texture-region [^Batch batch texture-region [x y] [w h] rotation color]
  (if color (.setColor batch color)) ; TODO move out, simplify ....
  (.draw batch
         texture-region
         x
         y
         (/ (float w) 2) ; rotation origin
         (/ (float h) 2)
         w ; width height
         h
         1 ; scaling factor
         1
         rotation)
  (if color (.setColor batch Color/WHITE)))

(defn- draw-with! [^Batch batch ^Viewport viewport draw-fn]
  (.setColor batch Color/WHITE) ; fix scene2d.ui.tooltip flickering
  (.setProjectionMatrix batch (.combined (.getCamera viewport)))
  (.begin batch)
  (draw-fn)
  (.end batch))

(defn- ->shape-drawer-texture ^Texture []
  (let [pixmap (doto (Pixmap. 1 1 Pixmap$Format/RGBA8888)
                 (.setColor Color/WHITE)
                 (.drawPixel 0 0))
        tex (Texture. pixmap)]
    (dispose! pixmap)
    tex))

(defn- ->shape-drawer [batch]
  (let [tex (->shape-drawer-texture)]
    {:shape-drawer (ShapeDrawer. batch (TextureRegion. tex 1 0 1 1))
     :shape-drawer-texture tex}))

(defn- kw->color [k] (->gdx-field "graphics.Color" k))

(comment
 (and (= Color/WHITE      (kw->color :white))
      (= Color/LIGHT_GRAY (kw->color :light-gray)))
 )

(def white Color/WHITE)
(def black Color/BLACK)

(defn- munge-color ^Color [color]
  (cond
   (= Color (class color)) color
   (keyword? color) (kw->color color)
   (vector? color) (apply ->color color)
   :else (throw (ex-info "Cannot understand color" {:color color}))))

(defn def-markup-color
  "A general purpose class containing named colors that can be changed at will. For example, the markup language defined by the BitmapFontCache class uses this class to retrieve colors and the user can define his own colors.

  [javadoc](https://javadoc.io/doc/com.badlogicgames.gdx/gdx/latest/com/badlogic/gdx/graphics/Colors.html)"
  [name-str color]
  (Colors/put name-str (munge-color color)))

(defn- set-color!          [^ShapeDrawer sd color] (.setColor sd (munge-color color)))
(defn- sd-ellipse          [^ShapeDrawer sd [x y] radius-x radius-y] (.ellipse sd (float x) (float y) (float radius-x) (float radius-y)))
(defn- sd-filled-ellipse   [^ShapeDrawer sd [x y] radius-x radius-y] (.filledEllipse sd (float x) (float y) (float radius-x) (float radius-y)))
(defn- sd-circle           [^ShapeDrawer sd [x y] radius] (.circle sd (float x) (float y) (float radius)))
(defn- sd-filled-circle    [^ShapeDrawer sd [x y] radius] (.filledCircle sd (float x) (float y) (float radius)))
(defn- sd-arc              [^ShapeDrawer sd [centre-x centre-y] radius start-angle degree] (.arc sd centre-x centre-y radius (degree->radians start-angle) (degree->radians degree)))
(defn- sd-sector           [^ShapeDrawer sd [centre-x centre-y] radius start-angle degree] (.sector sd centre-x centre-y radius (degree->radians start-angle) (degree->radians degree)))
(defn- sd-rectangle        [^ShapeDrawer sd x y w h] (.rectangle sd x y w h))
(defn- sd-filled-rectangle [^ShapeDrawer sd x y w h] (.filledRectangle sd (float x) (float y) (float w) (float h)) )
(defn- sd-line             [^ShapeDrawer sd [sx sy] [ex ey]] (.line sd (float sx) (float sy) (float ex) (float ey)))

(defn- sd-grid [sd leftx bottomy gridw gridh cellw cellh]
  (let [w (* (float gridw) (float cellw))
        h (* (float gridh) (float cellh))
        topy (+ (float bottomy) (float h))
        rightx (+ (float leftx) (float w))]
    (doseq [idx (range (inc (float gridw)))
            :let [linex (+ (float leftx) (* (float idx) (float cellw)))]]
      (sd-line sd [linex topy] [linex bottomy]))
    (doseq [idx (range (inc (float gridh)))
            :let [liney (+ (float bottomy) (* (float idx) (float cellh)))]]
      (sd-line sd [leftx liney] [rightx liney]))))

(defn- sd-with-line-width [^ShapeDrawer sd width draw-fn]
  (let [old-line-width (.getDefaultLineWidth sd)]
    (.setDefaultLineWidth sd (float (* (float width) old-line-width)))
    (draw-fn)
    (.setDefaultLineWidth sd (float old-line-width))))

(defn- ->ttf-params [size quality-scaling]
  (let [params (FreeTypeFontGenerator$FreeTypeFontParameter.)]
    (set! (.size params) (* size quality-scaling))
    ; .color and this:
    ;(set! (.borderWidth parameter) 1)
    ;(set! (.borderColor parameter) red)
    (set! (.minFilter params) Texture$TextureFilter/Linear) ; because scaling to world-units
    (set! (.magFilter params) Texture$TextureFilter/Linear)
    params))

(defn- generate-ttf [{:keys [file size quality-scaling]}]
  (let [generator (FreeTypeFontGenerator. (internal-file file))
        font (.generateFont generator (->ttf-params size quality-scaling))]
    (dispose! generator)
    (.setScale (.getData font) (float (/ quality-scaling)))
    (set! (.markupEnabled (.getData font)) true)
    (.setUseIntegerPositions font false) ; otherwise scaling to world-units (/ 1 48)px not visible
    font))

(defn- gdx-default-font [] (BitmapFont.))

(defn- text-height [^BitmapFont font text]
  (-> text
      (str/split #"\n")
      count
      (* (.getLineHeight font))))

(defn- font-draw [^BitmapFont font
                  unit-scale
                  batch
                  {:keys [x y text h-align up? scale]}]
  (let [data (.getData font)
        old-scale (float (.scaleX data))]
    (.setScale data (* old-scale (float unit-scale) (float (or scale 1))))
    (.draw font
           batch
           (str text)
           (float x)
           (+ (float y) (float (if up? (text-height font text) 0)))
           (float 0) ; target-width
           (case (or h-align :center)
             :center Align/center
             :left   Align/left
             :right  Align/right)
           false) ; wrap false, no need target-width
    (.setScale data old-scale)))

(declare ^:private batch
         ^:private shape-drawer
         ^:private shape-drawer-texture
         ^:private gui-view
         ^:private world-view
         ^:private default-font
         ^:private ^:dynamic *unit-scale*
         ^:private cursors)

(defn draw-ellipse [position radius-x radius-y color]
  (set-color! shape-drawer color)
  (sd-ellipse shape-drawer position radius-x radius-y))
(defn draw-filled-ellipse [position radius-x radius-y color]
  (set-color! shape-drawer color)
  (sd-filled-ellipse shape-drawer position radius-x radius-y))
(defn draw-circle [position radius color]
  (set-color! shape-drawer color)
  (sd-circle shape-drawer position radius))
(defn draw-filled-circle [position radius color]
  (set-color! shape-drawer color)
  (sd-filled-circle shape-drawer position radius))
(defn draw-arc [center radius start-angle degree color]
  (set-color! shape-drawer color)
  (sd-arc shape-drawer center radius start-angle degree))
(defn draw-sector [center radius start-angle degree color]
  (set-color! shape-drawer color)
  (sd-sector shape-drawer center radius start-angle degree))
(defn draw-rectangle [x y w h color]
  (set-color! shape-drawer color)
  (sd-rectangle shape-drawer x y w h))
(defn draw-filled-rectangle [x y w h color]
  (set-color! shape-drawer color)
  (sd-filled-rectangle shape-drawer x y w h))
(defn draw-line [start end color]
  (set-color! shape-drawer color)
  (sd-line shape-drawer start end))
(defn draw-grid [leftx bottomy gridw gridh cellw cellh color]
  (set-color! shape-drawer color)
  (sd-grid shape-drawer leftx bottomy gridw gridh cellw cellh))
(defn with-shape-line-width [width draw-fn]
  (sd-with-line-width shape-drawer width draw-fn))

(defn- ->gui-view [{:keys [world-width world-height]}]
  {:unit-scale 1
   :viewport (->gui-viewport world-width world-height)})

(defn- ->world-view [{:keys [world-width world-height tile-size]}]
  (let [unit-scale (/ tile-size)]
    {:unit-scale (float unit-scale)
     :viewport (->world-viewport world-width world-height unit-scale)}))

(defn- bind-views! [{:keys [gui-view world-view]}]
  (bind-root #'gui-view (->gui-view gui-view))
  (bind-root #'world-view (->world-view world-view)))

(defn world-unit-scale []
  (:unit-scale world-view))

(defn pixels->world-units [pixels]
  (* (int pixels) (world-unit-scale)))

(defn- gui-viewport   [] (:viewport gui-view))
(defn- world-viewport [] (:viewport world-view))

(defn- gui-mouse-position* []
  ; TODO mapv int needed?
  (mapv int (unproject-mouse-posi (gui-viewport))))

(defn- world-mouse-position* []
  ; TODO clamping only works for gui-viewport ? check. comment if true
  ; TODO ? "Can be negative coordinates, undefined cells."
  (unproject-mouse-posi (world-viewport)))

(defn gui-mouse-position    [] (gui-mouse-position*))
(defn world-mouse-position  [] (world-mouse-position*))
(defn gui-viewport-width    [] (vp-world-width  (gui-viewport)))
(defn gui-viewport-height   [] (vp-world-height (gui-viewport)))
(defn world-camera          [] (vp-camera       (world-viewport)))
(defn world-viewport-width  [] (vp-world-width  (world-viewport)))
(defn world-viewport-height [] (vp-world-height (world-viewport)))

(defrecord Sprite [texture-region
                   pixel-dimensions
                   world-unit-dimensions
                   color]) ; optional

(defn- unit-dimensions [image]
  (if (= *unit-scale* 1)
    (:pixel-dimensions image)
    (:world-unit-dimensions image)))

(defn- scale-dimensions [dimensions scale]
  (mapv (comp float (partial * scale)) dimensions))

(defn- assoc-dimensions
  "scale can be a number for multiplying the texture-region-dimensions or [w h]."
  [{:keys [texture-region] :as image} scale]
  {:pre [(or (number? scale)
             (and (vector? scale)
                  (number? (scale 0))
                  (number? (scale 1))))]}
  (let [pixel-dimensions (if (number? scale)
                           (scale-dimensions (texture-region-dimensions texture-region) scale)
                           scale)]
    (assoc image
           :pixel-dimensions pixel-dimensions
           :world-unit-dimensions (scale-dimensions pixel-dimensions (world-unit-scale)))))

(defn draw-image [{:keys [texture-region color] :as image} position]
  (draw-texture-region batch
                       texture-region
                       position
                       (unit-dimensions image)
                       0 ; rotation
                       color))

(defn draw-rotated-centered-image
  [{:keys [texture-region color] :as image} rotation [x y]]
  (let [[w h] (unit-dimensions image)]
    (draw-texture-region batch
                         texture-region
                         [(- (float x) (/ (float w) 2))
                          (- (float y) (/ (float h) 2))]
                         [w h]
                         rotation
                         color)))

(defn draw-centered-image [image position]
  (draw-rotated-centered-image image 0 position))

(defn- ->image* [texture-region]
  (-> {:texture-region texture-region}
      (assoc-dimensions 1) ; = scale 1
      map->Sprite))

(defn ->image [file]
  (->image* (->texture-region file)))

(defn sub-image [{:keys [texture-region]} bounds]
  (->image* (->texture-region texture-region bounds)))

(defn sprite-sheet [file tilew tileh]
  {:image (->image file)
   :tilew tilew
   :tileh tileh})

(defn sprite
  "x,y index starting top-left"
  [{:keys [image tilew tileh]} [x y]]
  (sub-image image [(* x tilew) (* y tileh) tilew tileh]))

(defn edn->image [{:keys [file sub-image-bounds]}]
  (if sub-image-bounds
    (let [[sprite-x sprite-y] (take 2 sub-image-bounds)
          [tilew tileh]       (drop 2 sub-image-bounds)]
      (sprite (sprite-sheet file tilew tileh)
              [(int (/ sprite-x tilew))
               (int (/ sprite-y tileh))]))
    (->image file)))

(defn- ->default-font [true-type-font]
  (or (and true-type-font (generate-ttf true-type-font))
      (gdx-default-font)))

(defn draw-text
  "font, h-align, up? and scale are optional.
  h-align one of: :center, :left, :right. Default :center.
  up? renders the font over y, otherwise under.
  scale will multiply the drawn text size with the scale."
  [{:keys [x y text font h-align up? scale] :as opts}]
  (font-draw (or font default-font) *unit-scale* batch opts))

(defn- mapvals [f m]
  (into {} (for [[k v] m]
             [k (f v)])))

(defn- ->cursor [file [hotspot-x hotspot-y]]
  (let [pixmap (Pixmap. (internal-file file))
        cursor (.newCursor Gdx/graphics pixmap hotspot-x hotspot-y)]
    (dispose! pixmap)
    cursor))

(defn- ->cursors [cursors]
  (mapvals (fn [[file hotspot]]
             (->cursor (str "cursors/" file ".png") hotspot))
           cursors))

(defn set-cursor! [cursor-key]
  (.setCursor Gdx/graphics (safe-get cursors cursor-key)))

(defc :tx/cursor
  (do! [[_ cursor-key]]
    (set-cursor! cursor-key)
    nil))

(defn- render-view! [{:keys [viewport unit-scale]} draw-fn]
  (draw-with! batch
              viewport
              (fn []
                (with-shape-line-width unit-scale
                  #(binding [*unit-scale* unit-scale]
                     (draw-fn))))))

(defn render-gui-view!   [render-fn] (render-view! gui-view render-fn))
(defn render-world-view! [render-fn] (render-view! world-view render-fn))

(defn- update-viewports! [dim]
  (vp-update! (gui-viewport) dim :center-camera? true)
  (vp-update! (world-viewport) dim)) ; Do not center the camera on world-viewport. We set the position there manually.

(declare draw-item-on-cursor)

(declare ^:private cached-map-renderer)

(defn draw-tiled-map
  "Renders tiled-map using world-view at world-camera position and with world-unit-scale.

  Color-setter is a `(fn [color x y])` which is called for every tile-corner to set the color.

  Can be used for lights & shadows.

  Renders only visible layers."
  [tiled-map color-setter]
  (t/render-tm! (cached-map-renderer tiled-map)
                color-setter
                (world-camera)
                tiled-map))

(defn- ->tiled-map-renderer [tiled-map]
  (t/->orthogonal-tiled-map-renderer tiled-map (world-unit-scale) batch))

(def-attributes
  :views [:map [:gui-view :world-view]]
  :gui-view [:map [:world-width :world-height]]
  :world-view [:map [:tile-size :world-width :world-height]]
  :world-width :pos-int
  :world-height :pos-int
  :tile-size :pos-int

  :default-font [:map [:file :quality-scaling :size]]
  :file :string
  :quality-scaling :pos-int
  :size :pos-int

  :cursors :some)

(defc :context/graphics
  {:data [:map [:cursors
                :default-font
                :views]]}
  (->mk [[_ {:keys [views default-font cursors]}]]
    (let [batch (->sprite-batch)
          {:keys [shape-drawer shape-drawer-texture]} (->shape-drawer batch)]
      (bind-root #'batch batch)
      (bind-root #'shape-drawer shape-drawer)
      (bind-root #'shape-drawer-texture shape-drawer-texture)
      (bind-root #'cursors (->cursors cursors))
      (bind-root #'default-font (->default-font default-font))
      (bind-views! views)
      (bind-root #'cached-map-renderer (memoize ->tiled-map-renderer))))

  (destroy! [_]
    (dispose! batch)
    (dispose! shape-drawer-texture)
    (dispose! default-font)
    (run! dispose! (vals cursors))))
