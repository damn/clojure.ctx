(in-ns 'clojure.gdx)

(defn- check-cleanup-visui! []
  ; app crashes during startup before VisUI/dispose and we do clojure.tools.namespace.refresh-> gui elements not showing.
  ; => actually there is a deeper issue at play
  ; we need to dispose ALL resources which were loaded already ...
  (when (VisUI/isLoaded)
    (VisUI/dispose)))

(defn- font-enable-markup! []
  (-> (VisUI/getSkin)
      (.getFont "default-font")
      .getData
      .markupEnabled
      (set! true)))

(defn- set-tooltip-config! []
  (set! Tooltip/DEFAULT_APPEAR_DELAY_TIME (float 0))
  ;(set! Tooltip/DEFAULT_FADE_TIME (float 0.3))
  ;Controls whether to fade out tooltip when mouse was moved. (default false)
  ;(set! Tooltip/MOUSE_MOVED_FADEOUT true)
  )

(defc :context/vis-ui
  {:data [:enum [:skin-scale/x1 :skin-scale/x2]]
   :let skin-scale}
  (->mk [_]
    (check-cleanup-visui!)
    (VisUI/load (case skin-scale
                  :skin-scale/x1 VisUI$SkinScale/X1
                  :skin-scale/x2 VisUI$SkinScale/X2))
    (font-enable-markup!)
    (set-tooltip-config!))
  (destroy! [_]
    (VisUI/dispose)))

(defn actor-x [^Actor a] (.getX a))
(defn actor-y [^Actor a] (.getY a))

(defn actor-id [^Actor actor]
  (.getUserObject actor))

(defn set-id! [^Actor actor id]
  (.setUserObject actor id))

(defn set-name! [^Actor actor name]
  (.setName actor name))

(defn actor-name [^Actor actor]
  (.getName actor))

(defn visible? [^Actor actor] ; used
  (.isVisible actor))

(defn set-visible! [^Actor actor bool]
  (.setVisible actor (boolean bool)))

(defn toggle-visible! [actor] ; used
  (set-visible! actor (not (visible? actor))))

(defn set-position! [^Actor actor x y]
  (.setPosition actor x y))

(defn set-center! [^Actor actor x y]
  (set-position! actor
                 (- x (/ (.getWidth actor) 2))
                 (- y (/ (.getHeight actor) 2))))

(defn set-touchable!
  ":children-only, :disabled or :enabled."
  [^Actor actor touchable]
  (.setTouchable actor (case touchable
                         :children-only Touchable/childrenOnly
                         :disabled      Touchable/disabled
                         :enabled       Touchable/enabled)))

(defn add-listener! [^Actor actor listener]
  (.addListener actor listener))

(defn remove!
  "Removes this actor from its parent, if it has a parent."
  [^Actor actor]
  (.remove actor))

(defn parent
  "Returns the parent actor, or null if not in a group."
  [^Actor actor]
  (.getParent actor))

(defn a-mouseover? [^Actor actor [x y]]
  (let [v (.stageToLocalCoordinates actor (Vector2. x y))]
    (.hit actor (.x v) (.y v) true)))

(defn remove-tooltip! [^Actor actor]
  (Tooltip/removeTooltip actor))

(defn find-ancestor-window ^Window [^Actor actor]
  (if-let [p (parent actor)]
    (if (instance? Window p)
      p
      (find-ancestor-window p))
    (throw (Error. (str "Actor has no parent window " actor)))))

(defn pack-ancestor-window! [^Actor actor]
  (.pack (find-ancestor-window actor)))

(defn children
  "Returns an ordered list of child actors in this group."
  [^Group group]
  (seq (.getChildren group)))

(defn clear-children!
  "Removes all actors from this group and unfocuses them."
  [^Group group]
  (.clearChildren group))

(defn add-actor!
  "Adds an actor as a child of this group, removing it from its previous parent. If the actor is already a child of this group, no changes are made."
  [^Group group actor]
  (.addActor group actor))

(defn find-actor-with-id [group id]
  (let [actors (children group)
        ids (keep actor-id actors)]
    (assert (or (empty? ids)
                (apply distinct? ids)) ; TODO could check @ add
            (str "Actor ids are not distinct: " (vec ids)))
    (first (filter #(= id (actor-id %)) actors))))

(defn set-cell-opts [^Cell cell opts]
  (doseq [[option arg] opts]
    (case option
      :fill-x?    (.fillX     cell)
      :fill-y?    (.fillY     cell)
      :expand?    (.expand    cell)
      :expand-x?  (.expandX   cell)
      :expand-y?  (.expandY   cell)
      :bottom?    (.bottom    cell)
      :colspan    (.colspan   cell (int arg))
      :pad        (.pad       cell (float arg))
      :pad-top    (.padTop    cell (float arg))
      :pad-bottom (.padBottom cell (float arg))
      :width      (.width     cell (float arg))
      :height     (.height    cell (float arg))
      :right?     (.right     cell)
      :left?      (.left      cell))))

(defn add-rows!
  "rows is a seq of seqs of columns.
  Elements are actors or nil (for just adding empty cells ) or a map of
  {:actor :expand? :bottom?  :colspan int :pad :pad-bottom}. Only :actor is required."
  [^Table table rows]
  (doseq [row rows]
    (doseq [props-or-actor row]
      (cond
       (map? props-or-actor) (-> (.add table ^Actor (:actor props-or-actor))
                                 (set-cell-opts (dissoc props-or-actor :actor)))
       :else (.add table ^Actor props-or-actor)))
    (.row table))
  table)

(defn- set-table-opts [^Table table {:keys [rows cell-defaults]}]
  (set-cell-opts (.defaults table) cell-defaults)
  (add-rows! table rows))

(defn t-row!   "Add row to table." [^Table t] (.row   t))
(defn t-clear! "Clear table. "     [^Table t] (.clear t))

(defn t-add!
  "Add to table"
  ([^Table t] (.add t))
  ([^Table t ^Actor a] (.add t a)))

(defn ->horizontal-separator-cell [colspan]
  {:actor (Separator. "default")
   :pad-top 2
   :pad-bottom 2
   :colspan colspan
   :fill-x? true
   :expand-x? true})

(defn ->vertical-separator-cell []
  {:actor (Separator. "vertical")
   :pad-top 2
   :pad-bottom 2
   :fill-y? true
   :expand-y? true})

; candidate for opts: :tooltip
(defn- set-actor-opts [actor {:keys [id name visible? touchable center-position position] :as opts}]
  (when id   (set-id!   actor id))
  (when name (set-name! actor name))
  (when (contains? opts :visible?)  (set-visible! actor visible?))
  (when touchable (set-touchable! actor touchable))
  (when-let [[x y] center-position] (set-center!   actor x y))
  (when-let [[x y] position]        (set-position! actor x y))
  actor)

(comment
 ; fill parent & pack is from Widget TODO ( not widget-group ?)
 com.badlogic.gdx.scenes.scene2d.ui.Widget
 ; about .pack :
 ; Generally this method should not be called in an actor's constructor because it calls Layout.layout(), which means a subclass would have layout() called before the subclass' constructor. Instead, in constructors simply set the actor's size to Layout.getPrefWidth() and Layout.getPrefHeight(). This allows the actor to have a size at construction time for more convenient use with groups that do not layout their children.
 )

(defn- set-widget-group-opts [^WidgetGroup widget-group {:keys [fill-parent? pack?]}]
  (.setFillParent widget-group (boolean fill-parent?)) ; <- actor? TODO
  (when pack?
    (.pack widget-group))
  widget-group)

(defn- set-opts [actor opts]
  (set-actor-opts actor opts)
  (when (instance? Table actor)
    (set-table-opts actor opts)) ; before widget-group-opts so pack is packing rows
  (when (instance? WidgetGroup actor)
    (set-widget-group-opts actor opts))
  actor)

#_(defn- add-window-close-button [^Window window]
    (.add (.getTitleTable window)
          (text-button "x" #(.setVisible window false)))
    window)

(defmacro ^:private proxy-ILookup
  "For actors inheriting from Group."
  [class args]
  `(proxy [~class clojure.lang.ILookup] ~args
     (valAt
       ([id#]
        (find-actor-with-id ~'this id#))
       ([id# not-found#]
        (or (find-actor-with-id ~'this id#) not-found#)))))

(defn ->group [{:keys [actors] :as opts}]
  (let [group (proxy-ILookup Group [])]
    (run! #(add-actor! group %) actors)
    (set-opts group opts)))

(defn ->horizontal-group [{:keys [space pad]}]
  (let [group (proxy-ILookup HorizontalGroup [])]
    (when space (.space group (float space)))
    (when pad   (.pad   group (float pad)))
    group))

(defn ->vertical-group [actors]
  (let [group (proxy-ILookup VerticalGroup [])]
    (run! #(add-actor! group %) actors)
    group))

(defn ->button-group
  "https://javadoc.io/doc/com.badlogicgames.gdx/gdx/latest/com/badlogic/gdx/scenes/scene2d/ui/ButtonGroup.html"
  [{:keys [max-check-count min-check-count]}]
  (let [button-group (ButtonGroup.)]
    (.setMaxCheckCount button-group max-check-count)
    (.setMinCheckCount button-group min-check-count)
    button-group))

(defn ->check-box
  "on-clicked is a fn of one arg, taking the current isChecked state
  [com.kotcrab.vis.ui.widget.VisCheckBox](https://www.javadoc.io/static/com.kotcrab.vis/vis-ui/1.5.3/com/kotcrab/vis/ui/widget/VisCheckBox.html)"
  [text on-clicked checked?]
  (let [^Button button (VisCheckBox. ^String text)]
    (.setChecked button checked?)
    (.addListener button
                  (proxy [ChangeListener] []
                    (changed [event ^Button actor]
                      (on-clicked (.isChecked actor)))))
    button))

(defn ->select-box [{:keys [items selected]}]
  (doto (VisSelectBox.)
    (.setItems ^"[Lcom.badlogic.gdx.scenes.scene2d.Actor;" (into-array items))
    (.setSelected selected)))

(defn ->table ^Table [opts]
  (-> (proxy-ILookup VisTable [])
      (set-opts opts)))

(defn ->window ^VisWindow [{:keys [title modal? close-button? center? close-on-escape?] :as opts}]
  (-> (let [window (doto (proxy-ILookup VisWindow [^String title true]) ; true = showWindowBorder
                     (.setModal (boolean modal?)))]
        (when close-button?    (.addCloseButton window))
        (when center?          (.centerWindow   window))
        (when close-on-escape? (.closeOnEscape  window))
        window)
      (set-opts opts)))

(defn ->label ^VisLabel [text]
  (VisLabel. ^CharSequence text))

(defn ->text-field [^String text opts]
  (-> (VisTextField. text)
      (set-opts opts)))

; TODO is not decendend of SplitPane anymore => check all type hints here
(defn ->split-pane [{:keys [^Actor first-widget
                            ^Actor second-widget
                            ^Boolean vertical?] :as opts}]
  (-> (VisSplitPane. first-widget second-widget vertical?)
      (set-actor-opts opts)))

(defn ->stack [actors]
  (proxy-ILookup Stack [(into-array Actor actors)]))

(defmulti ^:private ->vis-image type)
(defmethod ->vis-image Drawable      [^Drawable d      ] (VisImage.  d))
(defmethod ->vis-image TextureRegion [^TextureRegion tr] (VisImage. tr))

; TODO widget also make, for fill parent
(defn ->ui-image-widget
  "Takes either a texture-region or drawable. Opts are :scaling, :align and actor opts."
  [object {:keys [scaling align fill-parent?] :as opts}]
  (-> (let [^Image image (->vis-image object)]
        (when (= :center align) (.setAlign image Align/center))
        (when (= :fill scaling) (.setScaling image Scaling/fill))
        (when fill-parent? (.setFillParent image true))
        image)
      (set-opts opts)))

(defn ->image-widget [image opts]
  (->ui-image-widget (:texture-region image) opts))

; => maybe with VisImage not necessary anymore?
(defn ->texture-region-drawable [^TextureRegion texture-region]
  (TextureRegionDrawable. texture-region))

(defn ->scroll-pane [actor]
  (let [scroll-pane (VisScrollPane. actor)]
    (.setFlickScroll scroll-pane false)
    (.setFadeScrollBars scroll-pane false)
    scroll-pane))

(defn- button-class? [actor]
  (some #(= Button %) (supers (class actor))))

(defn button?
  "Returns true if the actor or its parent is a button."
  [actor]
  (or (button-class? actor)
      (and (parent actor)
           (button-class? (parent actor)))))

(defn window-title-bar?
  "Returns true if the actor is a window title bar."
  [actor]
  (when (instance? Label actor)
    (when-let [p (parent actor)]
      (when-let [p (parent p)]
        (and (instance? VisWindow p)
             (= (.getTitleLabel ^Window p) actor))))))

(defn add-tooltip!
  "tooltip-text is a (fn []) or a string. If it is a function will be-recalculated every show."
  [^Actor actor tooltip-text]
  (let [text? (string? tooltip-text)
        label (VisLabel. (if text? tooltip-text ""))
        tooltip (proxy [Tooltip] []
                  ; hooking into getWidth because at
                  ; https://github.com/kotcrab/vis-blob/master/ui/src/main/java/com/kotcrab/vis/ui/widget/Tooltip.java#L271
                  ; when tooltip position gets calculated we setText (which calls pack) before that
                  ; so that the size is correct for the newly calculated text.
                  (getWidth []
                    (let [^Tooltip this this]
                      (when-not text?
                        (.setText this (str (tooltip-text))))
                      (proxy-super getWidth))))]
    (.setAlignment label Align/center)
    (.setTarget  tooltip ^Actor actor)
    (.setContent tooltip ^Actor label)))

(declare ^:dynamic *on-clicked-actor*)

(defn- ->change-listener [on-clicked]
  (proxy [ChangeListener] []
    (changed [event actor]
      (binding [*on-clicked-actor* actor]
        (on-clicked)))))

(defn ->text-button [text on-clicked]
  (let [button (VisTextButton. ^String text)]
    (.addListener button (->change-listener on-clicked))
    button))

(defn- ->ui-image-button [texture-region scale on-clicked]
  (let [drawable (TextureRegionDrawable. ^TextureRegion texture-region)
        button (VisImageButton. drawable)]
    (when scale
      (let [[w h] (texture-region-dimensions texture-region)]
        (.setMinSize drawable (float (* scale w)) (float (* scale h)))))
    (.addListener button (->change-listener on-clicked))
    button))

; TODO check how to make toggle-able ? with hotkeys for actionbar trigger ?
(defn ->image-button
  ([image on-clicked]
   (->image-button image on-clicked {}))

  ([image on-clicked {:keys [scale]}]
   (->ui-image-button (:texture-region image) scale on-clicked)))

(defn- ->ui-actor [draw! act!]
  (proxy [Actor] []
    (draw [_batch _parent-alpha]
      (draw!))
    (act [_delta]
      (act!))))

(defn ->ui-widget [draw!]
  (proxy [Widget] []
    (draw [_batch _parent-alpha]
      (draw! this))))

(defn set-drawable! [^Image image drawable]
  (.setDrawable image drawable))

(defn set-min-size! [^TextureRegionDrawable drawable size]
  (.setMinSize drawable (float size) (float size)))

(defn ->tinted-drawable
  "Creates a new drawable that renders the same as this drawable tinted the specified color."
  [drawable color]
  (.tint ^TextureRegionDrawable drawable color))

(defmacro ->click-listener [& clicked-fn-body]
  `(proxy [ClickListener] []
     ~@clicked-fn-body))

(defn bg-add!    [button-group button] (.add    ^ButtonGroup button-group ^Button button))
(defn bg-remove! [button-group button] (.remove ^ButtonGroup button-group ^Button button))
(defn bg-checked
  "The first checked button, or nil."
  [button-group]
  (.getChecked ^ButtonGroup button-group))

; FIXME t- already used for trees

(defn ->t-node ^Tree$Node [actor]
  (proxy [Tree$Node] [actor]))

(defn ->ui-tree [] (VisTree.))

; FIXME broken
(defn t-node-add! [^Tree$Node parent node] (.add parent node))

(defn- ->ui-stage
  "Stage implements clojure.lang.ILookup (get) on actor id."
  ^Stage [viewport batch]
  (proxy [Stage clojure.lang.ILookup] [viewport batch]
    (valAt
      ([id]
       (find-actor-with-id (.getRoot ^Stage this) id))
      ([id not-found]
       (or (find-actor-with-id (.getRoot ^Stage this) id) not-found)))))

(defn- s-act!   [^Stage s]   (.act      s))
(defn- s-draw!  [^Stage s]   (.draw     s))
(defn  s-root   [^Stage s]   (.getRoot  s))
(defn  s-clear! [^Stage s]   (.clear    s))
(defn  s-add!   [^Stage s a] (.addActor s a))
(defn- s-hit    [^Stage s [x y] & {:keys [touchable?]}]
  (.hit s x y (boolean touchable?)))

; TODO not disposed anymore... screens are sub-level.... look for dispose stuff also in @ cdq! FIXME
(defc :screens/stage
  {:let {:keys [stage sub-screen]}}
  (screen-enter [_]
    (set-input-processor! stage)
    (screen-enter sub-screen))

  (screen-exit [_]
    (set-input-processor! nil)
    (screen-exit sub-screen))

  (screen-render! [_]
    ; stage act first so user-screen calls change-screen -> is the end of frame
    ; otherwise would need render-after-stage
    ; or on change-screen the stage of the current screen would still .act
    (s-act! stage)
    (screen-render sub-screen)
    (s-draw! stage)))

(defn ->stage [actors]
  (let [stage (->ui-stage (:viewport gui-view) batch)]
    (run! #(s-add! stage %) actors)
    stage))

(defn stage-get []
  (:stage ((current-screen) 1)))

(defn mouse-on-actor? []
  (s-hit (stage-get) (gui-mouse-position) :touchable? true))

(defn stage-add! [actor]
  (s-add! (stage-get) actor))

(defn ->actor [{:keys [draw act]}]
  (->ui-actor (fn [] (when draw
                       (binding [*unit-scale* 1]
                         (draw))))
              (fn [] (when act
                       (act)))))

(def ^:private image-file "images/moon_background.png")

(defn ->background-image []
  (->image-widget (->image image-file)
                  {:fill-parent? true
                   :scaling :fill
                   :align :center}))

(defmacro ^:private with-err-str
  "Evaluates exprs in a context in which *err* is bound to a fresh
  StringWriter.  Returns the string created by any nested printing
  calls."
  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*err* s#]
       ~@body
       (str s#))))

(defn error-window! [throwable]
  (binding [*print-level* 5]
    (pretty-pst throwable 24))
  (stage-add! (->window {:title "Error"
                         :rows [[(->label (binding [*print-level* 3]
                                            (with-err-str
                                              (clojure.repl/pst throwable))))]]
                         :modal? true
                         :close-button? true
                         :close-on-escape? true
                         :center? true
                         :pack? true})))

; TODO no window movable type cursor appears here like in player idle
; inventory still working, other stuff not, because custom listener to keypresses ? use actor listeners?
; => input events handling
; hmmm interesting ... can disable @ item in cursor  / moving / etc.

(defn- show-player-modal! [{:keys [title text button-text on-click]}]
  (assert (not (::modal (stage-get))))
  (stage-add! (->window {:title title
                         :rows [[(->label text)]
                                [(->text-button button-text
                                                (fn []
                                                  (remove! (::modal (stage-get)))
                                                  (on-click)))]]
                         :id ::modal
                         :modal? true
                         :center-position [(/ (gui-viewport-width) 2)
                                           (* (gui-viewport-height) (/ 3 4))]
                         :pack? true})))

(defc :tx/player-modal
  (do! [[_ params]]
    (show-player-modal! params)
    nil))
