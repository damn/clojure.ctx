(ns core.app
  (:require [clojure.gdx :refer :all]
            [clojure.gdx.tiled :as t]
            core.creature
            core.stat
            core.projectile
            [core.world :as world]
            [data.grid2d :as g]))

(defn- render-infostr-on-bar [infostr x y h]
  (draw-text {:text infostr
              :x (+ x 75)
              :y (+ y 2)
              :up? true}))

(defn- ->hp-mana-bars []
  (let [rahmen      (->image "images/rahmen.png")
        hpcontent   (->image "images/hp.png")
        manacontent (->image "images/mana.png")
        x (/ (gui-viewport-width) 2)
        [rahmenw rahmenh] (:pixel-dimensions rahmen)
        y-mana 80 ; action-bar-icon-size
        y-hp (+ y-mana rahmenh)
        render-hpmana-bar (fn [x y contentimg minmaxval name]
                            (draw-image rahmen [x y])
                            (draw-image (sub-image contentimg [0 0 (* rahmenw (val-max-ratio minmaxval)) rahmenh])
                                        [x y])
                            (render-infostr-on-bar (str (readable-number (minmaxval 0)) "/" (minmaxval 1) " " name) x y rahmenh))]
    (->actor {:draw (fn []
                         (let [player-entity* @world-player
                               x (- x (/ rahmenw 2))]
                           (render-hpmana-bar x y-hp   hpcontent   (entity-stat player-entity* :stats/hp) "HP")
                           (render-hpmana-bar x y-mana manacontent (entity-stat player-entity* :stats/mana) "MP")))})))

(defn- skill-info [{:keys [entity/skills]}]
  (clojure.string/join "\n"
                       (for [{:keys [property/id skill/cooling-down?]} (vals skills)
                             :when cooling-down? ]
                         [id [:cooling-down? (boolean cooling-down?)]])))

(defn- debug-infos ^String []
  (let [world-mouse (world-mouse-position)]
    (str
     "logic-frame: " logic-frame "\n"
     "FPS: " (frames-per-second)  "\n"
     "Zoom: " (zoom (world-camera)) "\n"
     "World: "(mapv int world-mouse) "\n"
     "X:" (world-mouse 0) "\n"
     "Y:" (world-mouse 1) "\n"
     "GUI: " (gui-mouse-position) "\n"
     "paused? " world-paused? "\n"
     "elapsed-time " (readable-number elapsed-time) " seconds \n"
     "skill cooldowns: " (skill-info @world-player) "\n"
     (when-let [entity* (mouseover-entity*)]
       (str "Mouseover-entity uid: " (:entity/uid entity*)))
     ;"\nMouseover-Actor:\n"
     #_(when-let [actor (mouse-on-actor?)]
         (str "TRUE - name:" (.getName actor)
              "id: " (actor-id actor))))))

(defn- ->debug-window []
  (let [label (->label "")
        window (->window {:title "Debug"
                          :id :debug-window
                          :visible? false
                          :position [0 (gui-viewport-height)]
                          :rows [[label]]})]
    (add-actor! window (->actor {:act #(do
                                        (.setText label (debug-infos))
                                        (.pack window))}))
    window))

(def ^:private disallowed-keys [:entity/skills
                                :entity/state
                                :entity/faction
                                :active-skill])

(defn- ->entity-info-window []
  (let [label (->label "")
        window (->window {:title "Info"
                          :id :entity-info-window
                          :visible? false
                          :position [(gui-viewport-width) 0]
                          :rows [[{:actor label :expand? true}]]})]
    ; TODO do not change window size ... -> no need to invalidate layout, set the whole stage up again
    ; => fix size somehow.
    (add-actor! window (->actor {:act (fn update-label-text []
                                        ; items then have 2x pretty-name
                                        #_(.setText (.getTitleLabel window)
                                                    (if-let [entity* (mouseover-entity*)]
                                                      (info-text [:property/pretty-name (:property/pretty-name entity*)])
                                                      "Entity Info"))
                                        (.setText label
                                                  (str (when-let [entity* (mouseover-entity*)]
                                                         (->info-text
                                                          ; don't use select-keys as it loses Entity record type
                                                          (apply dissoc entity* disallowed-keys)))))
                                        (.pack window))}))
    window))

(def ^:private image-scale 2)

(defn- ->action-bar []
  (let [group (->horizontal-group {:pad 2 :space 2})]
    (set-id! group ::action-bar)
    group))

(defn- ->action-bar-button-group []
  (->button-group {:max-check-count 1 :min-check-count 0}))

(defn- get-action-bar []
  {:horizontal-group (::action-bar (:action-bar-table (stage-get)))
   :button-group (:action-bar world-widgets)})

(defc :tx.action-bar/add
  (do! [[_ {:keys [property/id entity/image] :as skill}]]
    (let [{:keys [horizontal-group button-group]} (get-action-bar)
          button (->image-button image (fn []) {:scale image-scale})]
      (set-id! button id)
      (add-tooltip! button #(->info-text skill)) ; (assoc ctx :effect/source (world-player)) FIXME
      (add-actor! horizontal-group button)
      (bg-add! button-group button)
      nil)))

(defc :tx.action-bar/remove
  (do! [[_ {:keys [property/id]}]]
    (let [{:keys [horizontal-group button-group]} (get-action-bar)
          button (get horizontal-group id)]
      (remove! button)
      (bg-remove! button-group button)
      nil)))

(comment

 (comment
  (def sword-button (.getChecked button-group))
  (.setChecked sword-button false)
  )

 #_(defn- number-str->input-key [number-str]
     (eval (symbol (str "com.badlogic.gdx.Input$Keys/NUM_" number-str))))

 ; TODO do with an actor
 ; .getChildren horizontal-group => in order
 (defn up-skill-hotkeys []
   #_(doseq [slot slot-keys
             :let [skill-id (slot @slot->skill-id)]
             :when (and (key-just-pressed? (number-str->input-key (name slot)))
                        skill-id)]
       (.setChecked ^Button (.findActor horizontal-group (str skill-id)) true)))

 ; TODO
 ; * cooldown / not usable -> diff. colors ? disable on not able to use skills (stunned?)
 ; * or even sector circling for cooldown like in WoW (clipped !)
 ; * tooltips ! with hotkey-number !
 ;  ( (skills/text skill-id world-player))
 ; * add hotkey number to tooltips
 ; * hotkeys => select button
 ; when no selected-skill & new skill assoce'd (sword at start)
 ; => set selected
 ; keep weapon at position 1 always ?

 #_(def ^:private slot-keys {:1 input.keys.num-1
                             :2 input.keys.num-2
                             :3 input.keys.num-3
                             :4 input.keys.num-4
                             :5 input.keys.num-5
                             :6 input.keys.num-6
                             :7 input.keys.num-7
                             :8 input.keys.num-8
                             :9 input.keys.num-9})

 #_(defn- empty-slot->skill-id []
     (apply sorted-map
            (interleave slot-keys
                        (repeat nil))))

 #_(def selected-skill-id (atom nil))
 #_(def ^:private slot->skill-id (atom nil))

 #_(defn reset-skills! []
     (reset! selected-skill-id nil)
     (reset! slot->skill-id (empty-slot->skill-id)))


 ; https://javadoc.io/doc/com.badlogicgames.gdx/gdx/latest/com/badlogic/gdx/scenes/scene2d/ui/Button.html
 (.setProgrammaticChangeEvents ^Button (.findActor horizontal-group ":skills/spawn") true)
 ; but doesn't toggle:
 (.toggle ^Button (.findActor horizontal-group ":skills/spawn"))
 (.setChecked ^Button (.findActor horizontal-group ":skills/spawn") true)
 ; Toggles the checked state. This method changes the checked state, which fires a ChangeListener.ChangeEvent (if programmatic change events are enabled), so can be used to simulate a button click.

 ; => it _worked_ => active skill changed
 ; only button is not highlighted idk why

 (.getChildren horizontal-group)

 )

(def ^:private message-to-player nil)

(def ^:private duration-seconds 1.5)

(defn- draw-player-message []
  (when-let [{:keys [message]} message-to-player]
    (draw-text {:x (/ (gui-viewport-width) 2)
                :y (+ (/ (gui-viewport-height) 2) 200)
                :text message
                :scale 2.5
                :up? true})))

(defn- check-remove-message []
  (when-let [{:keys [counter]} message-to-player]
    (alter-var-root #'message-to-player update :counter + (delta-time))
    (when (>= counter duration-seconds)
      (bind-root #'message-to-player nil))))

(defc :widgets/player-message
  (->mk [_]
    (->actor {:draw draw-player-message
              :act check-remove-message})))

(defc :tx/msg-to-player
  (do! [[_ message]]
    (bind-root #'message-to-player {:message message :counter 0})
    nil))

(defn- ->ui-actors [widget-data]
  [(->table {:rows [[{:actor (->action-bar)
                      :expand? true
                      :bottom? true}]]
             :id :action-bar-table
             :cell-defaults {:pad 2}
             :fill-parent? true})
   (->hp-mana-bars)
   (->group {:id :windows
             :actors [(->debug-window)
                      (->entity-info-window)
                      (->inventory-window widget-data)]})
   (->actor {:draw draw-item-on-cursor})
   (->mk [:widgets/player-message])])

(defn ->world-widgets []
  (let [widget-data {:action-bar (->action-bar-button-group)
                     :slot->background (->inventory-window-data)}
        stage (stage-get)]
    (s-clear! stage)
    (run! #(s-add! stage %) (->ui-actors widget-data))
    widget-data))

(defn- ->explored-tile-corners [width height]
  (atom (g/create-grid width height (constantly false))))

(defn- world-grid-position->value-fn [tiled-map]
  (fn [position]
    (case (t/movement-property tiled-map position)
      "none" :none
      "air"  :air
      "all"  :all)))

(declare entity-tick-error)

(declare world-tiled-map)

(defn- cleanup-last-world! []
  (when (bound? #'world-tiled-map)
    (dispose! world-tiled-map)))

(def ^:private ^:dbg-flag spawn-enemies? true)

(def ^:private player-components {:entity/state [:state/player :player-idle]
                                  :entity/faction :good
                                  :entity/player? true
                                  :entity/free-skill-points 3
                                  :entity/clickable {:type :clickable/player}
                                  :entity/click-distance-tiles 1.5})

(def ^:private npc-components {:entity/state [:state/npc :npc-sleeping]
                               :entity/faction :evil})

; player-creature needs mana & inventory
; till then hardcode :creatures/vampire
(defn- world->player-creature [start-position]
  {:position start-position
   :creature-id :creatures/vampire
   :components player-components})

(defn- world->enemy-creatures [tiled-map]
  (for [[position creature-id] (t/positions-with-property tiled-map :creatures :id)]
    {:position position
     :creature-id (keyword creature-id)
     :components npc-components}))

(defn spawn-creatures! [tiled-map start-position]
  (effect! (for [creature (cons (world->player-creature start-position)
                                (when spawn-enemies?
                                  (world->enemy-creatures tiled-map)))]
             [:tx/creature (update creature :position tile->middle)])))

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
(defn- add-world-ctx [world-property-id]
  (cleanup-last-world!)
  (init-new-world! (world/generate-level world-property-id)))

(defc :tx/add-to-world
  (do! [[_ entity]]
    (content-grid-update-entity! entity)
    ; https://github.com/damn/core/issues/58
    ;(assert (valid-position? grid @entity)) ; TODO deactivate because projectile no left-bottom remove that field or update properly for all
    (grid-add-entity! entity)
    nil))

(defc :tx/remove-from-world
  (do! [[_ entity]]
    (content-grid-remove-entity! entity)
    (grid-remove-entity! entity)
    nil))

(defc :tx/position-changed
  (do! [[_ entity]]
    (content-grid-update-entity! entity)
    (grid-entity-position-changed! entity)
    nil))

(def dev-mode? (= (System/getenv "DEV_MODE") "true"))

; 28.4 viewportwidth
; 16 viewportheight
; camera shows :
;  [-viewportWidth/2, -(viewportHeight/2-1)] - [(viewportWidth/2-1), viewportHeight/2]
; zoom default '1'
; zoom 2 -> shows double amount

; we want min/max explored tiles X / Y and show the whole explored area....

(defn- minimap-zoom []
  (let [positions-explored (map first
                                (remove (fn [[position value]]
                                          (false? value))
                                        (seq @explored-tile-corners)))
        left   (apply min-key (fn [[x y]] x) positions-explored)
        top    (apply max-key (fn [[x y]] y) positions-explored)
        right  (apply max-key (fn [[x y]] x) positions-explored)
        bottom (apply min-key (fn [[x y]] y) positions-explored)]
    (calculate-zoom (world-camera)
                    :left left
                    :top top
                    :right right
                    :bottom bottom)))

(defn- ->tile-corner-color-setter [explored?]
  (fn tile-corner-color-setter [color x y]
    (if (get explored? [x y]) white black)))

#_(deftype Screen []
    (show [_]
      (set-zoom! (world-camera) (minimap-zoom)))

    (hide [_]
      (reset-zoom! (world-camera)))

    ; TODO fixme not subscreen
    (render [_]
      (draw-tiled-map world-tiled-map
                      (->tile-corner-color-setter @explored-tile-corners))
      (render-world-view! (fn []
                            (draw-filled-circle (camera-position (world-camera))
                                                0.5
                                                :green)))
      (when (or (key-just-pressed? :keys/tab)
                (key-just-pressed? :keys/escape))
        (change-screen :screens/world))))

#_(defc :screens/minimap
  (->mk [_]
    (->Screen)))

(defn- geom-test []
  (let [position (world-mouse-position)
        grid world-grid
        radius 0.8
        circle {:position position :radius radius}]
    (draw-circle position radius [1 0 0 0.5])
    (doseq [[x y] (map #(:position @%) (circle->cells grid circle))]
      (draw-rectangle x y 1 1 [1 0 0 0.5]))
    (let [{[x y] :left-bottom :keys [width height]} (circle->outer-rectangle circle)]
      (draw-rectangle x y width height [0 0 1 1]))))

(def ^:private ^:dbg-flag tile-grid? false)
(def ^:private ^:dbg-flag potential-field-colors? false)
(def ^:private ^:dbg-flag cell-entities? false)
(def ^:private ^:dbg-flag cell-occupied? false)

(defn- tile-debug []
  (let [grid world-grid
        world-camera (world-camera)
        [left-x right-x bottom-y top-y] (frustum world-camera)]

    (when tile-grid?
      (draw-grid (int left-x) (int bottom-y)
                 (inc (int (world-viewport-width)))
                 (+ 2 (int (world-viewport-height)))
                 1 1 [1 1 1 0.8]))

    (doseq [[x y] (visible-tiles world-camera)
            :let [cell (grid [x y])]
            :when cell
            :let [cell* @cell]]

      (when (and cell-entities? (seq (:entities cell*)))
        (draw-filled-rectangle x y 1 1 [1 0 0 0.6]))

      (when (and cell-occupied? (seq (:occupied cell*)))
        (draw-filled-rectangle x y 1 1 [0 0 1 0.6]))

      (when potential-field-colors?
        (let [faction :good
              {:keys [distance entity]} (faction cell*)]
          (when distance
            (let [ratio (/ distance (factions-iterations faction))]
              (draw-filled-rectangle x y 1 1 [ratio (- 1 ratio) ratio 0.6]))))))))

(def ^:private ^:dbg-flag highlight-blocked-cell? true)

(defn- highlight-mouseover-tile []
  (when highlight-blocked-cell?
    (let [[x y] (->tile (world-mouse-position))
          cell (get world-grid [x y])]
      (when (and cell (#{:air :none} (:movement @cell)))
        (draw-rectangle x y 1 1
                        (case (:movement @cell)
                          :air  [1 1 0 0.5]
                          :none [1 0 0 0.5]))))))

(defn- before-entities [] (tile-debug))

(defn- after-entities []
  #_(geom-test)
  (highlight-mouseover-tile))

(defn- calculate-mouseover-entity []
  (let [player-entity* @world-player
        hits (remove #(= (:z-order %) :z-order/effect) ; or: only items/creatures/projectiles.
                     (map deref
                          (point->entities (world-mouse-position))))]
    (->> render-order
         (sort-by-order hits :z-order)
         reverse
         (filter #(line-of-sight? player-entity* %))
         first
         :entity/id)))

(defn- update-mouseover-entity []
  (let [entity (if (mouse-on-actor?)
                 nil
                 (calculate-mouseover-entity))]
    [(when-let [old-entity mouseover-entity]
       [:e/dissoc old-entity :entity/mouseover?])
     (when entity
       [:e/assoc entity :entity/mouseover? true])
     (fn []
       (bind-root #'mouseover-entity entity)
       nil)]))

(def ^:private ^:dbg-flag pausing? true)

(defn- player-state-pause-game? [] (pause-game? (state-obj @world-player)))
(defn- player-update-state      [] (manual-tick (state-obj @world-player)))

(defn- player-unpaused? []
  (or (key-just-pressed? :keys/p)
      (key-pressed? :keys/space))) ; FIXMe :keys? shouldnt it be just :space?

(defn- update-game-paused []
  (bind-root #'world-paused? (or entity-tick-error
                                 (and pausing?
                                      (player-state-pause-game?)
                                      (not (player-unpaused?)))))
  nil)

(defn- update-world []
  (update-time (min (delta-time) max-delta-time))
  (let [entities (active-entities)]
    (potential-fields-update! entities)
    (try (tick-entities! entities)
         (catch Throwable t
           (error-window! t)
           (bind-root #'entity-tick-error t))))
  nil)

(defn- game-loop []
  (effect! [player-update-state
            update-mouseover-entity ; this do always so can get debug info even when game not running
            update-game-paused
            #(when-not world-paused?
               (update-world))
            remove-destroyed-entities! ; do not pause this as for example pickup item, should be destroyed.
            ]))

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

(defn render-map [light-position]
  (draw-tiled-map world-tiled-map
                  (->tile-color-setter (atom nil)
                                       light-position
                                       world-raycaster
                                       explored-tile-corners))
  #_(reset! do-once false))

(defn- render-world! []
  (camera-set-position! (world-camera) (:position @world-player))
  (render-map (camera-position (world-camera)))
  (render-world-view! (fn []
                        (before-entities)
                        (render-entities! (map deref (active-entities)))
                        (after-entities))))

(defn- hotkey->window-id []
  (merge {:keys/i :inventory-window
          :keys/e :entity-info-window}
         (when dev-mode?
           {:keys/z :debug-window})))

(defn- check-window-hotkeys []
  (doseq [[hotkey window-id] (hotkey->window-id)
          :when (key-just-pressed? hotkey)]
    (toggle-visible! (get (:windows (stage-get)) window-id))))

(defn- close-windows?! []
  (let [windows (children (:windows (stage-get)))]
    (if (some visible? windows)
      (do
       (run! #(set-visible! % false) windows)
       true))))

(defn- adjust-zoom [camera by] ; DRY map editor
  (set-zoom! camera (max 0.1 (+ (zoom camera) by))))

(def ^:private zoom-speed 0.05)

(defn- check-zoom-keys []
  (let [camera (world-camera)]
    (when (key-pressed? :keys/minus)  (adjust-zoom camera    zoom-speed))
    (when (key-pressed? :keys/equals) (adjust-zoom camera (- zoom-speed)))))

; TODO move to actor/stage listeners ? then input processor used ....
(defn- check-key-input []
  (check-zoom-keys)
  (check-window-hotkeys)
  (cond (and (key-just-pressed? :keys/escape)
             (not (close-windows?!)))
        (change-screen :screens/options-menu)

        ; TODO not implementing StageSubScreen so NPE no screen-render!
        #_(key-just-pressed? :keys/tab)
        #_(change-screen :screens/minimap)))

(defc :world/sub-screen
  (screen-exit [_]
    (set-cursor! :cursors/default))

  (screen-render [_]
    (render-world!)
    (game-loop)
    (check-key-input)))

(derive :screens/world :screens/stage)
(defc :screens/world
  (->mk [_]
    {:stage (->stage [])
     :sub-screen [:world/sub-screen]}))

(defn- start-game-fn [world-id]
  (fn []
    (change-screen :screens/world)
    (add-world-ctx world-id)))

(defn- ->buttons []
  (->table {:rows (remove nil? (concat
                                   (for [{:keys [property/id]} (all-properties :properties/worlds)]
                                     [(->text-button (str "Start " id) (start-game-fn id))])
                                   [(when dev-mode?
                                      [(->text-button "Map editor" #(change-screen :screens/map-editor))])
                                    (when dev-mode?
                                      [(->text-button "Property editor" #(change-screen :screens/property-editor))])
                                    [(->text-button "Exit" exit-app!)]]))
               :cell-defaults {:pad-bottom 25}
               :fill-parent? true}))

(defc :main/sub-screen
  (screen-enter [_]
    (set-cursor! :cursors/default)))

(defn- ->actors []
  [(->background-image)
   (->buttons)
   (->actor {:act (fn []
                    (when (key-just-pressed? :keys/escape)
                      (exit-app!)))})])

(derive :screens/main-menu :screens/stage)
(defc :screens/main-menu
  (->mk [[k _]]
    {:sub-screen [:main/sub-screen]
     :stage (->stage (->actors))}))

(defprotocol ^:private StatusCheckBox
  (^:private get-text [this])
  (^:private get-state [this])
  (^:private set-state [this is-selected]))

(deftype VarStatusCheckBox [^clojure.lang.Var avar]
  StatusCheckBox
  (get-text [this]
    (let [m (meta avar)]
      (str "[LIGHT_GRAY]" (str (:ns m)) "/[WHITE]" (name (:name m)) "[]")))

  (get-state [this]
    @avar)

  (set-state [this is-selected]
    (bind-root avar is-selected)))

; TODO not using clojure.gdx ns ... only core

(defn- debug-flags [] ;
  (apply concat
         ; TODO
         (for [nmspace (get-namespaces #{"core"})] ; DRY in core.component check ns-name & core.app require all ... core.components
           (get-vars nmspace (fn [avar] (:dbg-flag (meta avar)))))))

; TODO FIXME IF THE FLAGS ARE CHANGED MANUALLY IN THE REPL THIS IS NOT REFRESHED
; -. rebuild it on window open ...
(def ^:private debug-flags (map ->VarStatusCheckBox (debug-flags)))

(def ^:private key-help-text
  "[W][A][S][D] - Move\n[I] - Inventory window\n[E] - Entity Info window\n[-]/[=] - Zoom\n[TAB] - Minimap\n[P]/[SPACE] - Unpause")

(defn- create-table []
  (->table {:rows (concat
                   [[(->label key-help-text)]]
                   (when dev-mode? [[(->label "[Z] - Debug window")]])
                   (when dev-mode? (for [check-box debug-flags]
                                     [(->check-box (get-text check-box)
                                                   (partial set-state check-box)
                                                   (boolean (get-state check-box)))]))
                   [[(->text-button "Resume" #(change-screen :screens/world))]
                    [(->text-button "Exit"   #(change-screen :screens/main-menu))]])
            :fill-parent? true
            :cell-defaults {:pad-bottom 10}}))

(defc :options/sub-screen
  (screen-render [_]
    (when (key-just-pressed? :keys/escape)
      (change-screen :screens/world))))

(derive :screens/options-menu :screens/stage)
(defc :screens/options-menu
  (->mk [_]
    {:stage (->stage [(->background-image)
                      (create-table)])
     :sub-screen [:options/sub-screen]}))

(defn -main []
  (start-app! :title "Core"
              :width 1440
              :height 900
              :full-screen? false
              :fps 60
              :resources "resources/"
              :properties "resources/properties.edn"
              :graphics {:cursors {:cursors/bag ["bag001" [0 0]]
                                   :cursors/black-x ["black_x" [0 0]]
                                   :cursors/default ["default" [0 0]]
                                   :cursors/denied ["denied" [16 16]]
                                   :cursors/hand-before-grab ["hand004" [4 16]]
                                   :cursors/hand-before-grab-gray ["hand004_gray" [4 16]]
                                   :cursors/hand-grab ["hand003" [4 16]]
                                   :cursors/move-window ["move002" [16 16]]
                                   :cursors/no-skill-selected ["denied003" [0 0]]
                                   :cursors/over-button ["hand002" [0 0]]
                                   :cursors/sandclock ["sandclock" [16 16]]
                                   :cursors/skill-not-usable ["x007" [0 0]]
                                   :cursors/use-skill ["pointer004" [0 0]]
                                   :cursors/walking ["walking" [16 16]]}
                         :default-font {:file "fonts/exocet/films.EXL_____.ttf"
                                        :size 16
                                        :quality-scaling 2}
                         :views {:gui-view {:world-width 1440
                                            :world-height 900}
                                 :world-view {:world-width 1440
                                              :world-height 900
                                              :tile-size 48}}}
              :ui :skin-scale/x1
              :screen-ks [:screens/main-menu
                          :screens/map-editor
                          :screens/options-menu
                          :screens/property-editor
                          :screens/world]))
