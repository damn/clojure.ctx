(in-ns 'core.world)

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
                         (let [player-entity* @player-entity
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
     "skill cooldowns: " (skill-info @player-entity) "\n"
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

(defcomponent :tx.action-bar/add
  (do! [[_ {:keys [property/id entity/image] :as skill}]]
    (let [{:keys [horizontal-group button-group]} (get-action-bar)
          button (->image-button image (fn []) {:scale image-scale})]
      (set-id! button id)
      (add-tooltip! button #(->info-text skill)) ; (assoc ctx :effect/source (player-entity)) FIXME
      (add-actor! horizontal-group button)
      (bg-add! button-group button)
      nil)))

(defcomponent :tx.action-bar/remove
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
 ;  ( (skills/text skill-id player-entity))
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

(defcomponent :widgets/player-message
  (->mk [_]
    (->actor {:draw draw-player-message
              :act check-remove-message})))

(defcomponent :tx/msg-to-player
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
