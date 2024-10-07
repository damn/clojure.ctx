(in-ns 'core.app)

(load "screens/world/debug_render")

(defn- calculate-mouseover-entity []
  (let [player-entity* @player-entity
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

(defn- player-unpaused? []
  (or (key-just-pressed? :keys/p)
      (key-pressed? :keys/space)))

(defn- update-game-paused []
  (bind-root #'world-paused? (or world/entity-tick-error
                                 (and pausing?
                                      (player-state-pause-game?)
                                      (not (player-unpaused?)))))
  nil)

(defn- update-world []
  (world/update-time (min (delta-time) max-delta-time))
  (let [entities (active-entities)]
    (potential-fields-update! entities)
    (try (tick-entities! entities)
         (catch Throwable t
           (error-window! t)
           (bind-root #'world/entity-tick-error t))))
  nil)

(defn- game-loop []
  (effect! [player-update-state
            update-mouseover-entity ; this do always so can get debug info even when game not running
            update-game-paused
            #(when-not world-paused?
               (update-world))
            remove-destroyed-entities! ; do not pause this as for example pickup item, should be destroyed.
            ]))

(defn- render-world! []
  (camera-set-position! (world-camera) (:position @player-entity))
  (world/render-map (camera-position (world-camera)))
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

(defcomponent :world/sub-screen
  (screen-exit [_]
    (set-cursor! :cursors/default))

  (screen-render [_]
    (render-world!)
    (game-loop)
    (check-key-input)))

(derive :screens/world :screens/stage)
(defcomponent :screens/world
  (->mk [_]
    {:stage (->stage [])
     :sub-screen [:world/sub-screen]}))
