(in-ns 'core.app)

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


(defcomponent :main/sub-screen
  (screen-enter [_]
    (set-cursor! :cursors/default)))

(defn- ->actors []
  [(->background-image)
   (->buttons)
   (->actor {:act (fn []
                    (when (key-just-pressed? :keys/escape)
                      (exit-app!)))})])

(derive :screens/main-menu :screens/stage)
(defcomponent :screens/main-menu
  (->mk [[k _]]
    {:sub-screen [:main/sub-screen]
     :stage (->stage (->actors))}))
