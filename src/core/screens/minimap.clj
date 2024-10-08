(in-ns 'core.app)

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
