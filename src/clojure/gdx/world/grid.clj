(in-ns 'clojure.gdx)

(defprotocol Grid
  (cached-adjacent-cells [grid cell])
  (rectangle->cells [grid rectangle])
  (circle->cells    [grid circle])
  (circle->entities [grid circle]))

(defn- rectangle->tiles
  [{[x y] :left-bottom :keys [left-bottom width height]}]
  {:pre [left-bottom width height]}
  (let [x       (float x)
        y       (float y)
        width   (float width)
        height  (float height)
        l (int x)
        b (int y)
        r (int (+ x width))
        t (int (+ y height))]
    (set
     (if (or (> width 1) (> height 1))
       (for [x (range l (inc r))
             y (range b (inc t))]
         [x y])
       [[l b] [l t] [r b] [r t]]))))

(defn- set-cells! [grid entity]
  (let [cells (rectangle->cells grid @entity)]
    (assert (not-any? nil? cells))
    (swap! entity assoc ::touched-cells cells)
    (doseq [cell cells]
      (assert (not (get (:entities @cell) entity)))
      (swap! cell update :entities conj entity))))

(defn- remove-from-cells! [entity]
  (doseq [cell (::touched-cells @entity)]
    (assert (get (:entities @cell) entity))
    (swap! cell update :entities disj entity)))

; could use inside tiles only for >1 tile bodies (for example size 4.5 use 4x4 tiles for occupied)
; => only now there are no >1 tile entities anyway
(defn- rectangle->occupied-cells [grid {:keys [left-bottom width height] :as rectangle}]
  (if (or (> (float width) 1) (> (float height) 1))
    (rectangle->cells grid rectangle)
    [(get grid
          [(int (+ (float (left-bottom 0)) (/ (float width) 2)))
           (int (+ (float (left-bottom 1)) (/ (float height) 2)))])]))

(defn- set-occupied-cells! [grid entity]
  (let [cells (rectangle->occupied-cells grid @entity)]
    (doseq [cell cells]
      (assert (not (get (:occupied @cell) entity)))
      (swap! cell update :occupied conj entity))
    (swap! entity assoc ::occupied-cells cells)))

(defn- remove-from-occupied-cells! [entity]
  (doseq [cell (::occupied-cells @entity)]
    (assert (get (:occupied @cell) entity))
    (swap! cell update :occupied disj entity)))

(defn cells->entities [cells*]
  (into #{} (mapcat :entities) cells*))

; TODO LAZY SEQ @ g/get-8-neighbour-positions !!
; https://github.com/damn/g/blob/master/src/data/grid2d.clj#L126
(extend-type data.grid2d.Grid2D
  Grid
  (cached-adjacent-cells [grid cell]
    (if-let [result (:adjacent-cells @cell)]
      result
      (let [result (into [] (keep grid) (-> @cell :position g/get-8-neighbour-positions))]
        (swap! cell assoc :adjacent-cells result)
        result)))

  (rectangle->cells [grid rectangle]
    (into [] (keep grid) (rectangle->tiles rectangle)))

  (circle->cells [grid circle]
    (->> circle
         circle->outer-rectangle
         (rectangle->cells grid)))

  (circle->entities [grid circle]
    (->> (circle->cells grid circle)
         (map deref)
         cells->entities
         (filter #(shape-collides? circle @%)))))

(declare world-grid)

(defn point->entities [position]
  (when-let [cell (get world-grid (->tile position))]
    (filter #(point-in-rect? position @%)
            (:entities @cell))))

(defn grid-add-entity! [entity]
  (let [grid world-grid]
    (set-cells! grid entity)
    (when (:collides? @entity)
      (set-occupied-cells! grid entity))))

(defn grid-remove-entity! [entity]
  (remove-from-cells! entity)
  (when (:collides? @entity)
    (remove-from-occupied-cells! entity)))

(defn grid-entity-position-changed! [entity]
  (let [grid world-grid]
    (remove-from-cells! entity)
    (set-cells! grid entity)
    (when (:collides? @entity)
      (remove-from-occupied-cells! entity)
      (set-occupied-cells! grid entity))))

(defprotocol GridCell
  (blocked? [cell* z-order])
  (blocks-vision? [cell*])
  (occupied-by-other? [cell* entity]
                      "returns true if there is some occupying body with center-tile = this cell
                      or a multiple-cell-size body which touches this cell.")
  (nearest-entity          [cell* faction])
  (nearest-entity-distance [cell* faction]))

(defrecord RCell [position
                  middle ; only used @ potential-field-follow-to-enemy -> can remove it.
                  adjacent-cells
                  movement
                  entities
                  occupied
                  good
                  evil]
  GridCell
  (blocked? [_ z-order]
    (case movement
      :none true ; wall
      :air (case z-order ; water/doodads
             :z-order/flying false
             :z-order/ground true)
      :all false)) ; ground/floor

  (blocks-vision? [_]
    (= movement :none))

  (occupied-by-other? [_ entity]
    (some #(not= % entity) occupied)) ; contains? faster?

  (nearest-entity [this faction]
    (-> this faction :entity))

  (nearest-entity-distance [this faction]
    (-> this faction :distance)))

(defn- create-cell [position movement]
  {:pre [(#{:none :air :all} movement)]}
  (map->RCell
   {:position position
    :middle (tile->middle position)
    :movement movement
    :entities #{}
    :occupied #{}}))

(defn init-world-grid! [width height position->value]
  (bind-root #'world-grid (g/create-grid width
                                         height
                                         #(atom (create-cell % (position->value %))))))
