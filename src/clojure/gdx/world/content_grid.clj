(in-ns 'clojure.gdx)

(declare content-grid)

(defn content-grid-update-entity! [entity]
  (let [{:keys [grid cell-w cell-h]} content-grid
        {::keys [content-cell] :as entity*} @entity
        [x y] (:position entity*)
        new-cell (get grid [(int (/ x cell-w))
                            (int (/ y cell-h))])]
    (when-not (= content-cell new-cell)
      (swap! new-cell update :entities conj entity)
      (swap! entity assoc ::content-cell new-cell)
      (when content-cell
        (swap! content-cell update :entities disj entity)))))

(defn content-grid-remove-entity! [entity]
  (-> @entity
      ::content-cell
      (swap! update :entities disj entity)))

(defn- active-entities* [center-entity*]
  (let [{:keys [grid]} content-grid]
    (->> (let [idx (-> center-entity*
                       ::content-cell
                       deref
                       :idx)]
           (cons idx (g/get-8-neighbour-positions idx)))
         (keep grid)
         (mapcat (comp :entities deref)))))

(defn active-entities []
  (active-entities* @player-entity))

(defn init-content-grid! [& {:keys [cell-size width height]}]
  (bind-root
   #'content-grid
   {:grid (g/create-grid (inc (int (/ width  cell-size))) ; inc because corners
                         (inc (int (/ height cell-size)))
                         (fn [idx]
                           (atom {:idx idx,
                                  :entities #{}})))
    :cell-w cell-size
    :cell-h cell-size}))
