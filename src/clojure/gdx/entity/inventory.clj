(in-ns 'clojure.gdx)

(def-property-type :properties/items
  {:schema [:property/pretty-name
            :entity/image
            :item/slot
            [:item/modifiers {:optional true}]]
   :overview {:title "Items"
              :columns 20
              :image/scale 1.1
              :sort-by-fn #(vector (if-let [slot (:item/slot %)]
                                     (name slot)
                                     "")
                             (name (:property/id %)))}})

(def ^:private empty-inventory
  (->> #:inventory.slot{:bag      [6 4]
                        :weapon   [1 1]
                        :shield   [1 1]
                        :helm     [1 1]
                        :chest    [1 1]
                        :leg      [1 1]
                        :glove    [1 1]
                        :boot     [1 1]
                        :cloak    [1 1]
                        :necklace [1 1]
                        :rings    [2 1]}
       (map (fn [[slot [width height]]]
              [slot (g/create-grid width height (constantly nil))]))
       (into {})))

(defc :item/modifiers
  {:data [:components-ns :modifier]
   :let modifiers}
  (info-text [_]
    (when (seq modifiers)
      (mod-info-text modifiers))))

(defc :item/slot
  {:data [:enum (keys empty-inventory)]})

(def ^:private body-props
  {:width 0.75
   :height 0.75
   :z-order :z-order/on-ground})

(defc :tx/item
  (do! [[_ position item]]
    [[:e/create position body-props {:entity/image (:entity/image item)
                                     :entity/item item
                                     :entity/clickable {:type :clickable/item
                                                        :text (:property/pretty-name item)}}]]))

(defn- cells-and-items [inventory slot]
  (for [[position item] (slot inventory)]
    [[slot position] item]))

(defn valid-slot? [[slot _] item]
  (or (= :inventory.slot/bag slot)
      (= (:item/slot item) slot)))

(defn- applies-modifiers? [[slot _]]
  (not= :inventory.slot/bag slot))

(defn stackable? [item-a item-b]
  (and (:count item-a)
       (:count item-b) ; this is not required but can be asserted, all of one name should have count if others have count
       (= (:property/id item-a) (:property/id item-b))))

(defn- set-item [{:keys [entity/id] :as entity*} cell item]
  (let [inventory (:entity/inventory entity*)]
    (assert (and (nil? (get-in inventory cell))
                 (valid-slot? cell item))))
  [[:e/assoc-in id (cons :entity/inventory cell) item]
   (when (applies-modifiers? cell)
     [:tx/apply-modifiers id (:item/modifiers item)])
   (when (:entity/player? entity*)
     [:tx/set-item-image-in-widget cell item])])

(defn- remove-item [{:keys [entity/id] :as entity*} cell]
  (let [item (get-in (:entity/inventory entity*) cell)]
    (assert item)
    [[:e/assoc-in id (cons :entity/inventory cell) nil]
     (when (applies-modifiers? cell)
       [:tx/reverse-modifiers id (:item/modifiers item)])
     (when (:entity/player? entity*)
       [:tx/remove-item-from-widget cell])]))

(defc :tx/set-item
  (do! [[_ entity cell item]]
    (set-item @entity cell item)))

(defc :tx/remove-item
  (do! [[_ entity cell]]
    (remove-item @entity cell)))

; TODO doesnt exist, stackable, usable items with action/skillbar thingy
#_(defn remove-one-item [entity cell]
  (let [item (get-in (:entity/inventory @entity) cell)]
    (if (and (:count item)
             (> (:count item) 1))
      (do
       ; TODO this doesnt make sense with modifiers ! (triggered 2 times if available)
       ; first remove and then place, just update directly  item ...
       (remove-item! entity cell)
       (set-item! entity cell (update item :count dec)))
      (remove-item! entity cell))))

; TODO no items which stack are available
(defn- stack-item [entity* cell item]
  (let [cell-item (get-in (:entity/inventory entity*) cell)]
    (assert (stackable? item cell-item))
    ; TODO this doesnt make sense with modifiers ! (triggered 2 times if available)
    ; first remove and then place, just update directly  item ...
    (concat (remove-item entity* cell)
            (set-item entity* cell (update cell-item :count + (:count item))))))

(defc :tx/stack-item
  (do! [[_ entity cell item]]
    (stack-item @entity cell item)))

(defn- try-put-item-in [entity* slot item]
  (let [inventory (:entity/inventory entity*)
        cells-items (cells-and-items inventory slot)
        [cell _cell-item] (find-first (fn [[_cell cell-item]] (stackable? item cell-item))
                                      cells-items)]
    (if cell
      (stack-item entity* cell item)
      (when-let [[empty-cell] (find-first (fn [[_cell item]] (nil? item))
                                          cells-items)]
        (set-item entity* empty-cell item)))))

(defn- pickup-item [entity* item]
  (or
   (try-put-item-in entity* (:item/slot item)   item)
   (try-put-item-in entity* :inventory.slot/bag item)))

(defc :tx/pickup-item
  (do! [[_ entity item]]
    (pickup-item @entity item)))

(extend-type clojure.gdx.Entity
  Inventory
  (can-pickup-item? [entity* item]
    (boolean (pickup-item entity* item))))

(defc :entity/inventory
  {:data [:one-to-many :properties/items]}
  (create [[_ items] eid]
    (cons [:e/assoc eid :entity/inventory empty-inventory]
          (for [item items]
            [:tx/pickup-item eid item]))))


; Items are also smaller than 48x48 all of them
; so wasting space ...
; can maybe make a smaller textureatlas or something...

(def ^:private cell-size 48)
(def ^:private droppable-color    [0   0.6 0 0.8])
(def ^:private not-allowed-color  [0.6 0   0 0.8])

(defn- draw-cell-rect [player-entity* x y mouseover? cell]
  (draw-rectangle x y cell-size cell-size :gray)
  (when (and mouseover?
             (= :player-item-on-cursor (entity-state player-entity*)))
    (let [item (:entity/item-on-cursor player-entity*)
          color (if (valid-slot? cell item)
                  droppable-color
                  not-allowed-color)]
      (draw-filled-rectangle (inc x) (inc y) (- cell-size 2) (- cell-size 2) color))))

; TODO why do I need to call getX ?
; is not layouted automatically to cell , use 0/0 ??
; (maybe (.setTransform stack true) ? , but docs say it should work anyway
(defn- draw-rect-actor []
  (->ui-widget
   (fn [this]
     (binding [*unit-scale* 1]
       (draw-cell-rect @player-entity
                       (actor-x this)
                       (actor-y this)
                       (a-mouseover? this (gui-mouse-position))
                       (actor-id (parent this)))))))

(defn- ->cell [slot->background slot & {:keys [position]}]
  (let [cell [slot (or position [0 0])]
        image-widget (->ui-image-widget (slot->background slot) {:id :image})
        stack (->stack [(draw-rect-actor) image-widget])]
    (set-name! stack "inventory-cell")
    (set-id! stack cell)
    (add-listener! stack (->click-listener
                           (clicked [event x y]
                             (effect! (player-clicked-inventory cell)))))
    stack))

(defn- slot->background []
  (let [sheet (sprite-sheet "images/items.png" 48 48)]
    (->> #:inventory.slot {:weapon   0
                           :shield   1
                           :rings    2
                           :necklace 3
                           :helm     4
                           :cloak    5
                           :chest    6
                           :leg      7
                           :glove    8
                           :boot     9
                           :bag      10} ; transparent
         (map (fn [[slot y]]
                (let [drawable (->texture-region-drawable (:texture-region (sprite sheet [21 (+ y 2)])))]
                  (set-min-size! drawable cell-size)
                  [slot
                   (->tinted-drawable drawable (->color 1 1 1 0.4))])))
         (into {}))))

; TODO move together with empty-inventory definition ?
(defn- redo-table! [table slot->background]
  ; cannot do add-rows, need bag :position idx
  (let [cell (fn [& args] (apply ->cell slot->background args))]
    (t-clear! table) ; no need as we create new table ... TODO
    (doto table t-add! t-add!
      (t-add! (cell :inventory.slot/helm))
      (t-add! (cell :inventory.slot/necklace)) t-row!)
    (doto table t-add!
      (t-add! (cell :inventory.slot/weapon))
      (t-add! (cell :inventory.slot/chest))
      (t-add! (cell :inventory.slot/cloak))
      (t-add! (cell :inventory.slot/shield)) t-row!)
    (doto table t-add! t-add!
      (t-add! (cell :inventory.slot/leg)) t-row!)
    (doto table t-add!
      (t-add! (cell :inventory.slot/glove))
      (t-add! (cell :inventory.slot/rings :position [0 0]))
      (t-add! (cell :inventory.slot/rings :position [1 0]))
      (t-add! (cell :inventory.slot/boot)) t-row!)
    ; TODO add separator
    (doseq [y (range (g/height (:inventory.slot/bag empty-inventory)))]
      (doseq [x (range (g/width (:inventory.slot/bag empty-inventory)))]
        (t-add! table (cell :inventory.slot/bag :position [x y])))
      (t-row! table))))

(defn ->inventory-window [{:keys [slot->background]}]
  (let [table (->table {:id ::table})]
    (redo-table! table slot->background)
    (->window {:title "Inventory"
               :id :inventory-window
               :visible? false
               :pack? true
               :position [(gui-viewport-width)
                          (gui-viewport-height)]
               :rows [[{:actor table :pad 4}]]})))

(defn ->inventory-window-data [] (slot->background))

(declare world-widgets)

(defn- get-inventory []
  {:table (::table (get (:windows (stage-get)) :inventory-window))
   :slot->background (:slot->background world-widgets)})

(defc :tx/set-item-image-in-widget
  (do! [[_ cell item]]
    (let [{:keys [table]} (get-inventory)
          cell-widget (get table cell)
          image-widget (get cell-widget :image)
          drawable (->texture-region-drawable (:texture-region (:entity/image item)))]
      (set-min-size! drawable cell-size)
      (set-drawable! image-widget drawable)
      (add-tooltip! cell-widget #(->info-text item))
      nil)))

(defc :tx/remove-item-from-widget
  (do! [[_ cell]]
    (let [{:keys [table slot->background]} (get-inventory)
          cell-widget (get table cell)
          image-widget (get cell-widget :image)]
      (set-drawable! image-widget (slot->background (cell 0)))
      (remove-tooltip! cell-widget)
      nil)))
