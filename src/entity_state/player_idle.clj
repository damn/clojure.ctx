(ns entity-state.player-idle
  (:require [utils.core :refer [safe-merge]]
            [gdx.input :as input]
            [gdx.input.buttons :as buttons]
            [api.graphics :as g]
            [api.scene2d.actor :refer [visible? toggle-visible! parent] :as actor]
            [api.scene2d.ui.button :refer [button?]]
            [api.scene2d.ui.window :refer [window-title-bar?]]
            [math.vector :as v]
            [utils.wasd-movement :refer [WASD-movement-vector]]
            [api.context :as ctx :refer [mouse-on-stage-actor? get-property inventory-window selected-skill]]
            [api.entity :as entity]
            [api.entity-state :as state]
            [entity-state.active-skill :refer [skill-usable-state]]))

(defn- denied [text]
  [[:tx/sound "sounds/bfxr_denied.wav"]
   [:tx/msg-to-player text]])

(defmulti ^:private on-clicked
  (fn [_context entity*]
    (:type (:entity/clickable entity*))))

(defmethod on-clicked :clickable/item
  [context clicked-entity*]
  (let [player-entity* (ctx/player-entity* context)
        item (:entity/item clicked-entity*)
        clicked-entity (:entity/id clicked-entity*)]
    (cond
     (visible? (inventory-window context))
     [[:tx/sound "sounds/bfxr_takeit.wav"]
      [:tx/destroy clicked-entity]
      [:tx/event (:entity/id player-entity*) :pickup-item item]]

     (entity/can-pickup-item? player-entity* item)
     [[:tx/sound "sounds/bfxr_pickup.wav"]
      [:tx/destroy clicked-entity]
      [:tx/pickup-item (:entity/id player-entity*) item]]

     :else
     [[:tx/sound "sounds/bfxr_denied.wav"]
      [:tx/msg-to-player "Your Inventory is full"]])))

(defmethod on-clicked :clickable/player
  [ctx _clicked-entity*]
  (toggle-visible! (inventory-window ctx))) ; TODO no tx

(defmethod on-clicked :clickable/princess
  [ctx _clicked-entity*]
  [[:tx/event (:entity/id (ctx/player-entity* ctx)) :found-princess]])

(defn- clickable->cursor [mouseover-entity* too-far-away?]
  (case (:type (:entity/clickable mouseover-entity*))
    :clickable/item (if too-far-away?
                      :cursors/hand-before-grab-gray
                      :cursors/hand-before-grab)
    :clickable/player :cursors/bag
    :clickable/princess (if too-far-away?
                          :cursors/princess-gray
                          :cursors/princess)))

(defn- ->clickable-mouseover-entity-interaction [ctx player-entity* mouseover-entity*]
  (if (and (< (v/distance (entity/position player-entity*)
                          (entity/position mouseover-entity*))
              (:entity/click-distance-tiles player-entity*)))
    [(clickable->cursor mouseover-entity* false) (fn [] (on-clicked ctx mouseover-entity*))]
    [(clickable->cursor mouseover-entity* true)  (fn [] (denied "Too far away"))]))

; TODO move to inventory-window extend Context
(defn- inventory-cell-with-item? [ctx actor]
  (and (parent actor)
       (= "inventory-cell" (actor/name (parent actor)))
       (get-in (:entity/inventory (ctx/player-entity* ctx))
               (actor/id (parent actor)))))

(defn- mouseover-actor->cursor [ctx]
  (let [actor (mouse-on-stage-actor? ctx)]
    (cond
     (inventory-cell-with-item? ctx actor) :cursors/hand-before-grab
     (window-title-bar? actor) :cursors/move-window
     (button? actor) :cursors/over-button
     :else :cursors/default)))

(defn- ->effect-context [ctx entity*]
  (let [target* (ctx/mouseover-entity* ctx)
        target-position (or (and target* (entity/position target*))
                            (ctx/world-mouse-position ctx))]
    (ctx/map->Context
     {:effect/source (:entity/id entity*)
      :effect/target (:entity/id target*)
      :effect/target-position target-position
      :effect/direction (v/direction (entity/position entity*) target-position)})))

(defn- ->interaction-state [context entity*]
  (let [mouseover-entity* (ctx/mouseover-entity* context)]
    (cond
     (mouse-on-stage-actor? context)
     [(mouseover-actor->cursor context)
      (fn []
        nil)] ; handled by actors themself, they check player state

     (and mouseover-entity*
          (:entity/clickable mouseover-entity*))
     (->clickable-mouseover-entity-interaction context entity* mouseover-entity*)

     :else
     (if-let [skill-id (selected-skill context)]
       (let [skill (skill-id (:entity/skills entity*))
             effect-ctx (->effect-context context entity*)
             state (skill-usable-state (safe-merge context effect-ctx)
                                       entity*
                                       skill)]
         (if (= state :usable)
           (do
            ; TODO cursor AS OF SKILL effect (SWORD !) / show already what the effect would do ? e.g. if it would kill highlight
            ; different color ?
            ; => e.g. meditation no TARGET .. etc.
            [:cursors/use-skill
             (fn []
               [[:tx/event (:entity/id entity*) :start-action [skill effect-ctx]]])])
           (do
            ; TODO cursor as of usable state
            ; cooldown -> sanduhr kleine
            ; not-enough-mana x mit kreis?
            ; invalid-params -> depends on params ...
            [:cursors/skill-not-usable
             (fn []
               (denied (case state
                         :cooldown "Skill is still on cooldown"
                         :not-enough-mana "Not enough mana"
                         :invalid-params "Cannot use this here")))])))
       [:cursors/no-skill-selected
        (fn [] (denied "No selected skill"))]))))

(defrecord PlayerIdle []
  state/PlayerState
  (player-enter [_])
  (pause-game? [_] true)
  (manual-tick [_ entity* context]
    (if-let [movement-vector (WASD-movement-vector context)]
      [[:tx/event (:entity/id entity*) :movement-input movement-vector]]
      (let [[cursor on-click] (->interaction-state context entity*)]
        (cons [:tx.context.cursor/set cursor]
              (when (input/button-just-pressed? buttons/left)
                (on-click))))))

  (clicked-inventory-cell [_ {:keys [entity/id entity/inventory]} cell]
    (when-let [item (get-in inventory cell)]
      [[:tx/sound "sounds/bfxr_takeit.wav"]
       [:tx/event id :pickup-item item]
       [:tx/remove-item id cell]]))

  (clicked-skillmenu-skill [_ {:keys [entity/id entity/free-skill-points] :as entity*} skill]
    (when (and (pos? free-skill-points)
               (not (entity/has-skill? entity* skill)))
      [[:tx.entity/assoc id :entity/free-skill-points (dec free-skill-points)]
       [:tx/add-skill id skill]]))
  ; TODO no else case, no visible fsp..

  state/State
  (enter [_ entity* _ctx])
  (exit  [_ entity* context])
  (tick [_ entity* _context])
  (render-below [_ entity* g ctx])
  (render-above [_ entity* g ctx])
  (render-info  [_ entity* g ctx]))
