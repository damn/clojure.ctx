(in-ns 'clojure.gdx)

(declare player-entity
         player-update-state
         player-state-pause-game?
         player-clicked-inventory
         player-clicked-skillmenu)

; for potential-field
(declare entity-tile
         enemy-faction)

(load "gdx/world/raycaster"
      "gdx/world/grid"
      "gdx/world/potential_fields"
      "gdx/world/content_grid")

(declare add-world-ctx)

(declare world-paused?)

(def mouseover-entity nil)

(defn mouseover-entity* []
  (when-let [entity mouseover-entity]
    @entity))

(declare explored-tile-corners)

(declare ^{:doc "The game logic update delta-time. Different then delta-time-raw because it is bounded by a maximum value for entity movement speed."}
         world-delta

         ^{:doc "The elapsed in-game-time (not counting when game is paused)."}
         elapsed-time

         ^{:doc "The game-logic frame number, starting with 1. (not counting when game is paused)"}
         logic-frame)

(defrecord Counter [duration stop-time])

(defn ->counter [duration]
  {:pre [(>= duration 0)]}
  (->Counter duration (+ elapsed-time duration)))

(defn stopped? [{:keys [stop-time]}]
  (>= elapsed-time stop-time))

(defn reset [{:keys [duration] :as counter}]
  (assoc counter :stop-time (+ elapsed-time duration)))

(defn finished-ratio [{:keys [duration stop-time] :as counter}]
  {:post [(<= 0 % 1)]}
  (if (stopped? counter)
    0
    ; min 1 because floating point math inaccuracies
    (min 1 (/ (- stop-time elapsed-time) duration))))
