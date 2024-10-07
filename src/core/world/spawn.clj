(in-ns 'core.world)

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
