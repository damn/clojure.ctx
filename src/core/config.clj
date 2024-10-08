(ns core.config
  (:require [clojure.gdx :refer :all]))

(def-markup-color "ITEM_GOLD" [0.84 0.8 0.52])

(defc :property/pretty-name
  {:data :string
   :let value}
  (info-text [_]
    (str "[ITEM_GOLD]"value"[]")))

(bind-root #'clojure.gdx/info-text-k-order
           [:property/pretty-name
            :skill/action-time-modifier-key
            :skill/action-time
            :skill/cooldown
            :skill/cost
            :skill/effects
            :creature/species
            :creature/level
            :entity/stats
            :entity/delete-after-duration
            :projectile/piercing?
            :entity/projectile-collision
            :maxrange
            :entity-effects])

(bind-root #'clojure.gdx/property-k-sort-order
           [:property/id
            :property/pretty-name
            :app/lwjgl3
            :entity/image
            :entity/animation
            :creature/species
            :creature/level
            :entity/body
            :item/slot
            :projectile/speed
            :projectile/max-range
            :projectile/piercing?
            :skill/action-time-modifier-key
            :skill/action-time
            :skill/start-action-sound
            :skill/cost
            :skill/cooldown])
