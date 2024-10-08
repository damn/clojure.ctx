(in-ns 'clojure.gdx)

(def-property-type :properties/skills
  {:schema [:entity/image
            :property/pretty-name
            :skill/action-time-modifier-key
            :skill/action-time
            :skill/start-action-sound
            :skill/effects
            [:skill/cooldown {:optional true}]
            [:skill/cost {:optional true}]]
   :overview {:title "Skills"
              :columns 16
              :image/scale 2}})

; TODO render text label free-skill-points
; (str "Free points: " (:entity/free-skill-points @player-entity))
#_(defn ->skill-window []
  (->window {:title "Skills"
             :id :skill-window
             :visible? false
             :cell-defaults {:pad 10}
             :rows [(for [id [:skills/projectile
                              :skills/meditation
                              :skills/spawn
                              :skills/melee-attack]
                          :let [; get-property in callbacks if they get changed, this is part of context permanently
                                button (->image-button ; TODO reuse actionbar button scale?
                                                       (:entity/image (build-property id)) ; TODO here anyway taken
                                                       ; => should probably build this window @ game start
                                                       (fn []
                                                         (effect! (player-clicked-skillmenu (build-property id)))))]]
                      (do
                       (add-tooltip! button #(->info-text (build-property id))) ; TODO no player modifiers applied (see actionbar)
                       button))]
             :pack? true}))

(defc :skill/action-time {:data :pos}
  (info-text [[_ v]]
    (str "[GOLD]Action-Time: " (readable-number v) " seconds[]")))

(defc :skill/cooldown {:data :nat-int}
  (info-text [[_ v]]
    (when-not (zero? v)
      (str "[SKY]Cooldown: " (readable-number v) " seconds[]"))))

(defc :skill/cost {:data :nat-int}
  (info-text [[_ v]]
    (when-not (zero? v)
      (str "[CYAN]Cost: " v " Mana[]"))))

(defc :skill/effects
  {:data [:components-ns :effect]})

(defc :skill/start-action-sound {:data :sound})

(defc :skill/action-time-modifier-key
  {:data [:enum [:stats/cast-speed :stats/attack-speed]]}
  (info-text [[_ v]]
    (str "[VIOLET]" (case v
                      :stats/cast-speed "Spell"
                      :stats/attack-speed "Attack") "[]")))

(defc :entity/skills
  {:data [:one-to-many :properties/skills]}
  (create [[k skills] eid]
    (cons [:e/assoc eid k nil]
          (for [skill skills]
            [:tx/add-skill eid skill])))

  (info-text [[_ skills]]
    ; => recursive info-text leads to endless text wall
    #_(when (seq skills)
        (str "[VIOLET]Skills: " (str/join "," (map name (keys skills))) "[]")))

  (tick [[k skills] eid]
    (for [{:keys [skill/cooling-down?] :as skill} (vals skills)
          :when (and cooling-down?
                     (stopped? cooling-down?))]
      [:e/assoc-in eid [k (:property/id skill) :skill/cooling-down?] false])))

(defn has-skill? [{:keys [entity/skills]} {:keys [property/id]}]
  (contains? skills id))

(defc :tx/add-skill
  (do! [[_ entity {:keys [property/id] :as skill}]]
    (assert (not (has-skill? @entity skill)))
    [[:e/assoc-in entity [:entity/skills id] skill]
     (when (:entity/player? @entity)
       [:tx.action-bar/add skill])]))

(defc :tx/remove-skill
  (do! [[_ entity {:keys [property/id] :as skill}]]
    (assert (has-skill? @entity skill))
    [[:e/dissoc-in entity [:entity/skills id]]
     (when (:entity/player? @entity)
       [:tx.action-bar/remove skill])]))

(defc :tx.entity.stats/pay-mana-cost
  (do! [[_ entity cost]]
    (let [mana-val ((entity-stat @entity :stats/mana) 0)]
      (assert (<= cost mana-val))
      [[:e/assoc-in entity [:entity/stats :stats/mana 0] (- mana-val cost)]])))

(comment
 (let [mana-val 4
       entity (atom (map->Entity {:entity/stats {:stats/mana [mana-val 10]}}))
       mana-cost 3
       resulting-mana (- mana-val mana-cost)]
   (= (do! [:tx.entity.stats/pay-mana-cost entity mana-cost] nil)
      [[:e/assoc-in entity [:entity/stats :stats/mana 0] resulting-mana]]))
 )
