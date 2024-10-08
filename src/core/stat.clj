(ns core.stat
  (:require [clojure.gdx :refer :all]
            [clojure.gdx.rand :refer [rand-int-between]]
            [clojure.string :as str]
            core.creature
            [core.projectile :refer [projectile-size]]))

(defn- defmodifier [k operations]
  (defc* k {:data [:map-optional operations]}))

(defn- stat-k->modifier-k [k]
  (keyword "modifier" (name k)))

(defn- stat-k->effect-k [k]
  (keyword "effect.entity" (name k)))

(defn effect-k->stat-k [effect-k]
  (keyword "stats" (name effect-k)))

(defn defstat [k {:keys [modifier-ops effect-ops] :as attr-m}]
  (defc* k attr-m)
  (when modifier-ops
    (defmodifier (stat-k->modifier-k k) modifier-ops))
  (when effect-ops
    (let [effect-k (stat-k->effect-k k)]
      (defc* effect-k {:data [:map-optional effect-ops]})
      (derive effect-k :base/stat-effect))))

; TODO needs to be there for each npc - make non-removable (added to all creatures)
; and no need at created player (npc controller component?)
(defstat :stats/aggro-range   {:data :nat-int})
(defstat :stats/reaction-time {:data :pos-int})

; TODO
; @ hp says here 'Minimum' hp instead of just 'HP'
; Sets to 0 but don't kills
; Could even set to a specific value ->
; op/set-to-ratio 0.5 ....
; sets the hp to 50%...
(defstat :stats/hp
  {:data :pos-int
   :modifier-ops [:op/max-inc :op/max-mult]
   :effect-ops [:op/val-inc :op/val-mult :op/max-inc :op/max-mult]})

(defstat :stats/mana
  {:data :nat-int
   :modifier-ops [:op/max-inc :op/max-mult]
   :effect-ops [:op/val-inc :op/val-mult :op/max-inc :op/max-mult]})

; * TODO clamp/post-process effective-values @ stat-k->effective-value
; * just don't create movement-speed increases too much?
; * dont remove strength <0 or floating point modifiers  (op/int-inc ?)
; * cast/attack speed dont decrease below 0 ??

; TODO clamp between 0 and max-speed ( same as movement-speed-schema )
(defstat :stats/movement-speed
  {:data :pos;(m/form entity/movement-speed-schema)
   :modifier-ops [:op/inc :op/mult]})

; TODO show the stat in different color red/green if it was permanently modified ?
; or an icon even on the creature
; also we want audiovisuals always ...
(defc :effect.entity/movement-speed
  {:data [:map [:op/mult]]})
(derive :effect.entity/movement-speed :base/stat-effect)

; TODO clamp into ->pos-int
(defstat :stats/strength
  {:data :nat-int
   :modifier-ops [:op/inc]})

; TODO here >0
(let [doc "action-time divided by this stat when a skill is being used.
          Default value 1.

          For example:
          attack/cast-speed 1.5 => (/ action-time 1.5) => 150% attackspeed."
      data :pos
      operations [:op/inc]]
  (defstat :stats/cast-speed
    {:data data
     :editor/doc doc
     :modifier-ops operations})

  (defstat :stats/attack-speed
    {:data data
     :editor/doc doc
     :modifier-ops operations}))

; TODO bounds
(defstat :stats/armor-save
  {:data :number
   :modifier-ops [:op/inc]})

(defstat :stats/armor-pierce
  {:data :number
   :modifier-ops [:op/inc]})

(extend-type clojure.gdx.Entity
  Stats
  (entity-stat [entity* stat-k]
    (when-let [base-value (stat-k (:entity/stats entity*))]
      (->modified-value entity* (stat-k->modifier-k stat-k) base-value))))

(def ^:private hpbar-colors
  {:green     [0 0.8 0]
   :darkgreen [0 0.5 0]
   :yellow    [0.5 0.5 0]
   :red       [0.5 0 0]})

(defn- hpbar-color [ratio]
  (let [ratio (float ratio)
        color (cond
                (> ratio 0.75) :green
                (> ratio 0.5)  :darkgreen
                (> ratio 0.25) :yellow
                :else          :red)]
    (color hpbar-colors)))

(def ^:private borders-px 1)

(let [stats-order [:stats/hp
                   :stats/mana
                   ;:stats/movement-speed
                   :stats/strength
                   :stats/cast-speed
                   :stats/attack-speed
                   :stats/armor-save
                   ;:stats/armor-pierce
                   ;:stats/aggro-range
                   ;:stats/reaction-time
                   ]]
  (defn- stats-info-texts [entity*]
    (str/join "\n"
              (for [stat-k stats-order
                    :let [value (entity-stat entity* stat-k)]
                    :when value]
                (str (k->pretty-name stat-k) ": " value)))))

; TODO mana optional? / armor-save / armor-pierce (anyway wrong here)
; cast/attack-speed optional ?
(defc :entity/stats
  {:data [:map [:stats/hp
                :stats/movement-speed
                :stats/aggro-range
                :stats/reaction-time
                [:stats/mana          {:optional true}]
                [:stats/strength      {:optional true}]
                [:stats/cast-speed    {:optional true}]
                [:stats/attack-speed  {:optional true}]
                [:stats/armor-save    {:optional true}]
                [:stats/armor-pierce  {:optional true}]]]
   :let stats}
  (->mk [_]
    (-> stats
        (update :stats/hp (fn [hp] (when hp [hp hp])))
        (update :stats/mana (fn [mana] (when mana [mana mana])))))

  (info-text [_]
    (stats-info-texts *info-text-entity*))

  (render-info [_ entity*]
    (when-let [hp (entity-stat entity* :stats/hp)]
      (let [ratio (val-max-ratio hp)
            {:keys [position width half-width half-height entity/mouseover?]} entity*
            [x y] position]
        (when (or (< ratio 1) mouseover?)
          (let [x (- x half-width)
                y (+ y half-height)
                height (pixels->world-units hpbar-height-px)
                border (pixels->world-units borders-px)]
            (draw-filled-rectangle x y width height :black)
            (draw-filled-rectangle (+ x border)
                                   (+ y border)
                                   (- (* width ratio) (* 2 border))
                                   (- height (* 2 border))
                                   (hpbar-color ratio))))))))

; TODO negate this value also @ use
; so can make positiive modifeirs green , negative red....
(defmodifier :modifier/damage-receive [:op/max-inc :op/max-mult])
(defmodifier :modifier/damage-deal [:op/val-inc :op/val-mult :op/max-inc :op/max-mult])

;;;;
;;;;
;;;; Helpers
;;;;
;;;;

(defn- nearest-enemy [entity*]
  (nearest-entity @(world-grid (entity-tile entity*))
                  (enemy-faction entity*)))


;;;;
;;;;
;;;;
;;;;
;;;;
;;;;
;;;; action/effect/skill
;;;;
;;;;
;;;;
;;;;
;;;;
;;;;

(defsystem applicable?
  "An effect will only be done (with do!) if this function returns truthy.
Required system for every effect, no default."
  [_])

(defsystem useful?
  "Used for NPC AI.
Called only if applicable? is truthy.
For example use for healing effect is only useful if hitpoints is < max.
Default method returns true."
  [_])
(defmethod useful? :default [_] true)

(defsystem ^:private render-effect
  "Renders effect during active-skill state while active till done?. Default do nothing."
  [_])
(defmethod render-effect :default [_])

;;
;; Aggregate functions
;;

(defn- filter-applicable? [effect]
  (filter applicable? effect))

(defn- effect-applicable? [effect]
  (seq (filter-applicable? effect)))

(defn- effect-useful? [effect]
  (->> effect
       filter-applicable?
       (some useful?)))

;;
;;
;;

; SCHEMA effect-ctx
; * source = always available
; # npc:
;   * target = maybe
;   * direction = maybe
; # player
;  * target = maybe
;  * target-position  = always available (mouse world position)
;  * direction  = always available (from mouse world position)

(declare ^:dynamic source
         ^:dynamic target
         ^:dynamic target-direction
         ^:dynamic target-position)

(defn- ->npc-effect-ctx [entity*]
  (let [target (nearest-enemy entity*)
        target (when (and target (line-of-sight? entity* @target))
                 target)]
    {:effect/source (:entity/id entity*)
     :effect/target target
     :effect/target-direction (when target (direction entity* @target))}))

(defn- ->player-effect-ctx [entity*]
  (let [target* (mouseover-entity*)
        target-position (or (and target* (:position target*))
                            (world-mouse-position))]
    {:effect/source (:entity/id entity*)
     :effect/target (:entity/id target*)
     :effect/target-position target-position
     :effect/target-direction (v-direction (:position entity*) target-position)}))

; this is not necessary if effect does not need target, but so far not other solution came up.
(defn- check-update-ctx
  "Call this on effect-context if the time of using the context is not the time when context was built."
  [{:keys [effect/source effect/target] :as ctx}]
  (if (and target
           (not (:entity/destroyed? @target))
           (line-of-sight? @source @target))
    ctx
    (dissoc ctx :effect/target)))

(defmacro with-effect-ctx [ctx & body]
  `(binding [source           (:effect/source           ~ctx)
             target           (:effect/target           ~ctx)
             target-direction (:effect/target-direction ~ctx)
             target-position  (:effect/target-position  ~ctx)]
     ~@body))

(defc :tx/effect
  (do! [[_ effect-ctx effect]]
    (with-effect-ctx effect-ctx
      (effect! (filter-applicable? effect)))))

; is called :base/stat-effect so it doesn't show up in (:data [:components-ns :effect.entity]) list in editor
; for :skill/effects
(defc :base/stat-effect
  (info-text [[k operations]]
    (str/join "\n"
              (for [operation operations]
                (str (op-info-text operation) " " (k->pretty-name k)))))

  (applicable? [[k _]]
    (and target
         (entity-stat @target (effect-k->stat-k k))))

  (useful? [_]
    true)

  (do! [[effect-k operations]]
    (let [stat-k (effect-k->stat-k effect-k)]
      (when-let [effective-value (entity-stat @target stat-k)]
        [[:e/assoc-in target [:entity/stats stat-k]
          ; TODO similar to components.entity.modifiers/->modified-value
          ; but operations not sort-by op/order ??
          ; op-apply reuse fn over operations to get effectiv value
          (reduce (fn [value operation] (op-apply operation value))
                  effective-value
                  operations)]]))))

(defn- entity*->melee-damage [entity*]
  (let [strength (or (entity-stat entity* :stats/strength) 0)]
    {:damage/min-max [strength strength]}))

(defn- damage-effect []
  [:effect.entity/damage (entity*->melee-damage @source)])

(defc :effect.entity/melee-damage
  {:data :some}
  (info-text [_]
    (str "Damage based on entity strength."
         (when source
           (str "\n" (info-text (damage-effect))))))

  (applicable? [_]
    (applicable? (damage-effect)))

  (do! [_]
    [(damage-effect)]))

(defn- effective-armor-save [source* target*]
  (max (- (or (entity-stat target* :stats/armor-save) 0)
          (or (entity-stat source* :stats/armor-pierce) 0))
       0))

(comment
 ; broken
 (let [source* {:entity/stats {:stats/armor-pierce 0.4}}
       target* {:entity/stats {:stats/armor-save   0.5}}]
   (effective-armor-save source* target*))
 )

(defn- armor-saves? [source* target*]
  (< (rand) (effective-armor-save source* target*)))

(defn- ->effective-damage [damage source*]
  (update damage :damage/min-max #(->modified-value source* :modifier/damage-deal %)))

(comment
 (let [->source (fn [mods] {:entity/modifiers mods})]
   (and
    (= (->effective-damage {:damage/min-max [5 10]}
                           (->source {:modifier/damage-deal {:op/val-inc [1 5 10]
                                                            :op/val-mult [0.2 0.3]
                                                            :op/max-mult [1]}}))
       {:damage/min-max [31 62]})

    (= (->effective-damage {:damage/min-max [5 10]}
                           (->source {:modifier/damage-deal {:op/val-inc [1]}}))
       {:damage/min-max [6 10]})

    (= (->effective-damage {:damage/min-max [5 10]}
                           (->source {:modifier/damage-deal {:op/max-mult [2]}}))
       {:damage/min-max [5 30]})

    (= (->effective-damage {:damage/min-max [5 10]}
                           (->source nil))
       {:damage/min-max [5 10]}))))

(defn- damage->text [{[min-dmg max-dmg] :damage/min-max}]
  (str min-dmg "-" max-dmg " damage"))

(defc :damage/min-max {:data :val-max})

(defc :effect.entity/damage
  {:let damage
   :data [:map [:damage/min-max]]}
  (info-text [_]
    (if source
      (let [modified (->effective-damage damage @source)]
        (if (= damage modified)
          (damage->text damage)
          (str (damage->text damage) "\nModified: " (damage->text modified))))
      (damage->text damage))) ; property menu no source,modifiers

  (applicable? [_]
    (and target
         (entity-stat @target :stats/hp)))

  (do! [_]
    (let [source* @source
          target* @target
          hp (entity-stat target* :stats/hp)]
      (cond
       (zero? (hp 0))
       []

       (armor-saves? source* target*)
       [[:tx/add-text-effect target "[WHITE]ARMOR"]] ; TODO !_!_!_!_!_!

       :else
       (let [;_ (println "Source unmodified damage:" damage)
             {:keys [damage/min-max]} (->effective-damage damage source*)
             ;_ (println "\nSource modified: min-max:" min-max)
             min-max (->modified-value target* :modifier/damage-receive min-max)
             ;_ (println "effective min-max: " min-max)
             dmg-amount (rand-int-between min-max)
             ;_ (println "dmg-amount: " dmg-amount)
             new-hp-val (max (- (hp 0) dmg-amount) 0)]
         [[:tx/audiovisual (:position target*) :audiovisuals/damage]
          [:tx/add-text-effect target (str "[RED]" dmg-amount)]
          [:e/assoc-in target [:entity/stats :stats/hp 0] new-hp-val]
          [:tx/event target (if (zero? new-hp-val) :kill :alert)]])))))

(defc :entity/temp-modifier
  {:let {:keys [counter modifiers]}}
  (info-text [_]
    (str "[LIGHT_GRAY]Spiderweb - remaining: " (readable-number (finished-ratio counter)) "/1[]"))

  (tick [[k _] eid]
    (when (stopped? counter)
      [[:e/dissoc eid k]
       [:tx/reverse-modifiers eid modifiers]]))

  (render-above [_ entity*]
    (draw-filled-circle (:position entity*) 0.5 [0.5 0.5 0.5 0.4])))

(let [modifiers {:modifier/movement-speed {:op/mult -0.5}}
      duration 5]
  (defc :effect.entity/spiderweb
    {:data :some}
    (info-text [_]
      "Spiderweb slows 50% for 5 seconds."
      ; modifiers same like item/modifiers has info-text
      ; counter ?
      )

    (applicable? [_]
      ; ?
      true)

    ; TODO stacking? (if already has k ?) or reset counter ? (see string-effect too)
    (do! [_]
      (when-not (:entity/temp-modifier @target)
        [[:tx/apply-modifiers target modifiers]
         [:e/assoc target :entity/temp-modifier {:modifiers modifiers
                                                 :counter (->counter duration)}]]))))
(defc :effect.entity/convert
  {:data :some}
  (info-text [_]
    "Converts target to your side.")

  (applicable? [_]
    (and target
         (= (:entity/faction @target)
            (enemy-faction @source))))

  (do! [_]
    [[:tx/audiovisual (:position @target) :audiovisuals/convert]
     [:e/assoc target :entity/faction (friendly-faction @source)]]))

; TODO https://github.com/damn/core/issues/29
; spawning on player both without error ?! => not valid position checked
; also what if someone moves on the target posi ? find nearby valid cell ?
; BLOCKING PLAYER MOVEMENT ! (summons no-clip with player ?)
; check not blocked position // line of sight. (part of target-position make)
; limit max. spawns
; animation/sound
; proper icon (grayscaled ?)
; keep in player movement range priority ( follow player if too far, otherwise going for enemies)
; => so they follow you around
; not try-spawn, but check valid-params & then spawn !
; new UI -> show creature body & then place
; >> but what if it is blocked the area during action-time ?? <<
(defc :effect/spawn
  {:data [:one-to-one :properties/creatures]
   :let {:keys [property/id]}}
  (applicable? [_]
    (and (:entity/faction @source)
         target-position))

  (do! [_]
    [[:tx/sound "sounds/bfxr_shield_consume.wav"]
     [:tx/creature {:position target-position
                    :creature-id id ; already properties/get called through one-to-one, now called again.
                    :components {:entity/state [:state/npc :npc-idle]
                                 :entity/faction (:entity/faction @source)}}]]))

(defc :effect.entity/stun
  {:data :pos
   :let duration}
  (info-text [_]
    (str "Stuns for " (readable-number duration) " seconds"))

  (applicable? [_]
    (and target
         (:entity/state @target)))

  (do! [_]
    [[:tx/event target :stun duration]]))

(defc :effect.entity/kill
  {:data :some}
  (info-text [_]
    "Kills target")

  (applicable? [_]
    (and target
         (:entity/state @target)))

  (do! [_]
    [[:tx/event target :kill]]))

(defn- projectile-start-point [entity* direction size]
  (v-add (:position entity*)
         (v-scale direction
                  (+ (:radius entity*) size 0.1))))

; TODO effect/text ... shouldn't have source/target dmg stuff ....
; as it is just sent .....
; or we adjust the effect when we send it ....

(defc :effect/projectile
  {:data [:one-to-one :properties/projectiles]
   :let {:keys [entity-effects projectile/max-range] :as projectile}}
  ; TODO for npcs need target -- anyway only with direction
  (applicable? [_]
    target-direction) ; faction @ source also ?

  ; TODO valid params direction has to be  non-nil (entities not los player ) ?
  (useful? [_]
    (let [source-p (:position @source)
          target-p (:position @target)]
      (and (not (path-blocked? ; TODO test
                               source-p
                               target-p
                               (projectile-size projectile)))
           ; TODO not taking into account body sizes
           (< (v-distance source-p ; entity/distance function protocol EntityPosition
                          target-p)
              max-range))))

  (do! [_]
    [[:tx/sound "sounds/bfxr_waypointunlock.wav"]
     [:tx/projectile
      {:position (projectile-start-point @source target-direction (projectile-size projectile))
       :direction target-direction
       :faction (:entity/faction @source)}
      projectile]]))

(comment
 ; mass shooting
 (for [direction (map math.vector/normalise
                      [[1 0]
                       [1 1]
                       [1 -1]
                       [0 1]
                       [0 -1]
                       [-1 -1]
                       [-1 1]
                       [-1 0]])]
   [:tx/projectile projectile-id ...]
   )
 )

(defc :entity-effects {:data [:components-ns :effect.entity]})

; TODO applicable targets? e.g. projectiles/effect s/???item entiteis ??? check
; same code as in render entities on world view screens/world
(defn- creatures-in-los-of-player []
  (->> (active-entities)
       (filter #(:creature/species @%))
       (filter #(line-of-sight? @world-player @%))
       (remove #(:entity/player? @%))))

; TODO targets projectiles with -50% hp !!

(comment
 ; TODO showing one a bit further up
 ; maybe world view port is cut
 ; not quite showing correctly.
 (let [targets (creatures-in-los-of-player)]
   (count targets)
   #_(sort-by #(% 1) (map #(vector (:entity.creature/name @%)
                                   (:position @%)) targets)))

 )

(defc :effect/target-all
  {:data [:map [:entity-effects]]
   :let {:keys [entity-effects]}}
  (info-text [_]
    "[LIGHT_GRAY]All visible targets[]")

  (applicable? [_]
    true)

  (useful? [_]
    ; TODO
    false
    )

  (do! [_]
    (let [source* @source]
      (apply concat
             (for [target (creatures-in-los-of-player)]
               [[:tx/line-render {:start (:position source*) #_(start-point source* target*)
                                  :end (:position @target)
                                  :duration 0.05
                                  :color [1 0 0 0.75]
                                  :thick? true}]
                ; some sound .... or repeat smae sound???
                ; skill do sound  / skill start sound >?
                ; problem : nested tx/effect , we are still having direction/target-position
                ; at sub-effects
                ; and no more safe - merge
                ; find a way to pass ctx / effect-ctx separate ?
                [:tx/effect {:effect/source source :effect/target target} entity-effects]]))))

  (render-effect [_]
    (let [source* @source]
      (doseq [target* (map deref (creatures-in-los-of-player))]
        (draw-line (:position source*) #_(start-point source* target*)
                   (:position target*)
                   [1 0 0 0.5])))))

(defn- in-range? [entity* target* maxrange] ; == circle-collides?
  (< (- (float (v-distance (:position entity*)
                           (:position target*)))
        (float (:radius entity*))
        (float (:radius target*)))
     (float maxrange)))

; TODO use at projectile & also adjust rotation
(defn- start-point [entity* target*]
  (v-add (:position entity*)
         (v-scale (direction entity* target*)
                  (:radius entity*))))

(defn- end-point [entity* target* maxrange]
  (v-add (start-point entity* target*)
         (v-scale (direction entity* target*)
                  maxrange)))

(defc :maxrange {:data :pos}
  (info-text [[_ maxrange]]
    (str "[LIGHT_GRAY]Range " maxrange " meters[]")))

(defc :effect/target-entity
  {:let {:keys [maxrange entity-effects]}
   :data [:map [:entity-effects :maxrange]]
   :editor/doc "Applies entity-effects to a target if they are inside max-range & in line of sight.
               Cancels if line of sight is lost. Draws a red/yellow line wheter the target is inside the max range. If the effect is to be done and target out of range -> draws a hit-ground-effect on the max location."}

  (applicable? [_]
    (and target
         (effect-applicable? entity-effects)))

  (useful? [_]
    (assert source)
    (assert target)
    (in-range? @source @target maxrange))

  (do! [_]
    (let [source* @source
          target* @target]
      (if (in-range? source* target* maxrange)
        (cons
         [:tx/line-render {:start (start-point source* target*)
                           :end (:position target*)
                           :duration 0.05
                           :color [1 0 0 0.75]
                           :thick? true}]
         ; TODO => make new context with end-point ... and check on point entity
         ; friendly fire ?!
         ; player maybe just direction possible ?!

         ; TODO FIXME
         ; have to use tx/effect now ?!
         ; still same context ...
         ; filter applicable ?! - omg
         entity-effects

         )
        [; TODO
         ; * clicking on far away monster
         ; * hitting ground in front of you ( there is another monster )
         ; * -> it doesn't get hit ! hmmm
         ; * either use 'MISS' or get enemy entities at end-point
         [:tx/audiovisual (end-point source* target* maxrange) :audiovisuals/hit-ground]])))

  (render-effect [_]
    (when target
      (let [source* @source
            target* @target]
        (draw-line (start-point source* target*)
                   (end-point source* target* maxrange)
                   (if (in-range? source* target* maxrange)
                     [1 0 0 0.5]
                     [1 1 0 0.5]))))))

(defn- mana-value [entity*]
  (if-let [mana (entity-stat entity* :stats/mana)]
    (mana 0)
    0))

(defn- not-enough-mana? [entity* {:keys [skill/cost]}]
  (> cost (mana-value entity*)))

(defn- skill-usable-state
  [entity* {:keys [skill/cooling-down? skill/effects] :as skill}]
  (cond
   cooling-down?
   :cooldown

   (not-enough-mana? entity* skill)
   :not-enough-mana

   (not (effect-applicable? effects))
   :invalid-params

   :else
   :usable))

(defn- draw-skill-icon [icon entity* [x y] action-counter-ratio]
  (let [[width height] (:world-unit-dimensions icon)
        _ (assert (= width height))
        radius (/ (float width) 2)
        y (+ (float y) (float (:half-height entity*)) (float 0.15))
        center [x (+ y radius)]]
    (draw-filled-circle center radius [1 1 1 0.125])
    (draw-sector center radius
                 90 ; start-angle
                 (* (float action-counter-ratio) 360) ; degree
                 [1 1 1 0.5])
    (draw-image icon [(- (float x) radius) y])))

(defn- apply-action-speed-modifier [entity* skill action-time]
  (/ action-time
     (or (entity-stat entity* (:skill/action-time-modifier-key skill))
         1)))

(defc :active-skill
  {:let {:keys [eid skill effect-ctx counter]}}
  (->mk [[_ eid [skill effect-ctx]]]
    {:eid eid
     :skill skill
     :effect-ctx effect-ctx
     :counter (->> skill
                   :skill/action-time
                   (apply-action-speed-modifier @eid skill)
                   (->counter))})

  (player-enter [_]
    [[:tx/cursor :cursors/sandclock]])

  (pause-game? [_]
    false)

  (enter [_]
    [[:tx/sound (:skill/start-action-sound skill)]
     (when (:skill/cooldown skill)
       [:e/assoc-in eid [:entity/skills (:property/id skill) :skill/cooling-down?] (->counter (:skill/cooldown skill))])
     (when-not (zero? (:skill/cost skill))
       [:tx.entity.stats/pay-mana-cost eid (:skill/cost skill)])])

  (tick [_ eid]
    (cond
     (with-effect-ctx (check-update-ctx effect-ctx)
       (not (effect-applicable? (:skill/effects skill))))
     [[:tx/event eid :action-done]
      ; TODO some sound ?
      ]

     (stopped? counter)
     [[:tx/event eid :action-done]
      [:tx/effect effect-ctx (:skill/effects skill)]]))

  (render-info [_ entity*]
    (let [{:keys [entity/image skill/effects]} skill]
      (draw-skill-icon image entity* (:position entity*) (finished-ratio counter))
      (with-effect-ctx (check-update-ctx effect-ctx)
        (run! render-effect effects)))))

(defn- npc-choose-skill [entity*]
  (->> entity*
       :entity/skills
       vals
       (sort-by #(or (:skill/cost %) 0))
       reverse
       (filter #(and (= :usable (skill-usable-state entity* %))
                     (effect-useful? (:skill/effects %))))
       first))

(comment
 (let [entity* @(get-entity 76)
       effect-ctx (->npc-effect-ctx entity*)]
   (npc-choose-skill effect-ctx entity*))
 )

(defc :npc-idle
  {:let {:keys [eid]}}
  (->mk [[_ eid]]
    {:eid eid})

  (tick [_ eid]
    (let [entity* @eid
          effect-ctx (->npc-effect-ctx entity*)]
      (if-let [skill (with-effect-ctx effect-ctx
                       (npc-choose-skill entity*))]
        [[:tx/event eid :start-action [skill effect-ctx]]]
        [[:tx/event eid :movement-direction (potential-fields-follow-to-enemy eid)]]))))

(defn- selected-skill []
  (let [button-group (:action-bar world-widgets)]
    (when-let [skill-button (bg-checked button-group)]
      (actor-id skill-button))))

(defn- inventory-window []
  (get (:windows (stage-get)) :inventory-window))

(defn- denied [text]
  [[:tx/sound "sounds/bfxr_denied.wav"]
   [:tx/msg-to-player text]])

(defmulti ^:private on-clicked
  (fn [entity*]
    (:type (:entity/clickable entity*))))

(defmethod on-clicked :clickable/item [clicked-entity*]
  (let [player-entity* @world-player
        item (:entity/item clicked-entity*)
        clicked-entity (:entity/id clicked-entity*)]
    (cond
     (visible? (inventory-window))
     [[:tx/sound "sounds/bfxr_takeit.wav"]
      [:e/destroy clicked-entity]
      [:tx/event (:entity/id player-entity*) :pickup-item item]]

     (can-pickup-item? player-entity* item)
     [[:tx/sound "sounds/bfxr_pickup.wav"]
      [:e/destroy clicked-entity]
      [:tx/pickup-item (:entity/id player-entity*) item]]

     :else
     [[:tx/sound "sounds/bfxr_denied.wav"]
      [:tx/msg-to-player "Your Inventory is full"]])))

(defmethod on-clicked :clickable/player
  [_clicked-entity*]
  (toggle-visible! (inventory-window))) ; TODO no tx

(defn- clickable->cursor [mouseover-e* too-far-away?]
  (case (:type (:entity/clickable mouseover-e*))
    :clickable/item (if too-far-away?
                      :cursors/hand-before-grab-gray
                      :cursors/hand-before-grab)
    :clickable/player :cursors/bag))

(defn- ->clickable-mouseover-entity-interaction [player-e* mouseover-e*]
  (if (< (v-distance (:position player-e*) (:position mouseover-e*))
         (:entity/click-distance-tiles player-e*))
    [(clickable->cursor mouseover-e* false) (fn [] (on-clicked mouseover-e*))]
    [(clickable->cursor mouseover-e* true)  (fn [] (denied "Too far away"))]))

(defn- inventory-cell-with-item? [actor]
  (and (parent actor)
       (= "inventory-cell" (actor-name (parent actor)))
       (get-in (:entity/inventory @world-player)
               (actor-id (parent actor)))))

(defn- mouseover-actor->cursor []
  (let [actor (mouse-on-actor?)]
    (cond
     (inventory-cell-with-item? actor) :cursors/hand-before-grab
     (window-title-bar? actor) :cursors/move-window
     (button? actor) :cursors/over-button
     :else :cursors/default)))

(defn- ->interaction-state [entity*]
  (let [mouseover-e* (mouseover-entity*)]
    (cond
     (mouse-on-actor?)
     [(mouseover-actor->cursor) (fn [] nil)] ; handled by actors themself, they check player state

     (and mouseover-e* (:entity/clickable mouseover-e*))
     (->clickable-mouseover-entity-interaction entity* mouseover-e*)

     :else
     (if-let [skill-id (selected-skill)]
       (let [skill (skill-id (:entity/skills entity*))
             effect-ctx (->player-effect-ctx entity*)
             state (with-effect-ctx effect-ctx
                     (skill-usable-state entity* skill))]
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

(defc :player-idle
  {:let {:keys [eid]}}
  (->mk [[_ eid]]
    {:eid eid})

  (pause-game? [_]
    true)

  (manual-tick [_]
    (if-let [movement-vector (core.creature/WASD-movement-vector)]
      [[:tx/event eid :movement-input movement-vector]]
      (let [[cursor on-click] (->interaction-state @eid)]
        (cons [:tx/cursor cursor]
              (when (button-just-pressed? :left)
                (on-click))))))

  (clicked-inventory-cell [_ cell]
    ; TODO no else case
    (when-let [item (get-in (:entity/inventory @eid) cell)]
      [[:tx/sound "sounds/bfxr_takeit.wav"]
       [:tx/event eid :pickup-item item]
       [:tx/remove-item eid cell]]))

  (clicked-skillmenu-skill [_ skill]
    (let [free-skill-points (:entity/free-skill-points @eid)]
      ; TODO no else case, no visible free-skill-points
      (when (and (pos? free-skill-points)
                 (not (has-skill? @eid skill)))
        [[:e/assoc eid :entity/free-skill-points (dec free-skill-points)]
         [:tx/add-skill eid skill]]))))
