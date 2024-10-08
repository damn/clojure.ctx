(ns clojure.gdx
  {:metadoc/categories {:app "🖥️ Application"
                        :camera "🎥 Camera"
                        :component "⚙️ Component"
                        :effect "💥 Effects"
                        :entity "👾 Entity"
                        :geometry "📐 Geometry"
                        :graphics "🎨 Graphics"
                        :gui-view  "🖼️ Gui View"
                        :input "🎮 Input"
                        :properties "📦️ Properties"
                        :screen "📺 Screens"
                        :sprite "🖼️ Image"
                        :ui "🎛️ UI"
                        :ui.actor "🕴️ UI Actor"
                        :utils  "🔧 Utils"
                        :world "🌎 World"
                        :world-view  "🗺️ World View"
                        :world.timer "⏳ Timer"}}
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.math :as math]
            [clojure.pprint :refer [pprint]]
            [clojure.gdx.tiled :as t]
            [clj-commons.pretty.repl :refer [pretty-pst]]
            [data.grid2d :as g]
            [malli.core :as m]
            [malli.error :as me]
            [malli.generator :as mg])
  (:import (com.badlogic.gdx Gdx ApplicationAdapter)
           (com.badlogic.gdx.assets AssetManager)
           (com.badlogic.gdx.audio Sound)
           (com.badlogic.gdx.backends.lwjgl3 Lwjgl3Application Lwjgl3ApplicationConfiguration)
           (com.badlogic.gdx.files FileHandle)
           (com.badlogic.gdx.graphics Color Colors Texture Texture$TextureFilter OrthographicCamera Camera Pixmap Pixmap$Format)
           (com.badlogic.gdx.graphics.g2d SpriteBatch Batch BitmapFont TextureRegion)
           (com.badlogic.gdx.graphics.g2d.freetype FreeTypeFontGenerator FreeTypeFontGenerator$FreeTypeFontParameter)
           (com.badlogic.gdx.math MathUtils Vector2 Vector3 Circle Rectangle Intersector)
           (com.badlogic.gdx.utils Align Scaling Disposable SharedLibraryLoader ScreenUtils)
           (com.badlogic.gdx.utils.viewport Viewport FitViewport)
           (com.badlogic.gdx.scenes.scene2d Actor Touchable Group Stage)
           (com.badlogic.gdx.scenes.scene2d.ui Widget Image Label Button Table Cell WidgetGroup Stack ButtonGroup HorizontalGroup VerticalGroup Window Tree$Node)
           (com.badlogic.gdx.scenes.scene2d.utils ClickListener ChangeListener TextureRegionDrawable Drawable)
           (com.kotcrab.vis.ui VisUI VisUI$SkinScale)
           (com.kotcrab.vis.ui.widget Tooltip VisTextButton VisCheckBox VisSelectBox VisImage VisImageButton VisTextField VisWindow VisTable VisLabel VisSplitPane VisScrollPane Separator VisTree)
           (space.earlygrey.shapedrawer ShapeDrawer)
           (gdl RayCaster)))

(def defsystems "Map of all systems as key of name-string to var." {})

(defmacro defsystem
  "A system is a multimethod which takes a component `[k v]` and dispatches on k."
  [sys-name docstring params]
  (when (zero? (count params))
    (throw (IllegalArgumentException. "First argument needs to be component.")))
  (when-let [avar (resolve sys-name)]
    (println "WARNING: Overwriting defsystem:" avar))
  `(do
    (defmulti ~(vary-meta sys-name assoc :params (list 'quote params))
      ~(str "[[defsystem]] with params: `" params "` \n\n " docstring)
      (fn ~(symbol (str (name sys-name))) [[k# _#] & args#] k#))
    (alter-var-root #'defsystems assoc ~(str (ns-name *ns*) "/" sys-name) (var ~sys-name))
    (var ~sys-name)))

(def ^:private warn-name-ns-mismatch? false)

(defn- k->component-ns [k] ;
  (symbol (str "components." (name (namespace k)) "." (name k))))

(defn- check-warn-ns-name-mismatch [k]
  (when (and warn-name-ns-mismatch?
             (namespace k)
             (not= (k->component-ns k) (ns-name *ns*)))
    (println "WARNING: defc " k " is not matching with namespace name " (ns-name *ns*))))

(def component-attributes {})

(defn defc*
  "Defines a component without systems methods, so only to set metadata."
  [k attr-map]
  (when (get component-attributes k)
    (println "WARNING: Overwriting defc" k "attr-map"))
  (alter-var-root #'component-attributes assoc k attr-map))

(defmacro defc
  "Defines a component with keyword k and optional metadata attribute-map followed by system implementations (via defmethods).

attr-map may contain `:let` binding which is let over the value part of a component `[k value]`.

Example:
```clojure
(defsystem foo \"foo docstring.\" [_])

(defc :foo/bar
  {:let {:keys [a b]}}
  (foo [_]
    (+ a b)))

(foo [:foo/bar {:a 1 :b 2}])
=> 3
```"
  [k & sys-impls]
  (check-warn-ns-name-mismatch k)
  (let [attr-map? (not (list? (first sys-impls)))
        attr-map  (if attr-map? (first sys-impls) {})
        sys-impls (if attr-map? (rest sys-impls) sys-impls)
        let-bindings (:let attr-map)
        attr-map (dissoc attr-map :let)]
    `(do
      (when ~attr-map?
        (defc* ~k ~attr-map))
      #_(alter-meta! *ns* #(update % :doc str "\n* defc `" ~k "`"))
      ~@(for [[sys & fn-body] sys-impls
              :let [sys-var (resolve sys)
                    sys-params (:params (meta sys-var))
                    fn-params (first fn-body)
                    fn-exprs (rest fn-body)]]
          (do
           (when-not sys-var
             (throw (IllegalArgumentException. (str sys " does not exist."))))
           (when-not (= (count sys-params) (count fn-params)) ; defmethods do not check this, that's why we check it here.
             (throw (IllegalArgumentException.
                     (str sys-var " requires " (count sys-params) " args: " sys-params "."
                          " Given " (count fn-params)  " args: " fn-params))))
           `(do
             (assert (keyword? ~k) (pr-str ~k))
             (alter-var-root #'component-attributes assoc-in [~k :params ~(name (symbol sys-var))] (quote ~fn-params))
             (when (get (methods @~sys-var) ~k)
               (println "WARNING: Overwriting defc" ~k "on" ~sys-var))
             (defmethod ~sys ~k ~(symbol (str (name (symbol sys-var)) "." (name k)))
               [& params#]
               (let [~(if let-bindings let-bindings '_) (get (first params#) 1) ; get because maybe component is just [:foo] without v.
                     ~fn-params params#]
                 ~@fn-exprs)))))
      ~k)))

;;;;

(def ^:private ^:dbg-flag debug-print-txs? false)

(defn- debug-print-tx [tx]
  (pr-str (mapv #(cond
                  (instance? clojure.lang.Atom %) (str "<entity-atom{uid=" (:entity/uid @%) "}>")
                  :else %)
                tx)))

#_(defn- tx-happened! [tx]
    (when (and
           (not (fn? tx))
           (not= :tx/cursor (first tx)))
      (when debug-print-txs?
        (println logic-frame "." (debug-print-tx tx)))))

(defsystem do!
  "Return nil or new coll/seq of txs to be done recursively."
  [_])

(defn effect!
  "An effect is defined as a sequence of txs(transactions).

A tx is either a (fn []) with no args or a component which implements the do! system.

All txs are being executed in sequence, any nil are skipped.

If the result of a tx is non-nil, we assume a new sequence of txs and effect! calls itself recursively.

On any exception we get a stacktrace with all tx's values and names shown."
  [effect]
  (doseq [tx effect
          :when tx]
    (try (when-let [result (if (fn? tx)
                             (tx)
                             (do! tx))]
           (effect! result))
         (catch Throwable t
           (throw (ex-info "Error with transaction"
                           {:tx tx #_(debug-print-tx tx)}
                           t))))))

;;;;

(def ^:private val-max-schema
  (m/schema [:and
             [:vector {:min 2 :max 2} [:int {:min 0}]]
             [:fn {:error/fn (fn [{[^int v ^int mx] :value} _]
                               (when (< mx v)
                                 (format "Expected max (%d) to be smaller than val (%d)" v mx)))}
              (fn [[^int a ^int b]] (<= a b))]]))

(defn val-max-ratio
  "If mx and v is 0, returns 0, otherwise (/ v mx)"
  [[^int v ^int mx]]
  {:pre [(m/validate val-max-schema [v mx])]}
  (if (and (zero? v) (zero? mx))
    0
    (/ v mx)))

#_(defn lower-than-max? [[^int v ^int mx]]
  {:pre [(m/validate val-max-schema [v mx])]}
  (< v mx))

#_(defn set-to-max [[v mx]]
  {:pre [(m/validate val-max-schema [v mx])]}
  [mx mx])

;;;;

(defn- +? [n]
  (case (math/signum n)
    0.0 ""
    1.0 "+"
    -1.0 ""))

(defsystem op-value-text "FIXME" [_])

(defn op-info-text [{value 1 :as operation}]
  (str (+? value) (op-value-text operation)))

(defsystem op-apply "FIXME" [_ base-value])
(defsystem op-order "FIXME" [_])

(defc :op/inc
  {:data :number
   :let value}
  (op-value-text [_] (str value))
  (op-apply [_ base-value] (+ base-value value))
  (op-order [_] 0))

(defc :op/mult
  {:data :number
   :let value}
  (op-value-text [_] (str (int (* 100 value)) "%"))
  (op-apply [_ base-value] (* base-value (inc value)))
  (op-order [_] 1))

(defn- ->pos-int [v]
  (-> v int (max 0)))

(defn- val-max-op-k->parts [op-k]
  (let [[val-or-max inc-or-mult] (mapv keyword (str/split (name op-k) #"-"))]
    [val-or-max (keyword "op" (name inc-or-mult))]))

(comment
 (= (val-max-op-k->parts :op/val-inc) [:val :op/inc])
 )

(defc :op/val-max
  (op-value-text [[op-k value]]
    (let [[val-or-max op-k] (val-max-op-k->parts op-k)]
      (str (op-value-text [op-k value]) " " (case val-or-max
                                              :val "Minimum"
                                              :max "Maximum"))))


  (op-apply [[operation-k value] val-max]
    (assert (m/validate val-max-schema val-max) (pr-str val-max))
    (let [[val-or-max op-k] (val-max-op-k->parts operation-k)
          f #(op-apply [op-k value] %)
          [v mx] (update val-max (case val-or-max :val 0 :max 1) f)
          v  (->pos-int v)
          mx (->pos-int mx)
          vmx (case val-or-max
                :val [v (max v mx)]
                :max [(min v mx) mx])]
      (assert (m/validate val-max-schema vmx))
      vmx))

  (op-order [[op-k value]]
    (let [[_ op-k] (val-max-op-k->parts op-k)]
      (op-order [op-k value]))))

(defc :op/val-inc {:data :int})
(derive       :op/val-inc :op/val-max)

(defc :op/val-mult {:data :number})
(derive       :op/val-mult :op/val-max)

(defc :op/max-inc {:data :int})
(derive       :op/max-inc :op/val-max)

(defc :op/max-mult {:data :number})
(derive       :op/max-mult :op/val-max)

(comment
 (and
  (= (op-apply [:op/val-inc 30]    [5 10]) [35 35])
  (= (op-apply [:op/max-mult -0.5] [5 10]) [5 5])
  (= (op-apply [:op/val-mult 2]    [5 10]) [15 15])
  (= (op-apply [:op/val-mult 1.3]  [5 10]) [11 11])
  (= (op-apply [:op/max-mult -0.8] [5 10]) [1 1])
  (= (op-apply [:op/max-mult -0.9] [5 10]) [0 0]))
 )

;;;;

(defsystem ^:private ->value "..." [_])

(defn def-attributes [& attributes-data]
  {:pre [(even? (count attributes-data))]}
  (doseq [[k data] (partition 2 attributes-data)]
    (defc* k {:data data})))

(defn def-property-type [k {:keys [schema overview]}]
  (defc k
    {:data [:map (conj schema :property/id)]
     :overview overview}))

(defn safe-get [m k]
  (let [result (get m k ::not-found)]
    (if (= result ::not-found)
      (throw (IllegalArgumentException. (str "Cannot find " (pr-str k))))
      result)))

(defn- data-component [k]
  (try (let [data (:data (safe-get component-attributes k))]
         (if (vector? data)
           [(first data) (->value data)]
           [data (safe-get component-attributes data)]))
       (catch Throwable t
         (throw (ex-info "" {:k k} t)))))

;;;;

(comment

 (defn- raw-properties-of-type [ptype]
   (->> (vals properties-db)
        (filter #(= ptype (->type %)))))

 (defn- migrate [ptype prop-fn]
   (let [cnt (atom 0)]
     (time
      (doseq [prop (map prop-fn (raw-properties-of-type ptype))]
        (println (swap! cnt inc) (:property/id prop) ", " (:property/pretty-name prop))
        (update! prop)))
     ; omg every update! calls async-write-to-file ...
     ; actually if its async why does it take so long ?
     (async-write-to-file! @app-state)
     nil))

 (migrate :properties/creatures
          (fn [prop]
            (-> prop
                (assoc :entity/tags ""))))

 )

(defn- property-type->id-namespace [property-type]
  (keyword (name property-type)))

(defn- attribute-schema
  "Can define keys as just keywords or with properties like [:foo {:optional true}]."
  [ks]
  (for [k ks
        :let [k? (keyword? k)
              properties (if k? nil (k 1))
              k (if k? k (k 0))]]
    (do
     (assert (keyword? k))
     (assert (or (nil? properties) (map? properties)) (pr-str ks))
     [k properties (:schema ((data-component k) 1))])))

(defn- map-schema [ks]
  (apply vector :map {:closed true} (attribute-schema ks)))

(defn- namespaced-ks [ns-name-k]
  (filter #(= (name ns-name-k) (namespace %))
          (keys component-attributes)))

;;;; Component Data Schemas

(defc :some    {:schema :some})
(defc :boolean {:schema :boolean})
(defc :string  {:schema :string})
(defc :number  {:schema number?})
(defc :nat-int {:schema nat-int?})
(defc :int     {:schema int?})
(defc :pos     {:schema pos?})
(defc :pos-int {:schema pos-int?})
(defc :sound   {:schema :string})
(defc :val-max {:schema (m/form val-max-schema)})
(defc :image   {:schema [:map {:closed true}
                         [:file :string]
                         [:sub-image-bounds {:optional true} [:vector {:size 4} nat-int?]]]})
(defc :data/animation {:schema [:map {:closed true}
                                [:frames :some]
                                [:frame-duration pos?]
                                [:looping? :boolean]]})

(defc :enum
  (->value [[_ items]]
    {:schema (apply vector :enum items)}))

(defc :qualified-keyword
  (->value [schema]
    {:schema schema}))

(defc :map
  (->value [[_ ks]]
    {:schema (map-schema ks)}))

(defc :map-optional
  (->value [[_ ks]]
    {:schema (map-schema (map (fn [k] [k {:optional true}]) ks))}))

(defc :components-ns
  (->value [[_ ns-name-k]]
    (->value [:map-optional (namespaced-ks ns-name-k)])))

(defc :one-to-many
  (->value [[_ property-type]]
    {:schema [:set [:qualified-keyword {:namespace (property-type->id-namespace property-type)}]]}))

(defc :one-to-one
  (->value [[_ property-type]]
    {:schema [:qualified-keyword {:namespace (property-type->id-namespace property-type)}]}))

;;;;

(defmulti ^:private edn->value (fn [data v] (if data (data 0))))
(defmethod edn->value :default [_data v]
  v)

(defn- ns-k->property-type [ns-k]
  (keyword "properties" (name ns-k)))

(defn- ->type [{:keys [property/id]}]
  (keyword "properties" (namespace id)))

(defn prop->image [{:keys [entity/image entity/animation]}]
  (or image
      (first (:frames animation))))

(defn- types []
  (filter #(= "properties" (namespace %)) (keys component-attributes)))

(defn- overview [property-type]
  (:overview (get component-attributes property-type)))

(defn- ->schema [property]
  (-> property
      ->type
      data-component
      (get 1)
      :schema
      m/schema))

(defn- validate [property]
  (let [schema (->schema property)
        valid? (try (m/validate schema property)
                    (catch Throwable t
                      (throw (ex-info "m/validate fail" {:property property} t))))]
    (when-not valid?
      (throw (ex-info (str (me/humanize (m/explain schema property)))
                      {:property property
                       :schema (m/form schema)})))))

(defc :property/id {:data [:qualified-keyword]})

(defn bind-root [avar value] (alter-var-root avar (constantly value)))

(declare ^:private properties-db
         ^:private properties-edn-file)

(defn- load-properties-db! [file]
  (let [properties (-> file slurp edn/read-string)]
    (assert (apply distinct? (map :property/id properties)))
    (run! validate properties)
    (bind-root #'properties-db (zipmap (map :property/id properties) properties))
    (bind-root #'properties-edn-file properties-edn-file)))

(defn- async-pprint-spit! [properties]
  (.start
   (Thread.
    (fn []
      (binding [*print-level* nil]
        (->> properties
             pprint
             with-out-str
             (spit properties-edn-file)))))))

(defn- recur-sort-map [m]
  (into (sorted-map)
        (zipmap (keys m)
                (map #(if (map? %)
                        (recur-sort-map %)
                        %)
                     (vals m)))))

(defn- async-write-to-file! []
  (->> properties-db
       vals
       (sort-by ->type)
       (map recur-sort-map)
       doall
       async-pprint-spit!))

(def ^:private undefined-data-ks (atom #{}))

(comment
 #{:frames
   :looping?
   :frame-duration
   :file
   :sub-image-bounds})

; reduce-kv?
(defn- apply-kvs
  "Calls for every key in map (f k v) to calculate new value at k."
  [m f]
  (reduce (fn [m k]
            (assoc m k (f k (get m k)))) ; using assoc because non-destructive for records
          m
          (keys m)))

(defn- build [property]
  (apply-kvs property
             (fn [k v]
               (edn->value (try (data-component k)
                                (catch Throwable _t
                                  (swap! undefined-data-ks conj k)))
                           (if (map? v)
                             (build v)
                             v)))))

(defn build-property [id]
  (build (safe-get properties-db id)))

(defn all-properties [type]
  (->> (vals properties-db)
       (filter #(= type (->type %)))
       (map build)))

(defn- update! [{:keys [property/id] :as property}]
  {:pre [(contains? property :property/id)
         (contains? properties-db id)]}
  (validate property)
  (alter-var-root #'properties-db assoc id property)
  (async-write-to-file!))

(defn- delete! [property-id]
  {:pre [(contains? properties-db property-id)]}
  (alter-var-root #'properties-db dissoc property-id)
  (async-write-to-file!))

;;;;

(def ^:private info-text-k-order [:property/pretty-name
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

(defn index-of [k ^clojure.lang.PersistentVector v]
  (let [idx (.indexOf v k)]
    (if (= -1 idx)
      nil
      idx)))

(defn- sort-k-order [components]
  (sort-by (fn [[k _]] (or (index-of k info-text-k-order) 99))
           components))

(defn- remove-newlines [s]
  (let [new-s (-> s
                  (str/replace "\n\n" "\n")
                  (str/replace #"^\n" "")
                  str/trim-newline)]
    (if (= (count new-s) (count s))
      s
      (remove-newlines new-s))))

(defsystem info-text "Return info-string (for tooltips,etc.). Default nil." [_])
(defmethod info-text :default [_])

(declare ^:dynamic *info-text-entity*)

(defn ->info-text
  "Recursively generates info-text via [[info-text]]."
  [components]
  (->> components
       sort-k-order
       (keep (fn [{v 1 :as component}]
               (str (try (binding [*info-text-entity* components]
                           (info-text component))
                         (catch Throwable t
                           ; calling from property-editor where entity components
                           ; have a different data schema than after ->mk
                           ; and info-text might break
                           (pr-str component)))
                    (when (map? v)
                      (str "\n" (->info-text v))))))
       (str/join "\n")
       remove-newlines))

(defn equal?
  "Returns true if a is nearly equal to b."
  [a b]
  (MathUtils/isEqual a b))

(defn- degree->radians [degree]
  (* (float degree) MathUtils/degreesToRadians))

(defn clamp [value min max]
  (MathUtils/clamp (float value) (float min) (float max)))

; TODO not important badlogic, using clojure vectors
; could extend some protocol by clojure vectors and just require the protocol
; also call vector2 v2/add ? v2/scale ?

(defn- ^Vector2 ->v [[x y]]
  (Vector2. x y))

(defn- ->p [^Vector2 v]
  [(.x ^Vector2 v)
   (.y ^Vector2 v)])

(defn v-scale     [v n]    (->p (.scl ^Vector2 (->v v) (float n)))) ; TODO just (mapv (partial * 2) v)
(defn v-normalise [v]      (->p (.nor ^Vector2 (->v v))))
(defn v-add       [v1 v2]  (->p (.add ^Vector2 (->v v1) ^Vector2 (->v v2))))
(defn v-length    [v]      (.len ^Vector2 (->v v)))
(defn v-distance  [v1 v2]  (.dst ^Vector2 (->v v1) ^Vector2 (->v v2)))

(defn v-normalised? [v] (equal? 1 (v-length v)))

(defn v-get-normal-vectors [[x y]]
  [[(- (float y))         x]
   [          y (- (float x))]])

(defn v-direction [[sx sy] [tx ty]]
  (v-normalise [(- (float tx) (float sx))
                (- (float ty) (float sy))]))

(defn v-get-angle-from-vector
  "converts theta of Vector2 to angle from top (top is 0 degree, moving left is 90 degree etc.), ->counterclockwise"
  [v]
  (.angleDeg (->v v) (Vector2. 0 1)))

(comment

 (pprint
  (for [v [[0 1]
           [1 1]
           [1 0]
           [1 -1]
           [0 -1]
           [-1 -1]
           [-1 0]
           [-1 1]]]
    [v
     (.angleDeg (->v v) (Vector2. 0 1))
     (get-angle-from-vector (->v v))]))

 )

(defn- ->circle [[x y] radius]
  (Circle. x y radius))

(defn- ->rectangle [[x y] width height]
  (Rectangle. x y width height))

(defn- rect-contains? [^Rectangle rectangle [x y]]
  (.contains rectangle x y))

(defmulti ^:private overlaps? (fn [a b] [(class a) (class b)]))

(defmethod overlaps? [Circle Circle]
  [^Circle a ^Circle b]
  (Intersector/overlaps a b))

(defmethod overlaps? [Rectangle Rectangle]
  [^Rectangle a ^Rectangle b]
  (Intersector/overlaps a b))

(defmethod overlaps? [Rectangle Circle]
  [^Rectangle rect ^Circle circle]
  (Intersector/overlaps circle rect))

(defmethod overlaps? [Circle Rectangle]
  [^Circle circle ^Rectangle rect]
  (Intersector/overlaps circle rect))

(defn- rectangle? [{[x y] :left-bottom :keys [width height]}]
  (and x y width height))

(defn- circle? [{[x y] :position :keys [radius]}]
  (and x y radius))

(defn- m->shape [m]
  (cond
   (rectangle? m) (let [{:keys [left-bottom width height]} m]
                    (->rectangle left-bottom width height))

   (circle? m) (let [{:keys [position radius]} m]
                 (->circle position radius))

   :else (throw (Error. (str m)))))

(defn shape-collides? [a b]
  (overlaps? (m->shape a) (m->shape b)))

(defn point-in-rect? [point rectangle]
  (rect-contains? (m->shape rectangle) point))

(defn circle->outer-rectangle [{[x y] :position :keys [radius] :as circle}]
  {:pre [(circle? circle)]}
  (let [radius (float radius)
        size (* radius 2)]
    {:left-bottom [(- (float x) radius)
                   (- (float y) radius)]
     :width  size
     :height size}))

(defprotocol PFastRayCaster
  (fast-ray-blocked? [_ start target]))

; boolean array used because 10x faster than access to clojure grid data structure

; this was a serious performance bottleneck -> alength is counting the whole array?
;(def ^:private width alength)
;(def ^:private height (comp alength first))

; does not show warning on reflection, but shows cast-double a lot.
(defrecord ArrRayCaster [arr width height]
  PFastRayCaster
  (fast-ray-blocked? [_ [start-x start-y] [target-x target-y]]
    (RayCaster/rayBlocked (double start-x)
                          (double start-y)
                          (double target-x)
                          (double target-y)
                          width ;(width boolean-2d-array)
                          height ;(height boolean-2d-array)
                          arr)))

#_(defn ray-steplist [boolean-2d-array [start-x start-y] [target-x target-y]]
  (seq
   (RayCaster/castSteplist start-x
                           start-y
                           target-x
                           target-y
                           (width boolean-2d-array)
                           (height boolean-2d-array)
                           boolean-2d-array)))

#_(defn ray-maxsteps [boolean-2d-array  [start-x start-y] [vector-x vector-y] max-steps]
  (let [erg (RayCaster/castMaxSteps start-x
                                    start-y
                                    vector-x
                                    vector-y
                                    (width boolean-2d-array)
                                    (height boolean-2d-array)
                                    boolean-2d-array
                                    max-steps
                                    max-steps)]
    (if (= -1 erg)
      :not-blocked
      erg)))

; STEPLIST TEST

#_(def current-steplist (atom nil))

#_(defn steplist-contains? [tilex tiley] ; use vector equality
  (some
    (fn [[x y]]
      (and (= x tilex) (= y tiley)))
    @current-steplist))

#_(defn render-line-middle-to-mouse [color]
  (let [[x y] (input/get-mouse-pos)]
    (g/draw-line (/ (g/viewport-width) 2)
                 (/ (g/viewport-height) 2)
                 x y color)))

#_(defn update-test-raycast-steplist []
    (reset! current-steplist
            (map
             (fn [step]
               [(.x step) (.y step)])
             (raycaster/ray-steplist (get-cell-blocked-boolean-array)
                                     (:position @world-player)
                                     (g/map-coords)))))

;; MAXSTEPS TEST

#_(def current-steps (atom nil))

#_(defn update-test-raycast-maxsteps []
    (let [maxsteps 10]
      (reset! current-steps
              (raycaster/ray-maxsteps (get-cell-blocked-boolean-array)
                                      (v-direction (g/map-coords) start)
                                      maxsteps))))

#_(defn draw-test-raycast []
  (let [start (:position @world-player)
        target (g/map-coords)
        color (if (fast-ray-blocked? start target) g/red g/green)]
    (render-line-middle-to-mouse color)))

; PATH BLOCKED TEST

#_(defn draw-test-path-blocked [] ; TODO draw in map no need for screenpos-of-tilepos
  (let [[start-x start-y] (:position @world-player)
        [target-x target-y] (g/map-coords)
        [start1 target1 start2 target2] (create-double-ray-endpositions start-x start-y target-x target-y 0.4)
        [start1screenx,start1screeny]   (screenpos-of-tilepos start1)
        [target1screenx,target1screeny] (screenpos-of-tilepos target1)
        [start2screenx,start2screeny]   (screenpos-of-tilepos start2)
        [target2screenx,target2screeny] (screenpos-of-tilepos target2)
        color (if (is-path-blocked? start1 target1 start2 target2)
                g/red
                g/green)]
    (g/draw-line start1screenx start1screeny target1screenx target1screeny color)
    (g/draw-line start2screenx start2screeny target2screenx target2screeny color)))

(defn find-first
  "Returns the first item of coll for which (pred item) returns logical true.
  Consumes sequences up to the first match, will consume the entire sequence
  and return nil if no match is found."
  [pred coll]
  (first (filter pred coll)))

(defn ->tile [position]
  (mapv int position))

(defn tile->middle [position]
  (mapv (partial + 0.5) position))

(defn safe-merge [m1 m2]
  {:pre [(not-any? #(contains? m1 %) (keys m2))]}
  (merge m1 m2))

; libgdx fn is available:
; (MathUtils/isEqual 1 (length v))
(defn- approx-numbers [a b epsilon]
  (<=
    (Math/abs (float (- a b)))
    epsilon))

(defn- round-n-decimals [^double x n]
  (let [z (Math/pow 10 n)]
    (float
      (/
        (Math/round (float (* x z)))
        z))))

(defn readable-number [^double x]
  {:pre [(number? x)]} ; do not assert (>= x 0) beacuse when using floats x may become -0.000...000something
  (if (or
        (> x 5)
        (approx-numbers x (int x) 0.001)) ; for "2.0" show "2" -> simpler
    (int x)
    (round-n-decimals x 2)))

(defn get-namespaces [packages]
  (filter #(packages (first (str/split (name (ns-name %)) #"\.")))
          (all-ns)))

(defn get-vars [nmspace condition]
  (for [[sym avar] (ns-interns nmspace)
        :when (condition avar)]
    avar))

(defn exit-app!         [] (.exit               Gdx/app))
(defn delta-time        [] (.getDeltaTime       Gdx/graphics))
(defn frames-per-second [] (.getFramesPerSecond Gdx/graphics))

(defmacro post-runnable! [& exprs]
  `(.postRunnable Gdx/app (fn [] ~@exprs)))

(defn- ->gdx-field [klass-str k]
  (eval (symbol (str "com.badlogic.gdx." klass-str "/" (str/replace (str/upper-case (name k)) "-" "_")))))

(def ^:private ->gdx-input-button (partial ->gdx-field "Input$Buttons"))
(def ^:private ->gdx-input-key    (partial ->gdx-field "Input$Keys"))

(defn button-just-pressed?
  ":left, :right, :middle, :back or :forward."
  [b]
  (.isButtonJustPressed Gdx/input (->gdx-input-button b)))

(defn key-just-pressed?
  "See [[key-pressed?]]."
  [k]
  (.isKeyJustPressed Gdx/input (->gdx-input-key k)))

(defn key-pressed?
  "For options see [libgdx Input$Keys docs](https://javadoc.io/doc/com.badlogicgames.gdx/gdx/latest/com/badlogic/gdx/Input.Keys.html).
  Keys are upper-cased and dashes replaced by underscores.
  For example Input$Keys/ALT_LEFT can be used with :alt-left.
  Numbers via :num-3, etc."
  [k]
  (.isKeyPressed Gdx/input (->gdx-input-key k)))

(defn- set-input-processor! [processor]
  (.setInputProcessor Gdx/input processor))

(defn- kw->color [k] (->gdx-field "graphics.Color" k))

(def white Color/WHITE)
(def black Color/BLACK)

(defn ->color
  ([r g b] (->color r g b 1))
  ([r g b a] (Color. (float r) (float g) (float b) (float a))))

(defn- munge-color ^Color [color]
  (cond (= Color (class color)) color
        (keyword? color) (kw->color color)
        (vector? color) (apply ->color color)
        :else (throw (ex-info "Cannot understand color" {:color color}))))

(defn def-markup-color
  "A general purpose class containing named colors that can be changed at will. For example, the markup language defined by the BitmapFontCache class uses this class to retrieve colors and the user can define his own colors.

  [javadoc](https://javadoc.io/doc/com.badlogicgames.gdx/gdx/latest/com/badlogic/gdx/graphics/Colors.html)"
  [name-str color]
  (Colors/put name-str (munge-color color)))

(def dispose! Disposable/.dispose)

(defn- internal-file ^FileHandle [path]
  (.internal Gdx/files path))

(defn- recursively-search [folder extensions]
  (loop [[^FileHandle file & remaining] (.list (internal-file folder))
         result []]
    (cond (nil? file)
          result

          (.isDirectory file)
          (recur (concat remaining (.list file)) result)

          (extensions (.extension file))
          (recur remaining (conj result (.path file)))

          :else
          (recur remaining result))))

(defn- search-files [folder file-extensions]
  (map #(str/replace-first % folder "")
       (recursively-search folder file-extensions)))

(defn- load-assets [^AssetManager manager files class-k]
  (let [^Class klass (case class-k :sound Sound :texture Texture)]
    (doseq [file files]
      (.load manager ^String file klass))))

(defn- asset-manager []
  (proxy [AssetManager clojure.lang.ILookup] []
    (valAt [file]
      (.get ^AssetManager this ^String file))))

(declare assets
         ^:private all-sound-files
         ^:private all-texture-files)

(defn- load-assets! [folder]
  (let [manager (asset-manager)
        sound-files   (search-files folder #{"wav"})
        texture-files (search-files folder #{"png" "bmp"})]
    (load-assets manager sound-files   :sound)
    (load-assets manager texture-files :texture)
    (.finishLoading manager)
    (bind-root #'assets manager)
    (bind-root #'all-sound-files sound-files)
    (bind-root #'all-texture-files texture-files)))

(defn- play-sound! [path] (Sound/.play (get assets path)))

(defc :tx/sound {:data :sound}
  (do! [[_ file]] (play-sound! file) nil))

(defn camera-position
  "Returns camera position as [x y] vector."
  [^Camera camera]
  [(.x (.position camera))
   (.y (.position camera))])

(defn camera-set-position!
  "Sets x and y and calls update on the camera."
  [^Camera camera [x y]]
  (set! (.x (.position camera)) (float x))
  (set! (.y (.position camera)) (float y))
  (.update camera))

(defn frustum [^Camera camera]
  (let [frustum-points (for [^Vector3 point (take 4 (.planePoints (.frustum camera)))
                             :let [x (.x point)
                                   y (.y point)]]
                         [x y])
        left-x   (apply min (map first  frustum-points))
        right-x  (apply max (map first  frustum-points))
        bottom-y (apply min (map second frustum-points))
        top-y    (apply max (map second frustum-points))]
    [left-x right-x bottom-y top-y]))

(defn visible-tiles [camera]
  (let [[left-x right-x bottom-y top-y] (frustum camera)]
    (for  [x (range (int left-x)   (int right-x))
           y (range (int bottom-y) (+ 2 (int top-y)))]
      [x y])))

(defn calculate-zoom
  "calculates the zoom value for camera to see all the 4 points."
  [^Camera camera & {:keys [left top right bottom]}]
  (let [viewport-width  (.viewportWidth  camera)
        viewport-height (.viewportHeight camera)
        [px py] (camera-position camera)
        px (float px)
        py (float py)
        leftx (float (left 0))
        rightx (float (right 0))
        x-diff (max (- px leftx) (- rightx px))
        topy (float (top 1))
        bottomy (float (bottom 1))
        y-diff (max (- topy py) (- py bottomy))
        vp-ratio-w (/ (* x-diff 2) viewport-width)
        vp-ratio-h (/ (* y-diff 2) viewport-height)
        new-zoom (max vp-ratio-w vp-ratio-h)]
    new-zoom))

(defn zoom [^OrthographicCamera camera]
  (.zoom camera))

(defn set-zoom!
  "Sets the zoom value and updates."
  [^OrthographicCamera camera amount]
  (set! (.zoom camera) amount)
  (.update camera))

(defn reset-zoom!
  "Sets the zoom value to 1."
  [camera]
  (set-zoom! camera 1))

(defn- ->camera ^OrthographicCamera [] (OrthographicCamera.))

; touch coordinates are y-down, while screen coordinates are y-up
; so the clamping of y is reverse, but as black bars are equal it does not matter
(defn- unproject-mouse-posi
  "Returns vector of [x y]."
  [^Viewport viewport]
  (let [mouse-x (clamp (.getX Gdx/input)
                       (.getLeftGutterWidth viewport)
                       (.getRightGutterX viewport))
        mouse-y (clamp (.getY Gdx/input)
                       (.getTopGutterHeight viewport)
                       (.getTopGutterY viewport))
        coords (.unproject viewport (Vector2. mouse-x mouse-y))]
    [(.x coords) (.y coords)]))

(defn- ->gui-viewport [world-width world-height]
  (FitViewport. world-width world-height (->camera)))

(defn- ->world-viewport [world-width world-height unit-scale]
  (let [world-width  (* world-width  unit-scale)
        world-height (* world-height unit-scale)
        camera (->camera)
        y-down? false]
    (.setToOrtho camera y-down? world-width world-height)
    (FitViewport. world-width world-height camera)))

(defn- vp-world-width  [^Viewport vp] (.getWorldWidth  vp))
(defn- vp-world-height [^Viewport vp] (.getWorldHeight vp))
(defn- vp-camera       [^Viewport vp] (.getCamera      vp))
(defn- vp-update!      [^Viewport vp [w h] & {:keys [center-camera?]}]
  (.update vp w h (boolean center-camera?)))

(defn ->texture-region
  ([path-or-texture]
   (let [^Texture tex (if (string? path-or-texture)
                        (get assets path-or-texture)
                        path-or-texture)]
     (TextureRegion. tex)))

  ([^TextureRegion texture-region [x y w h]]
   (TextureRegion. texture-region (int x) (int y) (int w) (int h))))

(defn texture-region-dimensions [^TextureRegion texture-region]
  [(.getRegionWidth  texture-region)
   (.getRegionHeight texture-region)])

(declare ^:private batch)

; TODO [x y] is center or left-bottom ?
; why rotation origin calculations ?!
(defn- draw-texture-region [^Batch batch texture-region [x y] [w h] rotation color]
  (if color (.setColor batch color)) ; TODO move out, simplify ....
  (.draw batch
         texture-region
         x
         y
         (/ (float w) 2) ; rotation origin
         (/ (float h) 2)
         w ; width height
         h
         1 ; scaling factor
         1
         rotation)
  (if color (.setColor batch Color/WHITE)))

(defn- draw-with! [^Batch batch ^Viewport viewport draw-fn]
  (.setColor batch Color/WHITE) ; fix scene2d.ui.tooltip flickering
  (.setProjectionMatrix batch (.combined (.getCamera viewport)))
  (.begin batch)
  (draw-fn)
  (.end batch))

(defn- ->shape-drawer [batch]
  (let [^Texture tex (let [pixmap (doto (Pixmap. 1 1 Pixmap$Format/RGBA8888)
                                    (.setColor Color/WHITE)
                                    (.drawPixel 0 0))
                           tex (Texture. pixmap)]
                       (dispose! pixmap)
                       tex)]
    {:shape-drawer (ShapeDrawer. batch (TextureRegion. tex 1 0 1 1))
     :shape-drawer-texture tex}))

(defn- set-color!          [^ShapeDrawer sd color] (.setColor sd (munge-color color)))
(defn- sd-ellipse          [^ShapeDrawer sd [x y] radius-x radius-y] (.ellipse sd (float x) (float y) (float radius-x) (float radius-y)))
(defn- sd-filled-ellipse   [^ShapeDrawer sd [x y] radius-x radius-y] (.filledEllipse sd (float x) (float y) (float radius-x) (float radius-y)))
(defn- sd-circle           [^ShapeDrawer sd [x y] radius] (.circle sd (float x) (float y) (float radius)))
(defn- sd-filled-circle    [^ShapeDrawer sd [x y] radius] (.filledCircle sd (float x) (float y) (float radius)))
(defn- sd-arc              [^ShapeDrawer sd [centre-x centre-y] radius start-angle degree] (.arc sd centre-x centre-y radius (degree->radians start-angle) (degree->radians degree)))
(defn- sd-sector           [^ShapeDrawer sd [centre-x centre-y] radius start-angle degree] (.sector sd centre-x centre-y radius (degree->radians start-angle) (degree->radians degree)))
(defn- sd-rectangle        [^ShapeDrawer sd x y w h] (.rectangle sd x y w h))
(defn- sd-filled-rectangle [^ShapeDrawer sd x y w h] (.filledRectangle sd (float x) (float y) (float w) (float h)) )
(defn- sd-line             [^ShapeDrawer sd [sx sy] [ex ey]] (.line sd (float sx) (float sy) (float ex) (float ey)))

(defn- sd-grid [sd leftx bottomy gridw gridh cellw cellh]
  (let [w (* (float gridw) (float cellw))
        h (* (float gridh) (float cellh))
        topy (+ (float bottomy) (float h))
        rightx (+ (float leftx) (float w))]
    (doseq [idx (range (inc (float gridw)))
            :let [linex (+ (float leftx) (* (float idx) (float cellw)))]]
      (sd-line sd [linex topy] [linex bottomy]))
    (doseq [idx (range (inc (float gridh)))
            :let [liney (+ (float bottomy) (* (float idx) (float cellh)))]]
      (sd-line sd [leftx liney] [rightx liney]))))

(defn- sd-with-line-width [^ShapeDrawer sd width draw-fn]
  (let [old-line-width (.getDefaultLineWidth sd)]
    (.setDefaultLineWidth sd (float (* (float width) old-line-width)))
    (draw-fn)
    (.setDefaultLineWidth sd (float old-line-width))))

(defn- ->ttf-params [size quality-scaling]
  (let [params (FreeTypeFontGenerator$FreeTypeFontParameter.)]
    (set! (.size params) (* size quality-scaling))
    ; .color and this:
    ;(set! (.borderWidth parameter) 1)
    ;(set! (.borderColor parameter) red)
    (set! (.minFilter params) Texture$TextureFilter/Linear) ; because scaling to world-units
    (set! (.magFilter params) Texture$TextureFilter/Linear)
    params))

(defn- generate-ttf [{:keys [file size quality-scaling]}]
  (let [generator (FreeTypeFontGenerator. (internal-file file))
        font (.generateFont generator (->ttf-params size quality-scaling))]
    (dispose! generator)
    (.setScale (.getData font) (float (/ quality-scaling)))
    (set! (.markupEnabled (.getData font)) true)
    (.setUseIntegerPositions font false) ; otherwise scaling to world-units (/ 1 48)px not visible
    font))

(defn- gdx-default-font [] (BitmapFont.))

(defn- text-height [^BitmapFont font text]
  (-> text
      (str/split #"\n")
      count
      (* (.getLineHeight font))))

(defn- font-draw [^BitmapFont font
                  unit-scale
                  batch
                  {:keys [x y text h-align up? scale]}]
  (let [data (.getData font)
        old-scale (float (.scaleX data))]
    (.setScale data (* old-scale (float unit-scale) (float (or scale 1))))
    (.draw font
           batch
           (str text)
           (float x)
           (+ (float y) (float (if up? (text-height font text) 0)))
           (float 0) ; target-width
           (case (or h-align :center)
             :center Align/center
             :left   Align/left
             :right  Align/right)
           false) ; wrap false, no need target-width
    (.setScale data old-scale)))

(declare ^:private shape-drawer
         ^:private shape-drawer-texture)

(defn draw-ellipse [position radius-x radius-y color]
  (set-color! shape-drawer color)
  (sd-ellipse shape-drawer position radius-x radius-y))
(defn draw-filled-ellipse [position radius-x radius-y color]
  (set-color! shape-drawer color)
  (sd-filled-ellipse shape-drawer position radius-x radius-y))
(defn draw-circle [position radius color]
  (set-color! shape-drawer color)
  (sd-circle shape-drawer position radius))
(defn draw-filled-circle [position radius color]
  (set-color! shape-drawer color)
  (sd-filled-circle shape-drawer position radius))
(defn draw-arc [center radius start-angle degree color]
  (set-color! shape-drawer color)
  (sd-arc shape-drawer center radius start-angle degree))
(defn draw-sector [center radius start-angle degree color]
  (set-color! shape-drawer color)
  (sd-sector shape-drawer center radius start-angle degree))
(defn draw-rectangle [x y w h color]
  (set-color! shape-drawer color)
  (sd-rectangle shape-drawer x y w h))
(defn draw-filled-rectangle [x y w h color]
  (set-color! shape-drawer color)
  (sd-filled-rectangle shape-drawer x y w h))
(defn draw-line [start end color]
  (set-color! shape-drawer color)
  (sd-line shape-drawer start end))
(defn draw-grid [leftx bottomy gridw gridh cellw cellh color]
  (set-color! shape-drawer color)
  (sd-grid shape-drawer leftx bottomy gridw gridh cellw cellh))
(defn with-shape-line-width [width draw-fn]
  (sd-with-line-width shape-drawer width draw-fn))

(defn- ->gui-view [{:keys [world-width world-height]}]
  {:unit-scale 1
   :viewport (->gui-viewport world-width world-height)})

(defn- ->world-view [{:keys [world-width world-height tile-size]}]
  (let [unit-scale (/ tile-size)]
    {:unit-scale (float unit-scale)
     :viewport (->world-viewport world-width world-height unit-scale)}))

(declare ^:private gui-view
         ^:private world-view)

(defn- bind-views! [{:keys [gui-view world-view]}]
  (bind-root #'gui-view (->gui-view gui-view))
  (bind-root #'world-view (->world-view world-view)))

(defn world-unit-scale []
  (:unit-scale world-view))

(defn pixels->world-units [pixels]
  (* (int pixels) (world-unit-scale)))

(defn- gui-viewport   [] (:viewport gui-view))
(defn- world-viewport [] (:viewport world-view))

(defn- gui-mouse-position* []
  ; TODO mapv int needed?
  (mapv int (unproject-mouse-posi (gui-viewport))))

(defn- world-mouse-position* []
  ; TODO clamping only works for gui-viewport ? check. comment if true
  ; TODO ? "Can be negative coordinates, undefined cells."
  (unproject-mouse-posi (world-viewport)))

(defn gui-mouse-position    [] (gui-mouse-position*))
(defn world-mouse-position  [] (world-mouse-position*))
(defn gui-viewport-width    [] (vp-world-width  (gui-viewport)))
(defn gui-viewport-height   [] (vp-world-height (gui-viewport)))
(defn world-camera          [] (vp-camera       (world-viewport)))
(defn world-viewport-width  [] (vp-world-width  (world-viewport)))
(defn world-viewport-height [] (vp-world-height (world-viewport)))

(defrecord Sprite [texture-region
                   pixel-dimensions
                   world-unit-dimensions
                   color]) ; optional

(declare ^:private ^:dynamic *unit-scale*)

(defn- unit-dimensions [image]
  (if (= *unit-scale* 1)
    (:pixel-dimensions image)
    (:world-unit-dimensions image)))

(defn- scale-dimensions [dimensions scale]
  (mapv (comp float (partial * scale)) dimensions))

(defn- assoc-dimensions
  "scale can be a number for multiplying the texture-region-dimensions or [w h]."
  [{:keys [texture-region] :as image} scale]
  {:pre [(or (number? scale)
             (and (vector? scale)
                  (number? (scale 0))
                  (number? (scale 1))))]}
  (let [pixel-dimensions (if (number? scale)
                           (scale-dimensions (texture-region-dimensions texture-region) scale)
                           scale)]
    (assoc image
           :pixel-dimensions pixel-dimensions
           :world-unit-dimensions (scale-dimensions pixel-dimensions (world-unit-scale)))))

(defn draw-image [{:keys [texture-region color] :as image} position]
  (draw-texture-region batch
                       texture-region
                       position
                       (unit-dimensions image)
                       0 ; rotation
                       color))

(defn draw-rotated-centered-image
  [{:keys [texture-region color] :as image} rotation [x y]]
  (let [[w h] (unit-dimensions image)]
    (draw-texture-region batch
                         texture-region
                         [(- (float x) (/ (float w) 2))
                          (- (float y) (/ (float h) 2))]
                         [w h]
                         rotation
                         color)))

(defn draw-centered-image [image position]
  (draw-rotated-centered-image image 0 position))

(defn- ->image* [texture-region]
  (-> {:texture-region texture-region}
      (assoc-dimensions 1) ; = scale 1
      map->Sprite))

(defn ->image [file]
  (->image* (->texture-region file)))

(defn sub-image [{:keys [texture-region]} bounds]
  (->image* (->texture-region texture-region bounds)))

(defn sprite-sheet [file tilew tileh]
  {:image (->image file)
   :tilew tilew
   :tileh tileh})

(defn sprite
  "x,y index starting top-left"
  [{:keys [image tilew tileh]} [x y]]
  (sub-image image [(* x tilew) (* y tileh) tilew tileh]))

(defn- edn->image [{:keys [file sub-image-bounds]}]
  (if sub-image-bounds
    (let [[sprite-x sprite-y] (take 2 sub-image-bounds)
          [tilew tileh]       (drop 2 sub-image-bounds)]
      (sprite (sprite-sheet file tilew tileh)
              [(int (/ sprite-x tilew))
               (int (/ sprite-y tileh))]))
    (->image file)))

(defn- ->default-font [true-type-font]
  (or (and true-type-font (generate-ttf true-type-font))
      (gdx-default-font)))

(declare ^:private default-font)

(defn draw-text
  "font, h-align, up? and scale are optional.
  h-align one of: :center, :left, :right. Default :center.
  up? renders the font over y, otherwise under.
  scale will multiply the drawn text size with the scale."
  [{:keys [x y text font h-align up? scale] :as opts}]
  (font-draw (or font default-font) *unit-scale* batch opts))

(defn- mapvals [f m]
  (into {} (for [[k v] m]
             [k (f v)])))

(defn- ->cursor [file [hotspot-x hotspot-y]]
  (let [pixmap (Pixmap. (internal-file file))
        cursor (.newCursor Gdx/graphics pixmap hotspot-x hotspot-y)]
    (dispose! pixmap)
    cursor))

(defn- ->cursors [cursors]
  (mapvals (fn [[file hotspot]]
             (->cursor (str "cursors/" file ".png") hotspot))
           cursors))

(declare ^:private cursors )

(defn set-cursor! [cursor-key]
  (.setCursor Gdx/graphics (safe-get cursors cursor-key)))

(defc :tx/cursor
  (do! [[_ cursor-key]]
    (set-cursor! cursor-key)
    nil))

(defn- render-view! [{:keys [viewport unit-scale]} draw-fn]
  (draw-with! batch
              viewport
              (fn []
                (with-shape-line-width unit-scale
                  #(binding [*unit-scale* unit-scale]
                     (draw-fn))))))

(defn render-gui-view!   [render-fn] (render-view! gui-view render-fn))
(defn render-world-view! [render-fn] (render-view! world-view render-fn))

(declare draw-item-on-cursor)

(declare ^:private cached-map-renderer)

(defn draw-tiled-map
  "Renders tiled-map using world-view at world-camera position and with world-unit-scale.

  Color-setter is a `(fn [color x y])` which is called for every tile-corner to set the color.

  Can be used for lights & shadows.

  Renders only visible layers."
  [tiled-map color-setter]
  (t/render-tm! (cached-map-renderer tiled-map)
                color-setter
                (world-camera)
                tiled-map))

(defn- ->tiled-map-renderer [tiled-map]
  (t/->orthogonal-tiled-map-renderer tiled-map (world-unit-scale) batch))

(defn- load-graphics! [{:keys [views default-font cursors]}]
  (let [batch (SpriteBatch.)
        {:keys [shape-drawer shape-drawer-texture]} (->shape-drawer batch)]
    (bind-root #'batch batch)
    (bind-root #'shape-drawer shape-drawer)
    (bind-root #'shape-drawer-texture shape-drawer-texture)
    (bind-root #'cursors (->cursors cursors))
    (bind-root #'default-font (->default-font default-font))
    (bind-views! views)
    (bind-root #'cached-map-renderer (memoize ->tiled-map-renderer))))

(defn- dispose-graphics! []
  (dispose! batch)
  (dispose! shape-drawer-texture)
  (dispose! default-font)
  (run! dispose! (vals cursors)))

(declare ^:private screen-k
         ^:private screens)

(defn current-screen []
  [screen-k (screen-k screens)])

(defsystem screen-enter "FIXME" [_])
(defmethod screen-enter :default [_])

(defsystem screen-exit  "FIXME" [_])
(defmethod screen-exit :default  [_])

(defn change-screen
  "Calls `screen-exit` on the current-screen (if there is one).
  Calls `screen-enter` on the new screen."
  [new-k]
  (when-let [v (and (bound? #'screen-k) (screen-k screens))]
    (screen-exit [screen-k v]))
  (let [v (new-k screens)]
    (assert v (str "Cannot find screen with key: " new-k))
    (bind-root #'screen-k new-k)
    (screen-enter [new-k v])))

(defsystem ^:private screen-render! "FIXME" [_])

(defsystem screen-render "FIXME" [_])
(defmethod screen-render :default [_])

(defsystem ->mk "Create component value. Default returns v." [_])
(defmethod ->mk :default [[_ v]] v)

(defn create-vs
  "Creates a map for every component with map entries `[k (->mk [k v])]`."
  [components]
  (reduce (fn [m [k v]]
            (assoc m k (->mk [k v])))
          {}
          components))

(defn- load-screens! [screen-ks]
  (bind-root #'screens (create-vs (zipmap screen-ks (repeat nil))))
  (change-screen (ffirst screens)))

(defn- dispose-screens! []
  ; TODO screens not disposed https://github.com/damn/core/issues/41
  )

(defn- check-cleanup-visui! []
  ; app crashes during startup before VisUI/dispose and we do clojure.tools.namespace.refresh-> gui elements not showing.
  ; => actually there is a deeper issue at play
  ; we need to dispose ALL resources which were loaded already ...
  (when (VisUI/isLoaded)
    (VisUI/dispose)))

(defn- font-enable-markup! []
  (-> (VisUI/getSkin)
      (.getFont "default-font")
      .getData
      .markupEnabled
      (set! true)))

(defn- set-tooltip-config! []
  (set! Tooltip/DEFAULT_APPEAR_DELAY_TIME (float 0))
  ;(set! Tooltip/DEFAULT_FADE_TIME (float 0.3))
  ;Controls whether to fade out tooltip when mouse was moved. (default false)
  ;(set! Tooltip/MOUSE_MOVED_FADEOUT true)
  )

(defn- load-ui! [skin-scale]
  (check-cleanup-visui!)
  (VisUI/load (case skin-scale
                :skin-scale/x1 VisUI$SkinScale/X1
                :skin-scale/x2 VisUI$SkinScale/X2))
  (font-enable-markup!)
  (set-tooltip-config!))

(defn- dispose-ui! []
  (VisUI/dispose))

(defn actor-x [^Actor a] (.getX a))
(defn actor-y [^Actor a] (.getY a))

(defn actor-id [^Actor actor]
  (.getUserObject actor))

(defn set-id! [^Actor actor id]
  (.setUserObject actor id))

(defn set-name! [^Actor actor name]
  (.setName actor name))

(defn actor-name [^Actor actor]
  (.getName actor))

(defn visible? [^Actor actor] ; used
  (.isVisible actor))

(defn set-visible! [^Actor actor bool]
  (.setVisible actor (boolean bool)))

(defn toggle-visible! [actor] ; used
  (set-visible! actor (not (visible? actor))))

(defn set-position! [^Actor actor x y]
  (.setPosition actor x y))

(defn set-center! [^Actor actor x y]
  (set-position! actor
                 (- x (/ (.getWidth actor) 2))
                 (- y (/ (.getHeight actor) 2))))

(defn set-touchable!
  ":children-only, :disabled or :enabled."
  [^Actor actor touchable]
  (.setTouchable actor (case touchable
                         :children-only Touchable/childrenOnly
                         :disabled      Touchable/disabled
                         :enabled       Touchable/enabled)))

(defn add-listener! [^Actor actor listener]
  (.addListener actor listener))

(defn remove!
  "Removes this actor from its parent, if it has a parent."
  [^Actor actor]
  (.remove actor))

(defn parent
  "Returns the parent actor, or null if not in a group."
  [^Actor actor]
  (.getParent actor))

(defn a-mouseover? [^Actor actor [x y]]
  (let [v (.stageToLocalCoordinates actor (Vector2. x y))]
    (.hit actor (.x v) (.y v) true)))

(defn remove-tooltip! [^Actor actor]
  (Tooltip/removeTooltip actor))

(defn find-ancestor-window ^Window [^Actor actor]
  (if-let [p (parent actor)]
    (if (instance? Window p)
      p
      (find-ancestor-window p))
    (throw (Error. (str "Actor has no parent window " actor)))))

(defn pack-ancestor-window! [^Actor actor]
  (.pack (find-ancestor-window actor)))

(defn children
  "Returns an ordered list of child actors in this group."
  [^Group group]
  (seq (.getChildren group)))

(defn clear-children!
  "Removes all actors from this group and unfocuses them."
  [^Group group]
  (.clearChildren group))

(defn add-actor!
  "Adds an actor as a child of this group, removing it from its previous parent. If the actor is already a child of this group, no changes are made."
  [^Group group actor]
  (.addActor group actor))

(defn find-actor-with-id [group id]
  (let [actors (children group)
        ids (keep actor-id actors)]
    (assert (or (empty? ids)
                (apply distinct? ids)) ; TODO could check @ add
            (str "Actor ids are not distinct: " (vec ids)))
    (first (filter #(= id (actor-id %)) actors))))

(defn set-cell-opts [^Cell cell opts]
  (doseq [[option arg] opts]
    (case option
      :fill-x?    (.fillX     cell)
      :fill-y?    (.fillY     cell)
      :expand?    (.expand    cell)
      :expand-x?  (.expandX   cell)
      :expand-y?  (.expandY   cell)
      :bottom?    (.bottom    cell)
      :colspan    (.colspan   cell (int arg))
      :pad        (.pad       cell (float arg))
      :pad-top    (.padTop    cell (float arg))
      :pad-bottom (.padBottom cell (float arg))
      :width      (.width     cell (float arg))
      :height     (.height    cell (float arg))
      :right?     (.right     cell)
      :left?      (.left      cell))))

(defn add-rows!
  "rows is a seq of seqs of columns.
  Elements are actors or nil (for just adding empty cells ) or a map of
  {:actor :expand? :bottom?  :colspan int :pad :pad-bottom}. Only :actor is required."
  [^Table table rows]
  (doseq [row rows]
    (doseq [props-or-actor row]
      (cond
       (map? props-or-actor) (-> (.add table ^Actor (:actor props-or-actor))
                                 (set-cell-opts (dissoc props-or-actor :actor)))
       :else (.add table ^Actor props-or-actor)))
    (.row table))
  table)

(defn- set-table-opts [^Table table {:keys [rows cell-defaults]}]
  (set-cell-opts (.defaults table) cell-defaults)
  (add-rows! table rows))

(defn t-row!   "Add row to table." [^Table t] (.row   t))
(defn t-clear! "Clear table. "     [^Table t] (.clear t))

(defn t-add!
  "Add to table"
  ([^Table t] (.add t))
  ([^Table t ^Actor a] (.add t a)))

(defn ->horizontal-separator-cell [colspan]
  {:actor (Separator. "default")
   :pad-top 2
   :pad-bottom 2
   :colspan colspan
   :fill-x? true
   :expand-x? true})

(defn ->vertical-separator-cell []
  {:actor (Separator. "vertical")
   :pad-top 2
   :pad-bottom 2
   :fill-y? true
   :expand-y? true})

; candidate for opts: :tooltip
(defn- set-actor-opts [actor {:keys [id name visible? touchable center-position position] :as opts}]
  (when id   (set-id!   actor id))
  (when name (set-name! actor name))
  (when (contains? opts :visible?)  (set-visible! actor visible?))
  (when touchable (set-touchable! actor touchable))
  (when-let [[x y] center-position] (set-center!   actor x y))
  (when-let [[x y] position]        (set-position! actor x y))
  actor)

(comment
 ; fill parent & pack is from Widget TODO ( not widget-group ?)
 com.badlogic.gdx.scenes.scene2d.ui.Widget
 ; about .pack :
 ; Generally this method should not be called in an actor's constructor because it calls Layout.layout(), which means a subclass would have layout() called before the subclass' constructor. Instead, in constructors simply set the actor's size to Layout.getPrefWidth() and Layout.getPrefHeight(). This allows the actor to have a size at construction time for more convenient use with groups that do not layout their children.
 )

(defn- set-widget-group-opts [^WidgetGroup widget-group {:keys [fill-parent? pack?]}]
  (.setFillParent widget-group (boolean fill-parent?)) ; <- actor? TODO
  (when pack?
    (.pack widget-group))
  widget-group)

(defn- set-opts [actor opts]
  (set-actor-opts actor opts)
  (when (instance? Table actor)
    (set-table-opts actor opts)) ; before widget-group-opts so pack is packing rows
  (when (instance? WidgetGroup actor)
    (set-widget-group-opts actor opts))
  actor)

#_(defn- add-window-close-button [^Window window]
    (.add (.getTitleTable window)
          (text-button "x" #(.setVisible window false)))
    window)

(defmacro ^:private proxy-ILookup
  "For actors inheriting from Group."
  [class args]
  `(proxy [~class clojure.lang.ILookup] ~args
     (valAt
       ([id#]
        (find-actor-with-id ~'this id#))
       ([id# not-found#]
        (or (find-actor-with-id ~'this id#) not-found#)))))

(defn ->group [{:keys [actors] :as opts}]
  (let [group (proxy-ILookup Group [])]
    (run! #(add-actor! group %) actors)
    (set-opts group opts)))

(defn ->horizontal-group [{:keys [space pad]}]
  (let [group (proxy-ILookup HorizontalGroup [])]
    (when space (.space group (float space)))
    (when pad   (.pad   group (float pad)))
    group))

(defn ->vertical-group [actors]
  (let [group (proxy-ILookup VerticalGroup [])]
    (run! #(add-actor! group %) actors)
    group))

(defn ->button-group
  "https://javadoc.io/doc/com.badlogicgames.gdx/gdx/latest/com/badlogic/gdx/scenes/scene2d/ui/ButtonGroup.html"
  [{:keys [max-check-count min-check-count]}]
  (let [button-group (ButtonGroup.)]
    (.setMaxCheckCount button-group max-check-count)
    (.setMinCheckCount button-group min-check-count)
    button-group))

(defn ->check-box
  "on-clicked is a fn of one arg, taking the current isChecked state
  [com.kotcrab.vis.ui.widget.VisCheckBox](https://www.javadoc.io/static/com.kotcrab.vis/vis-ui/1.5.3/com/kotcrab/vis/ui/widget/VisCheckBox.html)"
  [text on-clicked checked?]
  (let [^Button button (VisCheckBox. ^String text)]
    (.setChecked button checked?)
    (.addListener button
                  (proxy [ChangeListener] []
                    (changed [event ^Button actor]
                      (on-clicked (.isChecked actor)))))
    button))

(defn ->select-box [{:keys [items selected]}]
  (doto (VisSelectBox.)
    (.setItems ^"[Lcom.badlogic.gdx.scenes.scene2d.Actor;" (into-array items))
    (.setSelected selected)))

(defn ->table ^Table [opts]
  (-> (proxy-ILookup VisTable [])
      (set-opts opts)))

(defn ->window ^VisWindow [{:keys [title modal? close-button? center? close-on-escape?] :as opts}]
  (-> (let [window (doto (proxy-ILookup VisWindow [^String title true]) ; true = showWindowBorder
                     (.setModal (boolean modal?)))]
        (when close-button?    (.addCloseButton window))
        (when center?          (.centerWindow   window))
        (when close-on-escape? (.closeOnEscape  window))
        window)
      (set-opts opts)))

(defn ->label ^VisLabel [text]
  (VisLabel. ^CharSequence text))

(defn ->text-field [^String text opts]
  (-> (VisTextField. text)
      (set-opts opts)))

; TODO is not decendend of SplitPane anymore => check all type hints here
(defn ->split-pane [{:keys [^Actor first-widget
                            ^Actor second-widget
                            ^Boolean vertical?] :as opts}]
  (-> (VisSplitPane. first-widget second-widget vertical?)
      (set-actor-opts opts)))

(defn ->stack [actors]
  (proxy-ILookup Stack [(into-array Actor actors)]))

(defmulti ^:private ->vis-image type)
(defmethod ->vis-image Drawable      [^Drawable d      ] (VisImage.  d))
(defmethod ->vis-image TextureRegion [^TextureRegion tr] (VisImage. tr))

; TODO widget also make, for fill parent
(defn ->ui-image-widget
  "Takes either a texture-region or drawable. Opts are :scaling, :align and actor opts."
  [object {:keys [scaling align fill-parent?] :as opts}]
  (-> (let [^Image image (->vis-image object)]
        (when (= :center align) (.setAlign image Align/center))
        (when (= :fill scaling) (.setScaling image Scaling/fill))
        (when fill-parent? (.setFillParent image true))
        image)
      (set-opts opts)))

(defn ->image-widget [image opts]
  (->ui-image-widget (:texture-region image) opts))

; => maybe with VisImage not necessary anymore?
(defn ->texture-region-drawable [^TextureRegion texture-region]
  (TextureRegionDrawable. texture-region))

(defn ->scroll-pane [actor]
  (let [scroll-pane (VisScrollPane. actor)]
    (.setFlickScroll scroll-pane false)
    (.setFadeScrollBars scroll-pane false)
    scroll-pane))

(defn- button-class? [actor]
  (some #(= Button %) (supers (class actor))))

(defn button?
  "Returns true if the actor or its parent is a button."
  [actor]
  (or (button-class? actor)
      (and (parent actor)
           (button-class? (parent actor)))))

(defn window-title-bar?
  "Returns true if the actor is a window title bar."
  [actor]
  (when (instance? Label actor)
    (when-let [p (parent actor)]
      (when-let [p (parent p)]
        (and (instance? VisWindow p)
             (= (.getTitleLabel ^Window p) actor))))))

(defn add-tooltip!
  "tooltip-text is a (fn []) or a string. If it is a function will be-recalculated every show."
  [^Actor actor tooltip-text]
  (let [text? (string? tooltip-text)
        label (VisLabel. (if text? tooltip-text ""))
        tooltip (proxy [Tooltip] []
                  ; hooking into getWidth because at
                  ; https://github.com/kotcrab/vis-blob/master/ui/src/main/java/com/kotcrab/vis/ui/widget/Tooltip.java#L271
                  ; when tooltip position gets calculated we setText (which calls pack) before that
                  ; so that the size is correct for the newly calculated text.
                  (getWidth []
                    (let [^Tooltip this this]
                      (when-not text?
                        (.setText this (str (tooltip-text))))
                      (proxy-super getWidth))))]
    (.setAlignment label Align/center)
    (.setTarget  tooltip ^Actor actor)
    (.setContent tooltip ^Actor label)))

(declare ^:dynamic *on-clicked-actor*)

(defn- ->change-listener [on-clicked]
  (proxy [ChangeListener] []
    (changed [event actor]
      (binding [*on-clicked-actor* actor]
        (on-clicked)))))

(defn ->text-button [text on-clicked]
  (let [button (VisTextButton. ^String text)]
    (.addListener button (->change-listener on-clicked))
    button))

(defn- ->ui-image-button [texture-region scale on-clicked]
  (let [drawable (TextureRegionDrawable. ^TextureRegion texture-region)
        button (VisImageButton. drawable)]
    (when scale
      (let [[w h] (texture-region-dimensions texture-region)]
        (.setMinSize drawable (float (* scale w)) (float (* scale h)))))
    (.addListener button (->change-listener on-clicked))
    button))

; TODO check how to make toggle-able ? with hotkeys for actionbar trigger ?
(defn ->image-button
  ([image on-clicked]
   (->image-button image on-clicked {}))

  ([image on-clicked {:keys [scale]}]
   (->ui-image-button (:texture-region image) scale on-clicked)))

(defn- ->ui-actor [draw! act!]
  (proxy [Actor] []
    (draw [_batch _parent-alpha]
      (draw!))
    (act [_delta]
      (act!))))

(defn ->ui-widget [draw!]
  (proxy [Widget] []
    (draw [_batch _parent-alpha]
      (draw! this))))

(defn set-drawable! [^Image image drawable]
  (.setDrawable image drawable))

(defn set-min-size! [^TextureRegionDrawable drawable size]
  (.setMinSize drawable (float size) (float size)))

(defn ->tinted-drawable
  "Creates a new drawable that renders the same as this drawable tinted the specified color."
  [drawable color]
  (.tint ^TextureRegionDrawable drawable color))

(defn bg-add!    [button-group button] (.add    ^ButtonGroup button-group ^Button button))
(defn bg-remove! [button-group button] (.remove ^ButtonGroup button-group ^Button button))
(defn bg-checked
  "The first checked button, or nil."
  [button-group]
  (.getChecked ^ButtonGroup button-group))

; FIXME t- already used for trees

(defn ->t-node ^Tree$Node [actor]
  (proxy [Tree$Node] [actor]))

(defn ->ui-tree [] (VisTree.))

; FIXME broken
(defn t-node-add! [^Tree$Node parent node] (.add parent node))

(defn- ->ui-stage
  "Stage implements clojure.lang.ILookup (get) on actor id."
  ^Stage [viewport batch]
  (proxy [Stage clojure.lang.ILookup] [viewport batch]
    (valAt
      ([id]
       (find-actor-with-id (.getRoot ^Stage this) id))
      ([id not-found]
       (or (find-actor-with-id (.getRoot ^Stage this) id) not-found)))))

(defn- s-act!   [^Stage s]   (.act      s))
(defn- s-draw!  [^Stage s]   (.draw     s))
(defn  s-root   [^Stage s]   (.getRoot  s))
(defn  s-clear! [^Stage s]   (.clear    s))
(defn  s-add!   [^Stage s a] (.addActor s a))
(defn- s-hit    [^Stage s [x y] & {:keys [touchable?]}]
  (.hit s x y (boolean touchable?)))

; TODO not disposed anymore... screens are sub-level.... look for dispose stuff also in @ cdq! FIXME
(defc :screens/stage
  {:let {:keys [stage sub-screen]}}
  (screen-enter [_]
    (set-input-processor! stage)
    (screen-enter sub-screen))

  (screen-exit [_]
    (set-input-processor! nil)
    (screen-exit sub-screen))

  (screen-render! [_]
    ; stage act first so user-screen calls change-screen -> is the end of frame
    ; otherwise would need render-after-stage
    ; or on change-screen the stage of the current screen would still .act
    (s-act! stage)
    (screen-render sub-screen)
    (s-draw! stage)))

(defn ->stage [actors]
  (let [stage (->ui-stage (:viewport gui-view) batch)]
    (run! #(s-add! stage %) actors)
    stage))

(defn stage-get []
  (:stage ((current-screen) 1)))

(defn mouse-on-actor? []
  (s-hit (stage-get) (gui-mouse-position) :touchable? true))

(defn stage-add! [actor]
  (s-add! (stage-get) actor))

(defn ->actor [{:keys [draw act]}]
  (->ui-actor (fn [] (when draw
                       (binding [*unit-scale* 1]
                         (draw))))
              (fn [] (when act
                       (act)))))

(def ^:private image-file "images/moon_background.png")

(defn ->background-image []
  (->image-widget (->image image-file)
                  {:fill-parent? true
                   :scaling :fill
                   :align :center}))

(defmacro ^:private with-err-str
  "Evaluates exprs in a context in which *err* is bound to a fresh
  StringWriter.  Returns the string created by any nested printing
  calls."
  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*err* s#]
       ~@body
       (str s#))))

(defn error-window! [throwable]
  (binding [*print-level* 5]
    (pretty-pst throwable 24))
  (stage-add! (->window {:title "Error"
                         :rows [[(->label (binding [*print-level* 3]
                                            (with-err-str
                                              (clojure.repl/pst throwable))))]]
                         :modal? true
                         :close-button? true
                         :close-on-escape? true
                         :center? true
                         :pack? true})))

; TODO no window movable type cursor appears here like in player idle
; inventory still working, other stuff not, because custom listener to keypresses ? use actor listeners?
; => input events handling
; hmmm interesting ... can disable @ item in cursor  / moving / etc.

(defn- show-player-modal! [{:keys [title text button-text on-click]}]
  (assert (not (::modal (stage-get))))
  (stage-add! (->window {:title title
                         :rows [[(->label text)]
                                [(->text-button button-text
                                                (fn []
                                                  (remove! (::modal (stage-get)))
                                                  (on-click)))]]
                         :id ::modal
                         :modal? true
                         :center-position [(/ (gui-viewport-width) 2)
                                           (* (gui-viewport-height) (/ 3 4))]
                         :pack? true})))

(defc :tx/player-modal
  (do! [[_ params]]
    (show-player-modal! params)
    nil))

(defn- lwjgl3-app-config
  [{:keys [title width height full-screen? fps]}]
  {:pre [title width height (boolean? full-screen?) (or (nil? fps) (int? fps))]}
  ; https://github.com/libgdx/libgdx/pull/7361
  ; Maybe can delete this when using that new libgdx version
  ; which includes this PR.
  (when SharedLibraryLoader/isMac
    (.set org.lwjgl.system.Configuration/GLFW_LIBRARY_NAME "glfw_async")
    (.set org.lwjgl.system.Configuration/GLFW_CHECK_THREAD0 false))
  (let [config (doto (Lwjgl3ApplicationConfiguration.)
                 (.setTitle title)
                 (.setForegroundFPS (or fps 60)))]
    (if full-screen?
      (.setFullscreenMode config (Lwjgl3ApplicationConfiguration/getDisplayMode))
      (.setWindowedMode config width height))
    config))

(defn start-app! [& {:keys [resources properties graphics screen-ks ui] :as config}]
  (load-properties-db! properties)
  (Lwjgl3Application. (proxy [ApplicationAdapter] []
                        (create []
                          (load-assets! resources)
                          (load-graphics! graphics)
                          (load-ui! ui)
                          (load-screens! screen-ks))

                        (dispose []
                          (dispose! assets)
                          (dispose-graphics!)
                          (dispose-ui!)
                          (dispose-screens!))

                        (render []
                          (ScreenUtils/clear black)
                          (screen-render! (current-screen)))

                        (resize [w h]
                          (vp-update! (gui-viewport) [w h] :center-camera? true)
                          (vp-update! (world-viewport) [w h])))
                      (lwjgl3-app-config config)))


(defn enemy-faction [{:keys [entity/faction]}]
  (case faction
    :evil :good
    :good :evil))

(defn friendly-faction [{:keys [entity/faction]}]
  faction)

(defc :entity/faction
  {:let faction
   :data [:enum [:good :evil]]}
  (info-text [_]
    (str "[SLATE]Faction: " (name faction) "[]")))

(defn entity-tile [entity*]
  (->tile (:position entity*)))

(declare world-paused?
         world-player)

(defn- set-arr [arr cell* cell*->blocked?]
  (let [[x y] (:position cell*)]
    (aset arr x y (boolean (cell*->blocked? cell*)))))

(defn- ->raycaster [grid position->blocked?]
  (let [width  (g/width  grid)
        height (g/height grid)
        arr (make-array Boolean/TYPE width height)]
    (doseq [cell (g/cells grid)]
      (set-arr arr @cell position->blocked?))
    (map->ArrRayCaster {:arr arr
                        :width width
                        :height height})))

; TO math.... // not tested
(defn- create-double-ray-endpositions
  "path-w in tiles."
  [[start-x start-y] [target-x target-y] path-w]
  {:pre [(< path-w 0.98)]} ; wieso 0.98??
  (let [path-w (+ path-w 0.02) ;etwas gr�sser damit z.b. projektil nicht an ecken anst�sst
        v (v-direction [start-x start-y]
                       [target-y target-y])
        [normal1 normal2] (v-get-normal-vectors v)
        normal1 (v-scale normal1 (/ path-w 2))
        normal2 (v-scale normal2 (/ path-w 2))
        start1  (v-add [start-x  start-y]  normal1)
        start2  (v-add [start-x  start-y]  normal2)
        target1 (v-add [target-x target-y] normal1)
        target2 (v-add [target-x target-y] normal2)]
    [start1,target1,start2,target2]))

(declare world-raycaster)

(defn init-world-raycaster! [grid position->blocked?]
  (bind-root #'world-raycaster (->raycaster grid position->blocked?)))

(defn ray-blocked? [start target]
  (fast-ray-blocked? world-raycaster start target))

(defn path-blocked?
  "path-w in tiles. casts two rays."
  [start target path-w]
  (let [[start1,target1,start2,target2] (create-double-ray-endpositions start target path-w)]
    (or
     (ray-blocked? start1 target1)
     (ray-blocked? start2 target2))))

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

; Assumption: The map contains no not-allowed diagonal cells, diagonal wall cells where both
; adjacent cells are walls and blocked.
; (important for wavefront-expansion and field-following)
; * entities do not move to NADs (they remove them)
; * the potential field flows into diagonals, so they should be reachable too.
;
; TODO assert @ mapload no NAD's and @ potential field init & remove from
; potential-field-following the removal of NAD's.

(def ^:private pf-cache (atom nil))

(def factions-iterations {:good 15 :evil 5})

(defn- cell-blocked? [cell*]
  (blocked? cell* :z-order/ground))

; FIXME assert @ mapload no NAD's and @ potential field init & remove from
; potential-field-following the removal of NAD's.

; TODO remove max pot field movement player screen + 10 tiles as of screen size
; => is coupled to max-steps & also
; to friendly units follow player distance

; TODO remove cached get adj cells & use grid as atom not cells ?
; how to compare perfr ?

; TODO visualize steps, maybe I see something I missed

(comment
 (defrecord Foo [a b c])

 (let [^Foo foo (->Foo 1 2 3)]
   (time (dotimes [_ 10000000] (:a foo)))
   (time (dotimes [_ 10000000] (.a foo)))
   ; .a 7x faster ! => use for faction/distance & make record?
   ))

(comment
 ; Stepping through manually
 (clear-marked-cells! :good (get @faction->marked-cells :good))

 (defn- faction->tiles->entities-map* [entities]
   (into {}
         (for [[faction entities] (->> entities
                                       (filter   #(:entity/faction @%))
                                       (group-by #(:entity/faction @%)))]
           [faction
            (zipmap (map #(entity-tile @%) entities)
                    entities)])))

 (def max-iterations 1)

 (let [entities (map db/get-entity [140 110 91])
       tl->es (:good (faction->tiles->entities-map* entities))]
   tl->es
   (def last-marked-cells (generate-potential-field :good tl->es)))
 (println *1)
 (def marked *2)
 (step :good *1)
 )

(defn- diagonal-direction? [[x y]]
  (and (not (zero? (float x)))
       (not (zero? (float y)))))

(defn- diagonal-cells? [cell* other-cell*]
  (let [[x1 y1] (:position cell*)
        [x2 y2] (:position other-cell*)]
    (and (not= x1 x2)
         (not= y1 y2))))

(defrecord FieldData [distance entity])

(defn- add-field-data! [cell faction distance entity]
  (swap! cell assoc faction (->FieldData distance entity)))

(defn- remove-field-data! [cell faction]
  (swap! cell assoc faction nil)) ; don't dissoc - will lose the Cell record type

; TODO performance
; * cached-adjacent-non-blocked-cells ? -> no need for cell blocked check?
; * sorted-set-by ?
; * do not refresh the potential-fields EVERY frame, maybe very 100ms & check for exists? target if they died inbetween.
; (or teleported?)
(defn- step [grid faction last-marked-cells]
  (let [marked-cells (transient [])
        distance       #(nearest-entity-distance % faction)
        nearest-entity #(nearest-entity          % faction)
        marked? faction]
    ; sorting important because of diagonal-cell values, flow from lower dist first for correct distance
    (doseq [cell (sort-by #(distance @%) last-marked-cells)
            adjacent-cell (cached-adjacent-cells grid cell)
            :let [cell* @cell
                  adjacent-cell* @adjacent-cell]
            :when (not (or (cell-blocked? adjacent-cell*)
                           (marked? adjacent-cell*)))
            :let [distance-value (+ (float (distance cell*))
                                    (float (if (diagonal-cells? cell* adjacent-cell*)
                                             1.4 ; square root of 2 * 10
                                             1)))]]
      (add-field-data! adjacent-cell faction distance-value (nearest-entity cell*))
      (conj! marked-cells adjacent-cell))
    (persistent! marked-cells)))

(defn- generate-potential-field
  "returns the marked-cells"
  [grid faction tiles->entities max-iterations]
  (let [entity-cell-seq (for [[tile entity] tiles->entities] ; FIXME lazy seq
                          [entity (get grid tile)])
        marked (map second entity-cell-seq)]
    (doseq [[entity cell] entity-cell-seq]
      (add-field-data! cell faction 0 entity))
    (loop [marked-cells     marked
           new-marked-cells marked
           iterations 0]
      (if (= iterations max-iterations)
        marked-cells
        (let [new-marked (step grid faction new-marked-cells)]
          (recur (concat marked-cells new-marked) ; FIXME lazy seq
                 new-marked
                 (inc iterations)))))))

(defn- tiles->entities [entities faction]
  (let [entities (filter #(= (:entity/faction @%) faction)
                         entities)]
    (zipmap (map #(entity-tile @%) entities)
            entities)))

(defn- update-faction-potential-field [grid faction entities max-iterations]
  (let [tiles->entities (tiles->entities entities faction)
        last-state   [faction :tiles->entities]
        marked-cells [faction :marked-cells]]
    (when-not (= (get-in @pf-cache last-state) tiles->entities)
      (swap! pf-cache assoc-in last-state tiles->entities)
      (doseq [cell (get-in @pf-cache marked-cells)]
        (remove-field-data! cell faction))
      (swap! pf-cache assoc-in marked-cells (generate-potential-field
                                             grid
                                             faction
                                             tiles->entities
                                             max-iterations)))))

;; MOVEMENT AI

(defn- indexed ; from clojure.contrib.seq-utils (discontinued in 1.3)
  "Returns a lazy sequence of [index, item] pairs, where items come
 from 's' and indexes count up from zero.

 (indexed '(a b c d)) => ([0 a] [1 b] [2 c] [3 d])"
  [s]
  (map vector (iterate inc 0) s))

(defn- utils-positions ; from clojure.contrib.seq-utils (discontinued in 1.3)
  "Returns a lazy sequence containing the positions at which pred
	 is true for items in coll."
  [pred coll]
  (for [[idx elt] (indexed coll) :when (pred elt)] idx))

(let [order (g/get-8-neighbour-positions [0 0])]
  (def ^:private diagonal-check-indizes
    (into {} (for [[x y] (filter diagonal-direction? order)]
               [(first (utils-positions #(= % [x y]) order))
                (vec (utils-positions #(some #{%} [[x 0] [0 y]])
                                     order))]))))

(defn- is-not-allowed-diagonal? [at-idx adjacent-cells]
  (when-let [[a b] (get diagonal-check-indizes at-idx)]
    (and (nil? (adjacent-cells a))
         (nil? (adjacent-cells b)))))

(defn- remove-not-allowed-diagonals [adjacent-cells]
  (remove nil?
          (map-indexed
            (fn [idx cell]
              (when-not (or (nil? cell)
                            (is-not-allowed-diagonal? idx adjacent-cells))
                cell))
            adjacent-cells)))

; not using filter because nil cells considered @ remove-not-allowed-diagonals
; TODO only non-nil cells check
; TODO always called with cached-adjacent-cells ...
(defn- filter-viable-cells [entity adjacent-cells]
  (remove-not-allowed-diagonals
    (mapv #(when-not (or (cell-blocked? @%)
                         (occupied-by-other? @% entity))
             %)
          adjacent-cells)))

(defmacro ^:private when-seq [[aseq bind] & body]
  `(let [~aseq ~bind]
     (when (seq ~aseq)
       ~@body)))

(defn- get-min-dist-cell [distance-to cells]
  (when-seq [cells (filter distance-to cells)]
    (apply min-key distance-to cells)))

; rarely called -> no performance bottleneck
(defn- viable-cell? [grid distance-to own-dist entity cell]
  (when-let [best-cell (get-min-dist-cell
                        distance-to
                        (filter-viable-cells entity (cached-adjacent-cells grid cell)))]
    (when (< (float (distance-to best-cell)) (float own-dist))
      cell)))

(defn- find-next-cell
  "returns {:target-entity entity} or {:target-cell cell}. Cell can be nil."
  [grid entity own-cell]
  (let [faction (enemy-faction @entity)
        distance-to    #(nearest-entity-distance @% faction)
        nearest-entity #(nearest-entity          @% faction)
        own-dist (distance-to own-cell)
        adjacent-cells (cached-adjacent-cells grid own-cell)]
    (if (and own-dist (zero? (float own-dist)))
      {:target-entity (nearest-entity own-cell)}
      (if-let [adjacent-cell (first (filter #(and (distance-to %)
                                                  (zero? (float (distance-to %))))
                                            adjacent-cells))]
        {:target-entity (nearest-entity adjacent-cell)}
        {:target-cell (let [cells (filter-viable-cells entity adjacent-cells)
                            min-key-cell (get-min-dist-cell distance-to cells)]
                        (cond
                         (not min-key-cell)  ; red
                         own-cell

                         (not own-dist)
                         min-key-cell

                         (> (float (distance-to min-key-cell)) (float own-dist)) ; red
                         own-cell

                         (< (float (distance-to min-key-cell)) (float own-dist)) ; green
                         min-key-cell

                         (= (distance-to min-key-cell) own-dist) ; yellow
                         (or
                          (some #(viable-cell? grid distance-to own-dist entity %) cells)
                          own-cell)))}))))

(defn- inside-cell? [grid entity* cell]
  (let [cells (rectangle->cells grid entity*)]
    (and (= 1 (count cells))
         (= cell (first cells)))))

; TODO work with entity* !? occupied-by-other? works with entity not entity* ... not with ids ... hmmm
(defn potential-fields-follow-to-enemy [entity] ; TODO pass faction here, one less dependency.
  (let [grid world-grid
        position (:position @entity)
        own-cell (get grid (->tile position))
        {:keys [target-entity target-cell]} (find-next-cell grid entity own-cell)]
    (cond
     target-entity
     (v-direction position (:position @target-entity))

     (nil? target-cell)
     nil

     :else
     (when-not (and (= target-cell own-cell)
                    (occupied-by-other? @own-cell entity)) ; prevent friction 2 move to center
       (when-not (inside-cell? grid @entity target-cell)
         (v-direction position (:middle @target-cell)))))))

(defn potential-fields-update! [entities]
  (doseq [[faction max-iterations] factions-iterations]
    (update-faction-potential-field world-grid faction entities max-iterations)))

;; DEBUG RENDER TODO not working in old map debug cdq.maps.render_

; -> render on-screen tile stuff
; -> I just use render-on-map and use tile coords
; -> I need the current viewed tiles x,y,w,h

#_(let [a 0.5]
  (color/defrgb transp-red 1 0 0 a)
  (color/defrgb transp-green 0 1 0 a)
  (color/defrgb transp-orange 1 0.34 0 a)
  (color/defrgb transp-yellow 1 1 0 a))

#_(def ^:private adjacent-cells-colors (atom nil))

#_(defn genmap
    "function is applied for every key to get value. use memoize instead?"
    [ks f]
    (zipmap ks (map f ks)))

#_(defn calculate-mouseover-body-colors [mouseoverbody]
  (when-let [body mouseoverbody]
    (let [occupied-cell (get world-grid (entity-tile @body))
          own-dist (distance-to occupied-cell)
          adj-cells (cached-adjacent-cells grid occupied-cell)
          potential-cells (filter distance-to
                                  (filter-viable-cells body adj-cells))
          adj-cells (remove nil? adj-cells)]
      (reset! adjacent-cells-colors
        (genmap adj-cells
          (fn [cell]
            (cond
              (not-any? #{cell} potential-cells)
              transp-red

              (not own-dist) ; die andre hat eine dist da sonst potential-cells rausgefiltert -> besser als jetzige cell.
              transp-green

              (< own-dist (distance-to cell))
              transp-red

              (= own-dist (distance-to cell))
              transp-yellow

              :else transp-green)))))))

#_(defn render-potential-field-following-mouseover-info
    [leftx topy xrect yrect cell mouseoverbody]
    (when-let [body mouseoverbody]
      (when-let [color (get @adjacent-cells-colors cell)]
        (shape-drawer/filled-rectangle leftx topy 1 1 color)))) ; FIXME scale ok for map rendering?

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
  (active-entities* @world-player))

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

(defn init-world-time! []
  (bind-root #'elapsed-time 0)
  (bind-root #'logic-frame 0))

(defn update-time [delta]
  (bind-root #'world-delta delta)
  (alter-var-root #'elapsed-time + delta)
  (alter-var-root #'logic-frame inc))

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

(defn- define-order [order-k-vector]
  (apply hash-map (interleave order-k-vector (range))))

(defn sort-by-order [coll get-item-order-k order]
  (sort-by #((get-item-order-k %) order) < coll))

#_(defn order-contains? [order k]
  ((apply hash-set (keys order)) k))

#_(deftest test-order
  (is
    (= (define-order [:a :b :c]) {:a 0 :b 1 :c 2}))
  (is
    (order-contains? (define-order [:a :b :c]) :a))
  (is
    (not
      (order-contains? (define-order [:a :b :c]) 2)))
  (is
    (=
      (sort-by-order [:c :b :a :b] identity (define-order [:a :b :c]))
      '(:a :b :b :c)))
  (is
    (=
      (sort-by-order [:b :c :null :null :a] identity (define-order [:c :b :a :null]))
      '(:c :b :a :null :null))))

;;;; ?

; java.lang.IllegalArgumentException: No method in multimethod 'render-info' for dispatch value: :position
; actually we dont want this to be called over that
; it should be :components? then ?
; => shouldn't need default fns for render -> don't call it if its not there

; every component has parent-entity-id (peid)
; fetch active entity-ids
; then fetch all components which implement render-below
; and have parent-id in entity-ids, etc.

;;;; Body

; so that at low fps the game doesn't jump faster between frames used @ movement to set a max speed so entities don't jump over other entities when checking collisions
(def max-delta-time 0.04)

; setting a min-size for colliding bodies so movement can set a max-speed for not
; skipping bodies at too fast movement
; TODO assert at properties load
(def ^:private min-solid-body-size 0.39) ; == spider smallest creature size.

; set max speed so small entities are not skipped by projectiles
; could set faster than max-speed if I just do multiple smaller movement steps in one frame
(def ^:private max-speed (/ min-solid-body-size max-delta-time)) ; need to make var because m/schema would fail later if divide / is inside the schema-form
(def movement-speed-schema (m/schema [:and number? [:>= 0] [:<= max-speed]]))

(def hpbar-height-px 5)

(def ^:private z-orders [:z-order/on-ground
                         :z-order/ground
                         :z-order/flying
                         :z-order/effect])

(def render-order (define-order z-orders))

(defrecord Entity [position
                   left-bottom
                   width
                   height
                   half-width
                   half-height
                   radius
                   collides?
                   z-order
                   rotation-angle])

(defn- ->Body [{[x y] :position
                :keys [position
                       width
                       height
                       collides?
                       z-order
                       rotation-angle]}]
  (assert position)
  (assert width)
  (assert height)
  (assert (>= width  (if collides? min-solid-body-size 0)))
  (assert (>= height (if collides? min-solid-body-size 0)))
  (assert (or (boolean? collides?) (nil? collides?)))
  (assert ((set z-orders) z-order))
  (assert (or (nil? rotation-angle)
              (<= 0 rotation-angle 360)))
  (map->Entity
   {:position (mapv float position)
    :left-bottom [(float (- x (/ width  2)))
                  (float (- y (/ height 2)))]
    :width  (float width)
    :height (float height)
    :half-width  (float (/ width  2))
    :half-height (float (/ height 2))
    :radius (float (max (/ width  2)
                        (/ height 2)))
    :collides? collides?
    :z-order z-order
    :rotation-angle (or rotation-angle 0)}))

(def ^{:doc "For effects just to have a mouseover body size for debugging purposes."}
  effect-body-props
  {:width 0.5
   :height 0.5
   :z-order :z-order/effect})

(defn direction [entity* other-entity*]
  (v-direction (:position entity*) (:position other-entity*)))

(defn collides? [entity* other-entity*]
  (shape-collides? entity* other-entity*))

;;;; ?

(defprotocol State
  (entity-state [_])
  (state-obj [_]))

(defprotocol Inventory
  (can-pickup-item? [_ item]))

(defprotocol Stats
  (entity-stat [_ stat] "Calculating value of the stat w. modifiers"))

(defprotocol Modifiers
  (->modified-value [_ modifier-k base-value]))

;;;; line-of-sight

; does not take into account zoom - but zoom is only for debug ???
; vision range?
(defn- on-screen? [entity*]
  (let [[x y] (:position entity*)
        x (float x)
        y (float y)
        [cx cy] (camera-position (world-camera))
        px (float cx)
        py (float cy)
        xdist (Math/abs (- x px))
        ydist (Math/abs (- y py))]
    (and
     (<= xdist (inc (/ (float (world-viewport-width))  2)))
     (<= ydist (inc (/ (float (world-viewport-height)) 2))))))

; TODO at wrong point , this affects targeting logic of npcs
; move the debug flag to either render or mouseover or lets see
(def ^:private ^:dbg-flag los-checks? true)

; does not take into account size of entity ...
; => assert bodies <1 width then
(defn line-of-sight? [source* target*]
  (and (or (not (:entity/player? source*))
           (on-screen? target*))
       (not (and los-checks?
                 (ray-blocked? (:position source*) (:position target*))))))

(defsystem create "Create entity with eid for txs side-effects. Default nil." [_ entity])
(defmethod create :default [_ entity])

(defsystem destroy "FIXME" [_ entity])
(defmethod destroy :default [_ entity])

(defsystem tick "FIXME" [_ entity])
(defmethod tick :default [_ entity])

(defsystem render-below "FIXME" [_ entity*])
(defmethod render-below :default [_ entity*])

(defsystem render "FIXME" [_ entity*])
(defmethod render :default [_ entity*])

(defsystem render-above "FIXME" [_ entity*])
(defmethod render-above :default [_ entity*])

(defsystem render-info "FIXME" [_ entity*])
(defmethod render-info :default [_ entity*])

(def ^:private render-systems [render-below
                               render
                               render-above
                               render-info])

(declare ^:private uids-entities)

(defn init-uids-entities! []
  (bind-root #'uids-entities {}))

(defn all-entities [] (vals uids-entities))

(defn get-entity
  "Mostly used for debugging, use an entity's atom for (probably) faster access in your logic."
  [uid]
  (get uids-entities uid))

(defc :entity/id
  (create  [[_ id] _eid] [[:tx/add-to-world      id]])
  (destroy [[_ id] _eid] [[:tx/remove-from-world id]]))

(defc :entity/uid
  {:let uid}
  (create [_ entity]
    (assert (number? uid))
    (alter-var-root #'uids-entities assoc uid entity)
    nil)

  (destroy [_ _entity]
    (assert (contains? uids-entities uid))
    (alter-var-root #'uids-entities dissoc uid)
    nil))

(let [cnt (atom 0)]
  (defn- unique-number! []
    (swap! cnt inc)))

(defn- create-e-system [eid]
  (for [component @eid]
    (fn []
      ; we are assuming components dont remove other ones at entity/create
      ; thats why we reuse component and not fetch each time again for key
      (create component eid))))

(defc :e/create
  (do! [[_ position body components]]
    (assert (and (not (contains? components :position))
                 (not (contains? components :entity/id))
                 (not (contains? components :entity/uid))))
    (let [eid (atom nil)]
      (reset! eid (-> body
                      (assoc :position position)
                      ->Body
                      (safe-merge (-> components
                                      (assoc :entity/id eid
                                             :entity/uid (unique-number!))
                                      (create-vs)))))
      (create-e-system eid))))

(defc :e/destroy
  (do! [[_ entity]]
    [[:e/assoc entity :entity/destroyed? true]]))

(defc :e/assoc
  (do! [[_ entity k v]]
    (assert (keyword? k))
    (swap! entity assoc k v)
    nil))

(defc :e/assoc-in
  (do! [[_ entity ks v]]
    (swap! entity assoc-in ks v)
    nil))

(defc :e/dissoc
  (do! [[_ entity k]]
    (assert (keyword? k))
    (swap! entity dissoc k)
    nil))

(defc :e/dissoc-in
  (do! [[_ entity ks]]
    (assert (> (count ks) 1))
    (swap! entity update-in (drop-last ks) dissoc (last ks))
    nil))

(defc :e/update-in
  (do! [[_ entity ks f]]
    (swap! entity update-in ks f)
    nil))

(def ^:private ^:dbg-flag show-body-bounds false)

(defn- draw-body-rect [entity* color]
  (let [[x y] (:left-bottom entity*)]
    (draw-rectangle x y (:width entity*) (:height entity*) color)))

(defn- render-entity* [system entity*]
  (try
   (when show-body-bounds
     (draw-body-rect entity* (if (:collides? entity*) :white :gray)))
   (run! #(system % entity*) entity*)
   (catch Throwable t
     (draw-body-rect entity* :red)
     (pretty-pst t 12))))

; precaution in case a component gets removed by another component
; the question is do we still want to update nil components ?
; should be contains? check ?
; but then the 'order' is important? in such case dependent components
; should be moved together?
(defn- tick-system [entity]
  (try
   (doseq [k (keys @entity)]
     (when-let [v (k @entity)]
       (effect! (tick [k v] entity))))
   (catch Throwable t
     (throw (ex-info "" (select-keys @entity [:entity/uid]) t)))))

(defn tick-entities!
  "Calls tick system on all components of entities."
  [entities]
  (run! tick-system entities))

(defn render-entities!
  "Draws entities* in the correct z-order and in the order of render-systems for each z-order."
  [entities*]
  (let [player-entity* @world-player]
    (doseq [[z-order entities*] (sort-by-order (group-by :z-order entities*)
                                               first
                                               render-order)
            system render-systems
            entity* entities*
            :when (or (= z-order :z-order/effect)
                      (line-of-sight? player-entity* entity*))]
      (render-entity* system entity*))))

(defn remove-destroyed-entities!
  "Calls destroy on all entities which are marked with ':e/destroy'"
  []
  (for [entity (filter (comp :entity/destroyed? deref) (all-entities))
        component @entity]
    (fn []
      (destroy component entity))))

(defc :entity/image
  {:data :image
   :let image}
  (render [_ entity*]
    (draw-rotated-centered-image image
                                 (or (:rotation-angle entity*) 0)
                                 (:position entity*))))

(defprotocol Animation
  (^:private anim-tick [_ delta])
  (^:private restart [_])
  (^:private anim-stopped? [_])
  (^:private current-frame [_]))

(defrecord ImmutableAnimation [frames frame-duration looping? cnt maxcnt]
  Animation
  (anim-tick [this delta]
    (let [maxcnt (float maxcnt)
          newcnt (+ (float cnt) (float delta))]
      (assoc this :cnt (cond (< newcnt maxcnt) newcnt
                             looping? (min maxcnt (- newcnt maxcnt))
                             :else maxcnt))))

  (restart [this]
    (assoc this :cnt 0))

  (anim-stopped? [_]
    (and (not looping?) (>= cnt maxcnt)))

  (current-frame [this]
    (frames (min (int (/ (float cnt) (float frame-duration)))
                 (dec (count frames))))))

(defn- ->animation [frames & {:keys [frame-duration looping?]}]
  (map->ImmutableAnimation
    {:frames (vec frames)
     :frame-duration frame-duration
     :looping? looping?
     :cnt 0
     :maxcnt (* (count frames) (float frame-duration))}))

(defn- edn->animation [{:keys [frames frame-duration looping?]}]
  (->animation (map edn->image frames)
               :frame-duration frame-duration
               :looping? looping?))


(defmethod edn->value :data/animation [_ animation]
  (edn->animation animation))

(defn- tx-assoc-image-current-frame [eid animation]
  [:e/assoc eid :entity/image (current-frame animation)])

(defc :entity/animation
  {:data :data/animation
   :let animation}
  (create [_ eid]
    [(tx-assoc-image-current-frame eid animation)])

  (tick [[k _] eid]
    [(tx-assoc-image-current-frame eid animation)
     [:e/assoc eid k (anim-tick animation world-delta)]]))

(defc :entity/delete-after-animation-stopped?
  (create [_ entity]
    (-> @entity :entity/animation :looping? not assert))

  (tick [_ entity]
    (when (anim-stopped? (:entity/animation @entity))
      [[:e/destroy entity]])))

(def-property-type :properties/audiovisuals
  {:schema [:tx/sound
            :entity/animation]
   :overview {:title "Audiovisuals"
              :columns 10
              :image/scale 2}})

(defc :tx/audiovisual
  (do! [[_ position id]]
    (let [{:keys [tx/sound
                  entity/animation]} (build-property id)]
      [[:tx/sound sound]
       [:e/create
        position
        effect-body-props
        {:entity/animation animation
         :entity/delete-after-animation-stopped? true}]])))

(defn- move-position [position {:keys [direction speed delta-time]}]
  (mapv #(+ %1 (* %2 speed delta-time)) position direction))

(defn- move-body [body movement]
  (-> body
      (update :position    move-position movement)
      (update :left-bottom move-position movement)))

(defn- valid-position? [{:keys [entity/id z-order] :as body}]
  {:pre [(:collides? body)]}
  (let [cells* (into [] (map deref) (rectangle->cells world-grid body))]
    (and (not-any? #(blocked? % z-order) cells*)
         (->> cells*
              cells->entities
              (not-any? (fn [other-entity]
                          (let [other-entity* @other-entity]
                            (and (not= (:entity/id other-entity*) id)
                                 (:collides? other-entity*)
                                 (collides? other-entity* body)))))))))

(defn- try-move [body movement]
  (let [new-body (move-body body movement)]
    (when (valid-position? new-body)
      new-body)))

; TODO sliding threshold
; TODO name - with-sliding? 'on'
; TODO if direction was [-1 0] and invalid-position then this algorithm tried to move with
; direection [0 0] which is a waste of processor power...
(defn- try-move-solid-body [body {[vx vy] :direction :as movement}]
  (let [xdir (Math/signum (float vx))
        ydir (Math/signum (float vy))]
    (or (try-move body movement)
        (try-move body (assoc movement :direction [xdir 0]))
        (try-move body (assoc movement :direction [0 ydir])))))

(defc :entity/movement
  {:let {:keys [direction speed rotate-in-movement-direction?] :as movement}}
  (tick [_ eid]
    (assert (m/validate movement-speed-schema speed))
    (assert (or (zero? (v-length direction))
                (v-normalised? direction)))
    (when-not (or (zero? (v-length direction))
                  (nil? speed)
                  (zero? speed))
      (let [movement (assoc movement :delta-time world-delta)
            body @eid]
        (when-let [body (if (:collides? body) ; < == means this is a movement-type ... which could be a multimethod ....
                          (try-move-solid-body body movement)
                          (move-body body movement))]
          [[:e/assoc eid :position    (:position    body)]
           [:e/assoc eid :left-bottom (:left-bottom body)]
           (when rotate-in-movement-direction?
             [:e/assoc eid :rotation-angle (v-get-angle-from-vector direction)])
           [:tx/position-changed eid]])))))

(defc :tx/set-movement
  (do! [[_ entity movement]]
    (assert (or (nil? movement)
                (nil? (:direction movement))
                (and (:direction movement) ; continue schema of that ...
                     #_(:speed movement)))) ; princess no stats/movement-speed, then nil and here assertion-error
    [(if (or (nil? movement)
             (nil? (:direction movement)))
       [:e/dissoc entity :entity/movement]
       [:e/assoc entity :entity/movement movement])]))

(defc :entity/delete-after-duration
  {:let counter}
  (->mk [[_ duration]]
    (->counter duration))

  (info-text [_]
    (str "[LIGHT_GRAY]Remaining: " (readable-number (finished-ratio counter)) "/1[]"))

  (tick [_ eid]
    (when (stopped? counter)
      [[:e/destroy eid]])))

(defc :entity/destroy-audiovisual
  {:let audiovisuals-id}
  (destroy [_ entity]
    [[:tx/audiovisual (:position @entity) audiovisuals-id]]))

(defc :entity/line-render
  {:let {:keys [thick? end color]}}
  (render [_ entity*]
    (let [position (:position entity*)]
      (if thick?
        (with-shape-line-width 4 #(draw-line position end color))
        (draw-line position end color)))))

(defc :tx/line-render
  (do! [[_ {:keys [start end duration color thick?]}]]
    [[:e/create
      start
      effect-body-props
      #:entity {:line-render {:thick? thick? :end end :color color}
                :delete-after-duration duration}]]))

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

(defsystem clicked-skillmenu-skill "FIXME" [_ skill])
(defmethod clicked-skillmenu-skill :default [_ skill])

(defn- player-clicked-skillmenu [skill]
  (clicked-skillmenu-skill (state-obj @world-player) skill))

; TODO render text label free-skill-points
; (str "Free points: " (:entity/free-skill-points @world-player))
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

(defc :entity/clickable
  (render [[_ {:keys [text]}]
           {:keys [entity/mouseover?] :as entity*}]
    (when (and mouseover? text)
      (let [[x y] (:position entity*)]
        (draw-text {:text text
                    :x x
                    :y (+ y (:half-height entity*))
                    :up? true})))))

(def ^:private outline-alpha 0.4)
(def ^:private enemy-color    [1 0 0 outline-alpha])
(def ^:private friendly-color [0 1 0 outline-alpha])
(def ^:private neutral-color  [1 1 1 outline-alpha])

(defc :entity/mouseover?
  (render-below [_ {:keys [entity/faction] :as entity*}]
    (let [player-entity* @world-player]
      (with-shape-line-width 3
        #(draw-ellipse (:position entity*)
                       (:half-width entity*)
                       (:half-height entity*)
                       (cond (= faction (enemy-faction player-entity*))
                             enemy-color
                             (= faction (friendly-faction player-entity*))
                             friendly-color
                             :else
                             neutral-color))))))

(def ^:private shout-radius 4)

(defn- friendlies-in-radius [position faction]
  (->> {:position position
        :radius shout-radius}
       (circle->entities world-grid)
       (map deref)
       (filter #(= (:entity/faction %) faction))
       (map :entity/id)))

(defc :entity/alert-friendlies-after-duration
  {:let {:keys [counter faction]}}
  (tick [_ eid]
    (when (stopped? counter)
      (cons [:e/destroy eid]
            (for [friendly-eid (friendlies-in-radius (:position @eid) faction)]
              [:tx/event friendly-eid :alert])))))

(defc :tx/shout
  (do! [[_ position faction delay-seconds]]
    [[:e/create
      position
      effect-body-props
      {:entity/alert-friendlies-after-duration
       {:counter (->counter delay-seconds)
        :faction faction}}]]))

(defc :entity/string-effect
  (tick [[k {:keys [counter]}] eid]
    (when (stopped? counter)
      [[:e/dissoc eid k]]))

  (render-above [[_ {:keys [text]}] entity*]
    (let [[x y] (:position entity*)]
      (draw-text {:text text
                  :x x
                  :y (+ y (:half-height entity*) (pixels->world-units hpbar-height-px))
                  :scale 2
                  :up? true}))))

(defc :tx/add-text-effect
  (do! [[_ entity text]]
    [[:e/assoc
      entity
      :entity/string-effect
      (if-let [string-effect (:entity/string-effect @entity)]
        (-> string-effect
            (update :text str "\n" text)
            (update :counter reset))
        {:text text
         :counter (->counter 0.4)})]]))

(defn- txs-update-modifiers [entity modifiers f]
  (for [[modifier-k operations] modifiers
        [operation-k value] operations]
    [:e/update-in entity [:entity/modifiers modifier-k operation-k] (f value)]))

(defn- conj-value [value]
  (fn [values]
    (conj values value)))

(defn- remove-one [coll item]
  (let [[n m] (split-with (partial not= item) coll)]
    (concat n (rest m))))

(defn- remove-value [value]
  (fn [values]
    {:post [(= (count %) (dec (count values)))]}
    (remove-one values value)))

(defc :tx/apply-modifiers
  (do! [[_ entity modifiers]]
    (txs-update-modifiers entity modifiers conj-value)))

(defc :tx/reverse-modifiers
  (do! [[_ entity modifiers]]
    (txs-update-modifiers entity modifiers remove-value)))

(comment
 (= (txs-update-modifiers :entity
                         {:modifier/hp {:op/max-inc 5
                                        :op/max-mult 0.3}
                          :modifier/movement-speed {:op/mult 0.1}}
                         (fn [_value] :fn))
    [[:e/update-in :entity [:entity/modifiers :modifier/hp :op/max-inc] :fn]
     [:e/update-in :entity [:entity/modifiers :modifier/hp :op/max-mult] :fn]
     [:e/update-in :entity [:entity/modifiers :modifier/movement-speed :op/mult] :fn]])
 )

; DRY ->effective-value (summing)
; also: sort-by op/order @ modifier/info-text itself (so player will see applied order)
(defn- sum-operation-values [modifiers]
  (for [[modifier-k operations] modifiers
        :let [operations (for [[operation-k values] operations
                               :let [value (apply + values)]
                               :when (not (zero? value))]
                           [operation-k value])]
        :when (seq operations)]
    [modifier-k operations]))

(def-markup-color "MODIFIER_BLUE" :cyan)

; For now no green/red color for positive/negative numbers
; as :stats/damage-receive negative value would be red but actually a useful buff
; -> could give damage reduce 10% like in diablo 2
; and then make it negative .... @ applicator
(def ^:private positive-modifier-color "[MODIFIER_BLUE]" #_"[LIME]")
(def ^:private negative-modifier-color "[MODIFIER_BLUE]" #_"[SCARLET]")

(defn k->pretty-name [k]
  (str/capitalize (name k)))

(defn mod-info-text [modifiers]
  (str "[MODIFIER_BLUE]"
       (str/join "\n"
                 (for [[modifier-k operations] modifiers
                       operation operations]
                   (str (op-info-text operation) " " (k->pretty-name modifier-k))))
       "[]"))

(defc :entity/modifiers
  {:data [:components-ns :modifier]
   :let modifiers}
  (->mk [_]
    (into {} (for [[modifier-k operations] modifiers]
               [modifier-k (into {} (for [[operation-k value] operations]
                                      [operation-k [value]]))])))

  (info-text [_]
    (let [modifiers (sum-operation-values modifiers)]
      (when (seq modifiers)
        (mod-info-text modifiers)))))

(extend-type clojure.gdx.Entity
  Modifiers
  (->modified-value [{:keys [entity/modifiers]} modifier-k base-value]
    {:pre [(= "modifier" (namespace modifier-k))]}
    (->> modifiers
         modifier-k
         (sort-by op-order)
         (reduce (fn [base-value [operation-k values]]
                   (op-apply [operation-k (apply + values)] base-value))
                 base-value))))

(comment

 (let [->entity (fn [modifiers]
                  (map->Entity {:entity/modifiers modifiers}))]
   (and
    (= (->modified-value (->entity {:modifier/damage-deal {:op/val-inc [30]
                                                           :op/val-mult [0.5]}})
                         :modifier/damage-deal
                         [5 10])
       [52 52])
    (= (->modified-value (->entity {:modifier/damage-deal {:op/val-inc [30]}
                                    :stats/fooz-barz {:op/babu [1 2 3]}})
                         :modifier/damage-deal
                         [5 10])
       [35 35])
    (= (->modified-value (map->Entity {})
                         :modifier/damage-deal
                         [5 10])
       [5 10])
    (= (->modified-value (->entity {:modifier/hp {:op/max-inc [10 1]
                                                  :op/max-mult [0.5]}})
                         :modifier/hp
                         [100 100])
       [100 166])
    (= (->modified-value (->entity {:modifier/movement-speed {:op/inc [2]
                                                              :op/mult [0.1 0.2]}})
                         :modifier/movement-speed
                         3)
       6.5)))
 )

(def-markup-color "ITEM_GOLD" [0.84 0.8 0.52])

(defc :property/pretty-name
  {:data :string
   :let value}
  (info-text [_]
    (str "[ITEM_GOLD]"value"[]")))

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
       (draw-cell-rect @world-player
                       (actor-x this)
                       (actor-y this)
                       (a-mouseover? this (gui-mouse-position))
                       (actor-id (parent this)))))))

(defsystem clicked-inventory-cell "FIXME" [_ cell])
(defmethod clicked-inventory-cell :default [_ cell])

(defn- player-clicked-inventory [cell]
  (clicked-inventory-cell (state-obj @world-player) cell))

(defn- ->cell [slot->background slot & {:keys [position]}]
  (let [cell [slot (or position [0 0])]
        image-widget (->ui-image-widget (slot->background slot) {:id :image})
        stack (->stack [(draw-rect-actor) image-widget])]
    (set-name! stack "inventory-cell")
    (set-id! stack cell)
    (add-listener! stack (proxy [ClickListener] []
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

(defsystem enter "FIXME" [_])
(defmethod enter :default [_])

(defsystem exit  "FIXME" [_])
(defmethod exit :default  [_])

(defsystem player-enter "FIXME" [_])
(defmethod player-enter :default [_])

(defsystem pause-game? "FIXME" [_])
(defmethod pause-game? :default [_])

(defsystem manual-tick "FIXME" [_])
(defmethod manual-tick :default [_])

(defn- k->widget [k]
  (cond
   (#{:map-optional :components-ns} k) :map
   (#{:number :nat-int :int :pos :pos-int :val-max} k) :number
   :else k))

(defmulti ^:private ->widget      (fn [[k _] _v] (k->widget k)))
(defmulti ^:private widget->value (fn [[k _] _widget] (k->widget k)))

;;;;

; looping? - click on widget restart
; frame-duration
; frames ....
; hidden actor act tick atom animation & set current frame image drawable
(defmethod ->widget :data/animation [_ animation]
  (->table {:rows [(for [image (:frames animation)]
                        (->image-widget (edn->image image) {}))]
               :cell-defaults {:pad 1}}))

;;;;

(defn- add-schema-tooltip! [widget data]
  (add-tooltip! widget (str "Schema: " (pr-str (m/form (:schema data)))))
  widget)

(defn- ->edn-str [v]
  (binding [*print-level* nil]
    (pr-str v)))

(defmethod ->widget :boolean [_ checked?]
  (assert (boolean? checked?))
  (->check-box "" (fn [_]) checked?))

(defmethod widget->value :boolean [_ widget]
  (.isChecked ^com.kotcrab.vis.ui.widget.VisCheckBox widget))

(defmethod ->widget :string [[_ data] v]
  (add-schema-tooltip! (->text-field v {})
                       data))

(defmethod widget->value :string [_ widget]
  (.getText ^com.kotcrab.vis.ui.widget.VisTextField widget))

(defmethod ->widget :number [[_ data] v]
  (add-schema-tooltip! (->text-field (->edn-str v) {})
                       data))

(defmethod widget->value :number [_ widget]
  (edn/read-string (.getText ^com.kotcrab.vis.ui.widget.VisTextField widget)))

(defmethod ->widget :enum [[_ data] v]
  (->select-box {:items (map ->edn-str (rest (:schema data)))
                    :selected (->edn-str v)}))

(defmethod widget->value :enum [_ widget]
  (edn/read-string (.getSelected ^com.kotcrab.vis.ui.widget.VisSelectBox widget)))

(defmethod edn->value :image [_ image]
  (edn->image image))

; too many ! too big ! scroll ... only show files first & preview?
; make tree view from folders, etc. .. !! all creatures animations showing...
(defn- texture-rows []
  (for [file (sort all-texture-files)]
    [(->image-button (prop->image file) (fn []))]
    #_[(->text-button file (fn []))]))

(defmethod ->widget :image [_ image]
  (->image-widget (edn->image image) {})
  #_(->image-button image
                       #(stage-add! (->scrollable-choose-window (texture-rows)))
                       {:dimensions [96 96]})) ; x2  , not hardcoded here

; TODO set to preferred width/height ??? why layouting not working properly?
; use a tree?
; make example with plain data
(defn ->scroll-pane-cell [rows]
  (let [table (->table {:rows rows :cell-defaults {:pad 1} :pack? true})
        scroll-pane (->scroll-pane table)]
    {:actor scroll-pane
     :width  (- (gui-viewport-width)  600)    ; (+ (actor/width table) 200)
     :height (- (gui-viewport-height) 100)})) ; (min (- (gui-viewport-height) 50) (actor/height table))

(defn- ->scrollable-choose-window [rows]
  (->window {:title "Choose"
             :modal? true
             :close-button? true
             :center? true
             :close-on-escape? true
             :rows [[(->scroll-pane-cell rows)]]
             :pack? true}))

(defn- ->play-sound-button [sound-file]
  (->text-button "play!" #(play-sound! sound-file)))

(declare ->sound-columns)

(defn- open-sounds-window! [table]
  (let [rows (for [sound-file all-sound-files]
               [(->text-button (str/replace-first sound-file "sounds/" "")
                                  (fn []
                                    (clear-children! table)
                                    (add-rows! table [(->sound-columns table sound-file)])
                                    (remove! (find-ancestor-window *on-clicked-actor*))
                                    (pack-ancestor-window! table)
                                    (set-id! table sound-file)))
                (->play-sound-button sound-file)])]
    (stage-add! (->scrollable-choose-window rows))))

(defn- ->sound-columns [table sound-file]
  [(->text-button (name sound-file) #(open-sounds-window! table))
   (->play-sound-button sound-file)])

(defmethod ->widget :sound [_ sound-file]
  (let [table (->table {:cell-defaults {:pad 5}})]
    (add-rows! table [(if sound-file
                        (->sound-columns table sound-file)
                        [(->text-button "No sound" #(open-sounds-window! table))])])
    table))

; TODO main properties optional keys to add them itself not possible (e.g. to add skill/cooldown back)
; TODO save button show if changes made, otherwise disabled?
; when closing (lose changes? yes no)
; TODO overview table not refreshed after changes in property editor window
; * don't show button if no components to add anymore (use remaining-ks)
; * what is missing to remove the button once the last optional key was added (not so important)
; maybe check java property/game/db/editors .... unity? rpgmaker? gamemaker?

(def ^:private property-k-sort-order
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

(defn- component-order [[k _v]]
  (or (index-of k property-k-sort-order) 99))

(defn- truncate [s limit]
  (if (> (count s) limit)
    (str (subs s 0 limit) "...")
    s))

(defmethod ->widget :default [_ v]
  (->label (truncate (->edn-str v) 60)))

(defmethod widget->value :default [_ widget]
  (actor-id widget))

(declare ->component-widget
         attribute-widget-group->data)

(defn- k-properties [schema]
  (let [[_m _p & ks] (m/form schema)]
    (into {} (for [[k m? _schema] ks]
               [k (if (map? m?) m?)]))))

(defn- map-keys [schema]
  (let [[_m _p & ks] (m/form schema)]
    (for [[k m? _schema] ks]
      k)))

(defn- k->default-value [k]
  (let [[data-type {:keys [schema]}] (data-component k)]
    (cond
     (#{:one-to-one :one-to-many} data-type) nil
     ;(#{:map} data-type) {} ; cannot have empty for required keys, then no Add Component button
     :else (mg/generate schema {:size 3}))))

(defn- ->choose-component-window [data attribute-widget-group]
  (fn []
    (let [k-props (k-properties (:schema data))
          window (->window {:title "Choose"
                            :modal? true
                            :close-button? true
                            :center? true
                            :close-on-escape? true
                            :cell-defaults {:pad 5}})
          remaining-ks (sort (remove (set (keys (attribute-widget-group->data attribute-widget-group)))
                                     (map-keys (:schema data))))]
      (add-rows! window (for [k remaining-ks]
                          [(->text-button (name k)
                                          (fn []
                                            (remove! window)
                                            (add-actor! attribute-widget-group
                                                        (->component-widget [k (get k-props k) (k->default-value k)]
                                                                            :horizontal-sep?
                                                                            (pos? (count (children attribute-widget-group)))))
                                            (pack-ancestor-window! attribute-widget-group)))]))
      (.pack window)
      (stage-add! window))))

(declare ->attribute-widget-group)

(defn- optional-keyset [schema]
  (set (map first
            (filter (fn [[k prop-m]] (:optional prop-m))
                    (k-properties schema)))))

(defmethod ->widget :map [[_ data] m]
  (let [attribute-widget-group (->attribute-widget-group (:schema data) m)
        optional-keys-left? (seq (set/difference (optional-keyset (:schema data))
                                                 (set (keys m))))]
    (set-id! attribute-widget-group :attribute-widget-group)
    (->table {:cell-defaults {:pad 5}
                 :rows (remove nil?
                               [(when optional-keys-left?
                                  [(->text-button "Add component" (->choose-component-window data attribute-widget-group))])
                                (when optional-keys-left?
                                  [(->horizontal-separator-cell 1)])
                                [attribute-widget-group]])})))


(defmethod widget->value :map [_ table]
  (attribute-widget-group->data (:attribute-widget-group table)))

(defn- ->attribute-label [k]
  (let [label (->label (str k))]
    (when-let [doc (:editor/doc (get component-attributes k))]
      (add-tooltip! label doc))
    label))

(defn- ->component-widget [[k k-props v] & {:keys [horizontal-sep?]}]
  (let [label (->attribute-label k)
        value-widget (->widget (data-component k) v)
        table (->table {:id k :cell-defaults {:pad 4}})
        column (remove nil?
                       [(when (:optional k-props)
                          (->text-button "-" #(let [window (find-ancestor-window table)]
                                                (remove! table)
                                                (.pack window))))
                        label
                        (->vertical-separator-cell)
                        value-widget])
        rows [(when horizontal-sep? [(->horizontal-separator-cell (count column))])
              column]]
    (set-id! value-widget v)
    (add-rows! table (remove nil? rows))
    table))

(defn- attribute-widget-table->value-widget [table]
  (-> table children last))

(defn- ->component-widgets [schema props]
  (let [first-row? (atom true)
        k-props (k-properties schema)]
    (for [[k v] (sort-by component-order props)
          :let [sep? (not @first-row?)
                _ (reset! first-row? false)]]
      (->component-widget [k (get k-props k) v] :horizontal-sep? sep?))))

(defn- ->attribute-widget-group [schema props]
  (->vertical-group (->component-widgets schema props)))

(defn- attribute-widget-group->data [group]
  (into {} (for [k (map actor-id (children group))
                 :let [table (k group)
                       value-widget (attribute-widget-table->value-widget table)]]
             [k (widget->value (data-component k) value-widget)])))

;;

(defn- apply-context-fn [window f]
  (fn []
    (try
     (f)
     (remove! window)
     (catch Throwable t
       (error-window! t)))))

(defn- ->property-editor-window [id]
  (let [props (safe-get properties-db id)
        window (->window {:title "Edit Property"
                             :modal? true
                             :close-button? true
                             :center? true
                             :close-on-escape? true
                             :cell-defaults {:pad 5}})
        widgets (->attribute-widget-group (->schema props) props)
        save!   (apply-context-fn window #(update! (attribute-widget-group->data widgets)))
        delete! (apply-context-fn window #(delete! id))]
    (add-rows! window [[(->scroll-pane-cell [[{:actor widgets :colspan 2}]
                                             [(->text-button "Save [LIGHT_GRAY](ENTER)[]" save!)
                                              (->text-button "Delete" delete!)]])]])
    (add-actor! window (->actor {:act (fn []
                                        (when (key-just-pressed? :enter)
                                          (save!)))}))
    (.pack window)
    window))

(defn- ->overview-property-widget [{:keys [property/id] :as props} clicked-id-fn extra-info-text scale]
  (let [on-clicked #(clicked-id-fn id)
        button (if-let [image (prop->image props)]
                 (->image-button image on-clicked {:scale scale})
                 (->text-button (name id) on-clicked))
        top-widget (->label (or (and extra-info-text (extra-info-text props)) ""))
        stack (->stack [button top-widget])]
    (add-tooltip! button #(->info-text props))
    (set-touchable! top-widget :disabled)
    stack))

(defn- ->overview-table [property-type clicked-id-fn]
  (let [{:keys [sort-by-fn
                extra-info-text
                columns
                image/scale]} (overview property-type)
        properties (all-properties property-type)
        properties (if sort-by-fn
                     (sort-by sort-by-fn properties)
                     properties)]
    (->table
     {:cell-defaults {:pad 5}
      :rows (for [properties (partition-all columns properties)]
              (for [property properties]
                (try (->overview-property-widget property clicked-id-fn extra-info-text scale)
                     (catch Throwable t
                       (throw (ex-info "" {:property property} t))))))})))

(import 'com.kotcrab.vis.ui.widget.tabbedpane.Tab)
(import 'com.kotcrab.vis.ui.widget.tabbedpane.TabbedPane)
(import 'com.kotcrab.vis.ui.widget.tabbedpane.TabbedPaneAdapter)
(import 'com.kotcrab.vis.ui.widget.VisTable)

(defn- ->tab [{:keys [title content savable? closable-by-user?]}]
  (proxy [Tab] [(boolean savable?) (boolean closable-by-user?)]
    (getTabTitle [] title)
    (getContentTable [] content)))

(defn- ->tabbed-pane [tabs-data]
  (let [main-table (->table {:fill-parent? true})
        container (VisTable.)
        tabbed-pane (TabbedPane.)]
    (.addListener tabbed-pane
                  (proxy [TabbedPaneAdapter] []
                    (switchedTab [^Tab tab]
                      (.clearChildren container)
                      (.fill (.expand (.add container (.getContentTable tab)))))))
    (.fillX (.expandX (.add main-table (.getTable tabbed-pane))))
    (.row main-table)
    (.fill (.expand (.add main-table container)))
    (.row main-table)
    (.pad (.left (.add main-table (->label "[LIGHT_GRAY]Left-Shift: Back to Main Menu[]"))) (float 10))
    (doseq [tab-data tabs-data]
      (.add tabbed-pane (->tab tab-data)))
    main-table))

(defn- open-property-editor-window! [property-id]
  (stage-add! (->property-editor-window property-id)))

(defn- ->tabs-data []
  (for [property-type (sort (types))]
    {:title (:title (overview property-type))
     :content (->overview-table property-type open-property-editor-window!)}))

(derive :screens/property-editor :screens/stage)
(defc :screens/property-editor
  (->mk [_]
    {:stage (->stage [(->background-image)
                      (->tabbed-pane (->tabs-data))
                      (->actor {:act (fn []
                                       (when (key-just-pressed? :shift-left)
                                         (change-screen :screens/main-menu)))})])}))

; TODO schemas not checking if that property exists in db...
; https://github.com/damn/core/issues/59


(defn- one-to-many-schema->linked-property-type [[_set [_qualif_kw {:keys [namespace]}]]]
  (ns-k->property-type namespace))

(comment
 (= (one-to-many-schema->linked-property-type [:set [:qualified-keyword {:namespace :items}]])
    :properties/items)
 )

(defmethod edn->value :one-to-many [_ property-ids]
  (map build-property property-ids))


(defn- one-to-one-schema->linked-property-type [[_qualif_kw {:keys [namespace]}]]
  (ns-k->property-type namespace))

(comment
 (= (one-to-one-schema->linked-property-type [:qualified-keyword {:namespace :creatures}])
    :properties/creatuers)
 )

(defmethod edn->value :one-to-one [_ property-id]
  (build-property property-id))

(defn- add-one-to-many-rows [table property-type property-ids]
  (let [redo-rows (fn [property-ids]
                    (clear-children! table)
                    (add-one-to-many-rows table property-type property-ids)
                    (pack-ancestor-window! table))]
    (add-rows!
     table
     [[(->text-button "+"
                      (fn []
                        (let [window (->window {:title "Choose"
                                                :modal? true
                                                :close-button? true
                                                :center? true
                                                :close-on-escape? true})
                              clicked-id-fn (fn [id]
                                              (remove! window)
                                              (redo-rows (conj property-ids id)))]
                          (t-add! window (->overview-table property-type clicked-id-fn))
                          (.pack window)
                          (stage-add! window))))]
      (for [property-id property-ids]
        (let [property (build-property property-id)
              image-widget (->image-widget (prop->image property) {:id property-id})]
          (add-tooltip! image-widget #(->info-text property))
          image-widget))
      (for [id property-ids]
        (->text-button "-" #(redo-rows (disj property-ids id))))])))

(defmethod ->widget :one-to-many [[_ data] property-ids]
  (let [table (->table {:cell-defaults {:pad 5}})]
    (add-one-to-many-rows table
                          (one-to-many-schema->linked-property-type (:schema data))
                          property-ids)
    table))

(defmethod widget->value :one-to-many [_ widget]
  (->> (children widget)
       (keep actor-id)
       set))

(defn- add-one-to-one-rows [table property-type property-id]
  (let [redo-rows (fn [id]
                    (clear-children! table)
                    (add-one-to-one-rows table property-type id)
                    (pack-ancestor-window! table))]
    (add-rows!
     table
     [[(when-not property-id
         (->text-button "+"
                        (fn []
                          (let [window (->window {:title "Choose"
                                                  :modal? true
                                                  :close-button? true
                                                  :center? true
                                                  :close-on-escape? true})
                                clicked-id-fn (fn [id]
                                                (remove! window)
                                                (redo-rows id))]
                            (t-add! window (->overview-table property-type clicked-id-fn))
                            (.pack window)
                            (stage-add! window)))))]
      [(when property-id
         (let [property (build-property property-id)
               image-widget (->image-widget (prop->image property) {:id property-id})]
           (add-tooltip! image-widget #(->info-text property))
           image-widget))]
      [(when property-id
         (->text-button "-" #(redo-rows nil)))]])))

(defmethod ->widget :one-to-one [[_ data] property-id]
  (let [table (->table {:cell-defaults {:pad 5}})]
    (add-one-to-one-rows table
                         (one-to-one-schema->linked-property-type (:schema data))
                         property-id)
    table))

(defmethod widget->value :one-to-one [_ widget]
  (->> (children widget) (keep actor-id) first))



(defn- add-metadoc! []
  (doseq [[doc-cat syms] (edn/read-string (slurp "doc/categories.edn"))
          sym syms]
    (try (alter-meta! (resolve sym) assoc :metadoc/categories #{doc-cat})
         (catch Throwable t
           (println "METADOC PROBLEM: " sym " : " t)))))

(defn- anony-class? [[sym avar]]
  (instance? java.lang.Class @avar))

; TODO only funcs, no macros
; what about record constructors, refer-all -> need to make either private or
; also highlight them ....
; only for categorization not necessary
(defn- vimstuff []
  (spit "vimstuff"
        (apply str
               (remove #{"defc" "defsystem"}
                       (interpose " , " (map str (keys (->> (ns-publics *ns*)
                                                            (remove anony-class?)))))))))

(defn- record-constructor? [[sym avar]]
  (re-find #"(map)?->\p{Upper}" (name sym)))


(defn- relevant-ns-publics []
  (->> (ns-publics *ns*)
       (remove anony-class?)
       (remove record-constructor?)))
; 1. macros separate
; 2. defsystems separate
; 3. 'v-'
; 4. protocols ?1 protocol functions included ?!

#_(spit "testo"
        (str/join "\n"
                  (for [[asym avar] (sort-by first (relevant-ns-publics))]
                    (str asym " " (:arglists (meta avar)))
                    )
                  )
        )

(comment
 (spit "relevant_ns_publics"
       (str/join "\n" (sort (map first (relevant-ns-publics))))))
; = 264 public vars
; next remove ->Foo and map->Foo

#_(let [[asym avar] (first (relevant-ns-publics))]
    (str asym " "(:arglists (meta avar)))
    )

(defn- ->clojurefuncs [fns-strs-names]
  (str "\\   'clojureFunc': [\""  fns-strs-names "\"]"))

; TODO only funcs, no macros
; what about record constructors, refer-all -> need to make either private or
; also highlight them ....
; only for categorization not necessary
(defn ->gdx-public-names-vimrc
  "Without macros `defc` and `defsystem`."
  []
  (spit "gdx_names_vimrc"
        (->clojurefuncs
         (apply str
                (remove #{"defc", "defsystem", "post-runnable!", "proxy-ILookup", "with-err-str", "when-seq"}
                        (interpose " , " (map str (keys (->> (ns-interns 'clojure.gdx)
                                                             (remove anony-class?))))))))))

(when (= "true" (System/getenv "ADD_METADOC"))
  (add-metadoc!))
