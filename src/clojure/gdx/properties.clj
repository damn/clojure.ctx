(in-ns 'clojure.gdx)

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

(defmulti edn->value (fn [data v] (if data (data 0))))
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

(declare ^:private properties-db
         ^:private properties-edn-file)

(defn- ->ctx-properties
  "Validates all properties."
  [properties-edn-file]
  (let [properties (-> properties-edn-file slurp edn/read-string)]
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
