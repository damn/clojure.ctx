(in-ns 'clojure.gdx)

(defn- k->widget [k]
  (cond
   (#{:map-optional :components-ns} k) :map
   (#{:number :nat-int :int :pos :pos-int :val-max} k) :number
   :else k))

(defmulti ->widget      (fn [[k _] _v] (k->widget k)))
(defmulti widget->value (fn [[k _] _widget] (k->widget k)))

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
  (for [file (sort (:texture-files assets))]
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
  (let [rows (for [sound-file (:sound-files assets)]
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

(declare property-k-sort-order)

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
