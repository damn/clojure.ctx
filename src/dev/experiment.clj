(ns dev.experiment
  (:require [clojure.gdx :refer :all]))
(comment


 (post-runnable! (show-tree-view! :ctx))

 (show-tree-view! :entity)
 (show-tree-view! :tile)

 ; if we remove ns we can remove it at tree view !!


 (print-app-values-tree)

 ; We mostly want to know abouts txs, entity and schema
 ; the rest not so important?

 (print-txs "txs.md")

 (print-components "components.md")

 (spit-out "data-components.md" (data-components))

 ; TODO items dont refresh on clicking tab -!

 ; * Test
 ; * if z-order/effect renders behind wall
 ; * => graphics txs?
 (post-tx! [:tx/line-render {:start [68 38]
                             :end [70 30]
                             :color [1 1 1]
                             :duration 2}])

 (do
  (learn-skill! :skills/projectile)
  (learn-skill! :skills/spawn)
  (learn-skill! :skills/meditation)
  (learn-skill! :skills/death-ray)
  (learn-skill! :skills/convert)
  (learn-skill! :skills/blood-curse)
  (learn-skill! :skills/slow)
  (learn-skill! :skills/double-fireball))

 ; FIXME
 ; first says inventory is full
 ; ok! beholder doesn't have inventory !
 ; => tests...
 (create-item! :items/blood-glove)

 (require '[clojure.string :as str])
 (spit "item_tags.txt"
       (with-out-str
        (clojure.pprint/pprint
         (distinct
          (sort
           (mapcat
            (comp #(str/split % #"-")
                  name
                  :property/id)
            (property/all-properties :properties/items)))))))

 )

(comment
 ; start world - small empty test room
 ; 2 creatures - player?
 ; start skill w. applicable needs target (bow)
 ; this command:
 (post-tx! [:e/destroy (get-entity 68)])
 ; check skill has stopped using

 (post-tx! [:tx/creature {:position [35 73]
                          :creature-id :creatures/dragon-red
                          :components {:entity/state [:state/npc :npc-sleeping]
                                       :entity/faction :evil} }])
 )

(defn- post-tx! [tx]
  (post-runnable! (effect! [tx])))

(defn- learn-skill! [skill-id] (post-tx! (fn [] [[:tx/add-skill (:entity/id @player-entity) (build-property skill-id)]])))
(defn- create-item! [item-id]  (post-tx! (fn [] [[:tx/item       (:position @player-entity) (build-property item-id)]])))

(defn- protocol? [value]
  (and (instance? clojure.lang.PersistentArrayMap value)
       (:on value)))

(defn- get-non-fn-vars [nmspace]
  (get-vars nmspace (fn [avar]
                      (let [value @avar]
                        (not (or (fn? value)
                                 (instance? clojure.lang.MultiFn value)
                                 #_(:method-map value) ; breaks for stage Ilookup
                                 (protocol? value)
                                 (instance? java.lang.Class value) ;anonymous class (proxy)
                                 ))))))

(defn- print-app-values-tree []
  (spit "app-values-tree.clj"
        (with-out-str
         (clojure.pprint/pprint
          (for [nmspace (sort-by (comp name ns-name)
                                 (get-namespaces
                                  (fn [first-ns-part-str]
                                    (not (#{;"clojure"
                                            "nrepl"
                                            "malli"
                                            "user"
                                            "borkdude"
                                            "clj-commons"
                                            "dorothy"
                                            "reduce-fsm"}
                                          first-ns-part-str)))))
                :let [value-vars (get-non-fn-vars nmspace)]
                :when (seq value-vars)]
            [(ns-name nmspace) (map (comp symbol name symbol) value-vars)])))))

; https://gist.github.com/pierrejoubert73/902cc94d79424356a8d20be2b382e1ab
; https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/organizing-information-with-collapsed-sections
; -> and after each 'build' I can have a bash script which uploads the components go github

; => component-attributes private
; => move component docs in here
; => I need also an internal documentation ??
; of private fns?!

(defn- print-txs [file]
  (spit file
        (binding [*print-level* nil]
          (with-out-str
           (doseq [[nmsp ks] (sort-by first
                                      (group-by namespace (sort (keys (methods do!)))))]

             (println "\n#" nmsp)
             (doseq [k ks
                     :let [attr-m (get component-attributes k)]]
               (println (str "* __" k "__ `" (get (:params attr-m) "do!") "`"))
               (when-let [data (:data attr-m)]
                 (println (str "    * data: `" (pr-str data) "`")))
               (let [ks (descendants k)]
                 (when (seq ks)
                   (println "    * Descendants"))
                 (doseq [k ks]
                   (println "      *" k)
                   (println (str "        * data: `" (pr-str (:data (get component-attributes k))) "`"))))))))))

(defn- component-systems [component-k]
   (for [[sys-name sys-var] defsystems
         [k method] (methods @sys-var)
         :when (= k component-k)]
     sys-name))

(defn- data-components []
  (sort
   (concat
    (keys (methods @#'clojure.gdx/->value))
    (map first
         (filter (fn [[k attr-m]]
                   (:schema attr-m))
                 component-attributes)))))

(defn- print-components* [ks]
  (doseq [k ks]
    (println "*" k
             (if-let [ancestrs (ancestors k)]
               (str "-> "(clojure.string/join "," ancestrs))
               "")
             (let [attr-map (get component-attributes k)]
               #_(if (seq attr-map)
                   (pr-str (:core.component/fn-params attr-map))
                   (str " `"
                        (binding [*print-level* nil]
                          (with-out-str
                           (clojure.pprint/pprint (dissoc attr-map :params))))
                        "`\n"
                        )
                   "")
               ""))
    #_(doseq [system-name (component-systems k)]
        (println "  * " system-name))))

(defn- spit-out [file ks]
  (spit file
        (binding [*print-level* nil]
          (with-out-str
           (print-components* ks)))))

(defn- print-components [file]
  (spit file
        (binding [*print-level* nil]
          (with-out-str
           (doseq [[nmsp components] (sort-by first
                                              (group-by namespace
                                                        (sort (keys component-attributes))))]
             (println "\n#" nmsp)
             (print-components* components)
             )))))

; TODO expand only on click ... save bandwidth ....
; crash only on expanding big one ...



(defn- ->v-str [v]
  (cond
   (number? v) v
   (keyword? v) v
   (string? v) (pr-str v)
   (boolean? v) v
   (instance? clojure.lang.Atom v) (str "[LIME] Atom [GRAY]" (class @v) "[]")
   (map? v) (str (class v))
   (and (vector? v) (< (count v) 3)) (pr-str v)
   (vector? v) (str "Vector "(count v))
   :else (str "[GRAY]" (str v) "[]")))

(defn- ->labelstr [k v]
  (str "[LIGHT_GRAY]:"
       (if (keyword? k)
         (str
          (when-let [ns (namespace k)] (str ns "/")) "[WHITE]" (name k))
         k) ; TODO truncate ...
       ": [GOLD]" (str (->v-str v))))

(defn- add-elements! [node elements]
  (doseq [element elements
          :let [el-node (->t-node (->label (str (->v-str element))))]]
    (t-node-add! node el-node)))

(declare add-map-nodes!)

(defn- children->str-map [children]
  (zipmap (map str children)
          children))

(defn- ->nested-nodes [node level v]
  (when (map? v)
    (add-map-nodes! node v (inc level)))

  (when (and (vector? v) (>= (count v) 3))
    (add-elements! node v))

  (when (instance? com.badlogic.gdx.scenes.scene2d.Stage v)
    (add-map-nodes! node (children->str-map (children (s-root v))) level))

  (when (instance? com.badlogic.gdx.scenes.scene2d.Group v)
    (add-map-nodes! node (children->str-map (children v)) level)))

(comment
 (let [vis-image (first (children (s-root (stage-get))))]
   (supers (class vis-image))
   (str vis-image)
   )
 )

(defn- add-map-nodes! [parent-node m level]
  ;(println "Level: " level " - go deeper? " (< level 4))
  (when (< level 2)
    (doseq [[k v] (into (sorted-map) m)]
      ;(println "add-map-nodes! k " k)
      (try
       (let [node (->t-node (->label (->labelstr k v)))]
         (.add parent-node node) ; no t-node-add!: tree cannot be casted to tree-node ... , Tree itself different .add
         #_(when (instance? clojure.lang.Atom v) ; StackOverFLow
           (->nested-nodes node level @v))
         (->nested-nodes node level v))
       (catch Throwable t
         (throw (ex-info "" {:k k :v v} t))
         #_(.add parent-node (->t-node (->label (str "[RED] "k " - " t))))

         )))))

(defn- ->prop-tree [prop]
  (let [tree (->ui-tree)]
    (add-map-nodes! tree prop 0)
    tree))

(defn- scroll-pane-cell [rows]
  (let [table (->table {:rows rows
                        :cell-defaults {:pad 1}
                        :pack? true})
        scroll-pane (->scroll-pane table)]
    {:actor scroll-pane
     :width (/ (gui-viewport-width) 2)
     :height
     (- (gui-viewport-height) 50)
     #_(min (- (gui-viewport-height) 50) (height table))}))

(defn- show-tree-view! [obj]
  (let [object (case obj
                 :entity (mouseover-entity*)
                 :tile @(get world-grid (mapv int (world-mouse-position))))]
    (stage-add! (->window {:title "Tree View"
                           :close-button? true
                           :close-on-escape? true
                           :center? true
                           :rows [[(scroll-pane-cell [[(->prop-tree (into (sorted-map) object))]])]]
                           :pack? true}))
    nil))
