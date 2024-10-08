(in-ns 'clojure.gdx)

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

(def component-attributes {})

(def ^:private warn-name-ns-mismatch? false)

(defn- k->component-ns [k] ;
  (symbol (str "components." (name (namespace k)) "." (name k))))

(defn- check-warn-ns-name-mismatch [k]
  (when (and warn-name-ns-mismatch?
             (namespace k)
             (not= (k->component-ns k) (ns-name *ns*)))
    (println "WARNING: defc " k " is not matching with namespace name " (ns-name *ns*))))

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

(defsystem ->mk "Create component value. Default returns v." [_])
(defmethod ->mk :default [[_ v]] v)

(defsystem ^:private destroy! "Side effect destroy resources. Default do nothing." [_])
(defmethod destroy! :default [_])

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

(defn- data-component [k]
  (try (let [data (:data (safe-get component-attributes k))]
         (if (vector? data)
           [(first data) (->value data)]
           [data (safe-get component-attributes data)]))
       (catch Throwable t
         (throw (ex-info "" {:k k} t)))))

;;;;


(declare info-text-k-order)

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
