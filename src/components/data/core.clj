(ns components.data.core
  (:require [clojure.edn :as edn]
            [malli.core :as m]
            [utils.core :as utils]
            [core.component :as component :refer [defcomponent]]
            [core.context :as ctx]
            [core.data :as data]
            [core.scene2d.actor :as actor]
            [core.scene2d.ui.text-field :as text-field])
  (:import (com.kotcrab.vis.ui.widget VisCheckBox VisSelectBox)))

(defcomponent :some {:schema :some})

(defcomponent :boolean {:schema :boolean :default-value true})

(defmethod data/->widget :boolean [_ checked? ctx]
  (assert (boolean? checked?))
  (ctx/->check-box ctx "" (fn [_]) checked?))

(defmethod data/widget->value :boolean [_ widget]
  (.isChecked ^VisCheckBox widget))

(defcomponent :string {:schema :string})

(defn- add-schema-tooltip! [widget data]
  (actor/add-tooltip! widget (str "Schema: " (pr-str (m/form (:schema data)))))
  widget)

(defmethod data/->widget :string [[_ data] v ctx]
  (add-schema-tooltip! (ctx/->text-field ctx v {})
                       data))

(defmethod data/widget->value :string [_ widget]
  (text-field/text widget))

(defcomponent :number  {:schema number?})
(defcomponent :nat-int {:schema nat-int? :widget :number})
(defcomponent :int     {:schema int?     :widget :number})
(defcomponent :pos     {:schema pos?     :widget :number})
(defcomponent :pos-int {:schema pos-int? :widget :number})

(defmethod data/->widget :number [[_ data] v ctx]
  (add-schema-tooltip! (ctx/->text-field ctx (utils/->edn-str v) {})
                       data))

(defmethod data/widget->value :number [_ widget]
  (edn/read-string (text-field/text widget)))

(defcomponent :enum
  (component/->data [[_ items]]
    {:schema (apply vector :enum items)
     :items items}))

(defmethod data/->widget :enum [[_ data] v ctx]
  (ctx/->select-box ctx {:items (map utils/->edn-str (:items data))
                         :selected (utils/->edn-str v)}))

(defmethod data/widget->value :enum [_ widget]
  (edn/read-string (.getSelected ^VisSelectBox widget)))

(defcomponent :qualified-keyword
  (component/->data [schema]
    {:schema schema}))

(defn- map-schema [ks]
  (apply vector :map {:closed true} (component/attribute-schema ks)))

; component/attribute schema used @ properties themself
; :data :map
; :data :components
; & :data :components-ns

(defcomponent :map
  (component/->data [[_ ks]]
    {:schema (map-schema ks)
     ;:default-value (zipmap ks (map component/default-value ks))

     }))

; TODO use :map
(defcomponent :components
  (component/->data [[_ ks]]
    {:widget :map
     :schema (map-schema (map (fn [k] [k {:optional true}]) ks))
     :components ks ; TODO if vector take first
     :default-value {}}))

(defcomponent :components-ns
  (component/->data [[_ k]]
    (let [ks (filter #(= (name k) (namespace %)) (keys component/attributes))]
      (component/->data [:components ks]))))
