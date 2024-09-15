(ns app
  (:require [clojure.string :as str]
            clojure.java.io
            [utils.core :refer [safe-merge]]
            [gdx.backends.lwjgl3 :as lwjgl3]
            [gdx.graphics.color :as color]
            [gdx.utils.screen-utils :as screen-utils]
            [core.component :as component]
            [core.context :as ctx]
            [components.context.properties :as properties])
  (:import com.badlogic.gdx.ApplicationAdapter
           com.badlogic.gdx.utils.ScreenUtils))

(def state (atom nil))

; screens require vis-ui / properties (map-editor, property editor uses properties)
(defn- context-create-order [[k _]]
  (if (= k :context/screens) 1 0))

(defn- ->application [context]
  (proxy [ApplicationAdapter] []
    (create []
      (->> context
           (sort-by context-create-order)
           (component/create-into (assoc context :context/state state))
           ctx/init-first-screen
           (reset! state)))

    (dispose []
      (run! component/destroy @state))

    (render []
      (ScreenUtils/clear color/black)
      (-> @state
          ctx/current-screen
          (component/render! state)))

    (resize [w h]
      (ctx/update-viewports @state w h))))

(defn- component-namespaces []
  (filter #(str/ends-with? % ".clj")
          (map (memfn getPath)
               (file-seq (clojure.java.io/file "src/components/")))))

(defn- require-all-components! []
  (doseq [file (component-namespaces)
          :let [ns (-> file
                       (str/replace "src/" "")
                       (str/replace ".clj" "")
                       (str/replace "/" ".")
                       symbol)]]
    (when-not (find-ns ns)
      (require ns))))

(defn -main [& [file]]
  (require-all-components!)
  (let [ctx (assoc (ctx/->Context) :context/properties (properties/validate-and-create file))
        app (ctx/property ctx :app/core)]
    (lwjgl3/->application (->application (safe-merge ctx (:app/context app)))
                          (lwjgl3/->configuration (:app/lwjgl3 app)))))
