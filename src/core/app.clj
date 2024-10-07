(ns core.app
  (:require [clojure.gdx :refer :all]
            core.config
            core.creature
            core.stat
            [core.world :as world]))

(def dev-mode? (= (System/getenv "DEV_MODE") "true"))

(load "screens/minimap"
      "screens/world"
      "screens/main_menu"
      "screens/options")

(defn -main []
  (start-app! "resources/properties.edn"))
