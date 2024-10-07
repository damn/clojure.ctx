(ns core.tool
  (:require [clojure.java.io :as io])
  (:import (javafx.event EventHandler)
           (javafx.scene.control Button TreeItem TreeView)
           (javafx.scene.image Image ImageView)
           (javafx.scene.layout StackPane)
           (javafx.scene Scene Node))
  (:gen-class
   :extends javafx.application.Application))

(defn- clj-file-forms [file]
  (read-string (str "[" (slurp file) "]")))

(require '[rewrite-clj.parser :as p])

(comment
 (p/parse-file-all "src/core/stat.clj")

 (p/parse-file-all "../clojure.gdx/src/clojure/gdx/java.clj")


 )



(defn -start [app stage]
  (def myapp app)
  (.setTitle stage "Tree View Sample")
  (let [root-icon (ImageView. (Image. (io/input-stream
                                       (io/resource "images/animations/vampire-1.png"))))
        root-item (TreeItem. "Inbox", root-icon)]
    (.setExpanded root-item true)
    (doseq [
            ;[first-sym second-sym] (doall (map (partial take 2) (clj-file-forms "src/core/stat.clj")))

            file (map str (file-seq (io/file "src/")))
            ]
      (.add (.getChildren root-item) (TreeItem.
                                      file
                                      #_(str first-sym " - " second-sym))))

    (let [stack-pane (StackPane.)]
      (.add (.getChildren stack-pane) (TreeView. root-item))
      (.setScene stage (Scene. stack-pane 300 250))
      (.show stage))))

(defn -main [& args]
  (javafx.application.Application/launch core.tool
                                         (into-array String [""])))
