(ns clj.gdx.app
  (:import com.badlogic.gdx.Gdx))

(defn exit []
  (.exit Gdx/app))
