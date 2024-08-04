(ns context.image-drawer-creator
  (:require [api.context :as ctx])
  (:import com.badlogic.gdx.graphics.Texture
           com.badlogic.gdx.graphics.g2d.TextureRegion))

(defn- texture-region-dimensions [^TextureRegion texture-region]
  [(.getRegionWidth  texture-region)
   (.getRegionHeight texture-region)])

(defn- scale-dimensions [dimensions scale]
  (mapv (comp float (partial * scale)) dimensions))

(defn- assoc-dimensions [{:keys [texture-region scale] :as image} world-unit-scale]
  {:pre [(number? world-unit-scale)
         (or (number? scale)
             (and (vector? scale)
                  (number? (scale 0))
                  (number? (scale 1))))]}
  (let [pixel-dimensions (if (number? scale)
                           (scale-dimensions (texture-region-dimensions texture-region) scale)
                           scale)]
    (assoc image
           :pixel-dimensions pixel-dimensions
           :world-unit-dimensions (scale-dimensions pixel-dimensions world-unit-scale))))

(defrecord Image [;; 4 fields used for drawing:
                  texture-region
                  color ; optional
                  pixel-dimensions
                  world-unit-dimensions
                  ;;
                  file ; used for serialization
                  sub-image-bounds ; => is in texture-region data? // only used for creating the texture-region itself -> pass
                  scale ; can be a number (multiplies original texture-region-dimensions) or [w h] for a fixed size
                        ; is not used  directly for drawing ....
                  tilew ;; used @ spritesheet image
                  tileh])

(defn- ->texture-region [ctx file & [x y w h]]
  (let [^Texture texture (ctx/cached-texture ctx file)]
    (if (and x y w h)
      (TextureRegion. texture (int x) (int y) (int w) (int h))
      (TextureRegion. texture))))

(extend-type api.context.Context
  api.context/ImageCreator
  (create-image [{{:keys [world-unit-scale]} :context/graphics :as ctx} file]
    (->> {:texture-region (->texture-region ctx file)
          :file file
          :scale 1}
         map->Image
         (assoc-dimensions world-unit-scale)))

  (get-scaled-copy [{{:keys [world-unit-scale]} :context/graphics} image scale]
    (-> image
        (assoc :scale scale)
        (assoc-dimensions world-unit-scale)))

  (get-sub-image [{{:keys [world-unit-scale]} :context/graphics :as ctx}
                  {:keys [file sub-image-bounds] :as image}]
    (-> image
        (assoc :scale 1
               :texture-region (apply ->texture-region ctx file sub-image-bounds)
               :sub-image-bounds sub-image-bounds)
        (assoc-dimensions world-unit-scale)))

  (spritesheet [context file tilew tileh]
    (-> (ctx/create-image context file)
        (assoc :tilew tilew
               :tileh tileh)))

  (get-sprite [context {:keys [tilew tileh] :as sheet} [x y]]
    (ctx/get-sub-image context
                       (assoc sheet :sub-image-bounds [(* x tilew) (* y tileh) tilew tileh]))))
