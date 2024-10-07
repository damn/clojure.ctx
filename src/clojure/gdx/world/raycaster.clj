(in-ns 'clojure.gdx)

(defn- set-arr [arr cell* cell*->blocked?]
  (let [[x y] (:position cell*)]
    (aset arr x y (boolean (cell*->blocked? cell*)))))

(defn- ->raycaster [grid position->blocked?]
  (let [width  (g/width  grid)
        height (g/height grid)
        arr (make-array Boolean/TYPE width height)]
    (doseq [cell (g/cells grid)]
      (set-arr arr @cell position->blocked?))
    (map->ArrRayCaster {:arr arr
                        :width width
                        :height height})))

; TO math.... // not tested
(defn- create-double-ray-endpositions
  "path-w in tiles."
  [[start-x start-y] [target-x target-y] path-w]
  {:pre [(< path-w 0.98)]} ; wieso 0.98??
  (let [path-w (+ path-w 0.02) ;etwas gr�sser damit z.b. projektil nicht an ecken anst�sst
        v (v-direction [start-x start-y]
                       [target-y target-y])
        [normal1 normal2] (v-get-normal-vectors v)
        normal1 (v-scale normal1 (/ path-w 2))
        normal2 (v-scale normal2 (/ path-w 2))
        start1  (v-add [start-x  start-y]  normal1)
        start2  (v-add [start-x  start-y]  normal2)
        target1 (v-add [target-x target-y] normal1)
        target2 (v-add [target-x target-y] normal2)]
    [start1,target1,start2,target2]))

(declare world-raycaster)

(defn init-world-raycaster! [grid position->blocked?]
  (bind-root #'world-raycaster (->raycaster grid position->blocked?)))

(defn ray-blocked? [start target]
  (fast-ray-blocked? world-raycaster start target))

(defn path-blocked?
  "path-w in tiles. casts two rays."
  [start target path-w]
  (let [[start1,target1,start2,target2] (create-double-ray-endpositions start target path-w)]
    (or
     (ray-blocked? start1 target1)
     (ray-blocked? start2 target2))))
