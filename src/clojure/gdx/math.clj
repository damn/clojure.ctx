(in-ns 'clojure.gdx)

(defn equal?
  "Returns true if a is nearly equal to b."
  [a b]
  (MathUtils/isEqual a b))

(defn- degree->radians [degree]
  (* (float degree) MathUtils/degreesToRadians))

(defn clamp [value min max]
  (MathUtils/clamp (float value) (float min) (float max)))

; TODO not important badlogic, using clojure vectors
; could extend some protocol by clojure vectors and just require the protocol
; also call vector2 v2/add ? v2/scale ?

(defn- ^Vector2 ->v [[x y]]
  (Vector2. x y))

(defn- ->p [^Vector2 v]
  [(.x ^Vector2 v)
   (.y ^Vector2 v)])

(defn v-scale     [v n]    (->p (.scl ^Vector2 (->v v) (float n)))) ; TODO just (mapv (partial * 2) v)
(defn v-normalise [v]      (->p (.nor ^Vector2 (->v v))))
(defn v-add       [v1 v2]  (->p (.add ^Vector2 (->v v1) ^Vector2 (->v v2))))
(defn v-length    [v]      (.len ^Vector2 (->v v)))
(defn v-distance  [v1 v2]  (.dst ^Vector2 (->v v1) ^Vector2 (->v v2)))

(defn v-normalised? [v] (equal? 1 (v-length v)))

(defn v-get-normal-vectors [[x y]]
  [[(- (float y))         x]
   [          y (- (float x))]])

(defn v-direction [[sx sy] [tx ty]]
  (v-normalise [(- (float tx) (float sx))
                (- (float ty) (float sy))]))

(defn v-get-angle-from-vector
  "converts theta of Vector2 to angle from top (top is 0 degree, moving left is 90 degree etc.), ->counterclockwise"
  [v]
  (.angleDeg (->v v) (Vector2. 0 1)))

(comment

 (pprint
  (for [v [[0 1]
           [1 1]
           [1 0]
           [1 -1]
           [0 -1]
           [-1 -1]
           [-1 0]
           [-1 1]]]
    [v
     (.angleDeg (->v v) (Vector2. 0 1))
     (get-angle-from-vector (->v v))]))

 )

(defn- ->circle [[x y] radius]
  (Circle. x y radius))

(defn- ->rectangle [[x y] width height]
  (Rectangle. x y width height))

(defn- rect-contains? [^Rectangle rectangle [x y]]
  (.contains rectangle x y))

(defmulti ^:private overlaps? (fn [a b] [(class a) (class b)]))

(defmethod overlaps? [Circle Circle]
  [^Circle a ^Circle b]
  (Intersector/overlaps a b))

(defmethod overlaps? [Rectangle Rectangle]
  [^Rectangle a ^Rectangle b]
  (Intersector/overlaps a b))

(defmethod overlaps? [Rectangle Circle]
  [^Rectangle rect ^Circle circle]
  (Intersector/overlaps circle rect))

(defmethod overlaps? [Circle Rectangle]
  [^Circle circle ^Rectangle rect]
  (Intersector/overlaps circle rect))

(defn- rectangle? [{[x y] :left-bottom :keys [width height]}]
  (and x y width height))

(defn- circle? [{[x y] :position :keys [radius]}]
  (and x y radius))

(defn- m->shape [m]
  (cond
   (rectangle? m) (let [{:keys [left-bottom width height]} m]
                    (->rectangle left-bottom width height))

   (circle? m) (let [{:keys [position radius]} m]
                 (->circle position radius))

   :else (throw (Error. (str m)))))

(defn shape-collides? [a b]
  (overlaps? (m->shape a) (m->shape b)))

(defn point-in-rect? [point rectangle]
  (rect-contains? (m->shape rectangle) point))

(defn circle->outer-rectangle [{[x y] :position :keys [radius] :as circle}]
  {:pre [(circle? circle)]}
  (let [radius (float radius)
        size (* radius 2)]
    {:left-bottom [(- (float x) radius)
                   (- (float y) radius)]
     :width  size
     :height size}))

(defprotocol PFastRayCaster
  (fast-ray-blocked? [_ start target]))

; boolean array used because 10x faster than access to clojure grid data structure

; this was a serious performance bottleneck -> alength is counting the whole array?
;(def ^:private width alength)
;(def ^:private height (comp alength first))

; does not show warning on reflection, but shows cast-double a lot.
(defrecord ArrRayCaster [arr width height]
  PFastRayCaster
  (fast-ray-blocked? [_ [start-x start-y] [target-x target-y]]
    (RayCaster/rayBlocked (double start-x)
                          (double start-y)
                          (double target-x)
                          (double target-y)
                          width ;(width boolean-2d-array)
                          height ;(height boolean-2d-array)
                          arr)))

#_(defn ray-steplist [boolean-2d-array [start-x start-y] [target-x target-y]]
  (seq
   (RayCaster/castSteplist start-x
                           start-y
                           target-x
                           target-y
                           (width boolean-2d-array)
                           (height boolean-2d-array)
                           boolean-2d-array)))

#_(defn ray-maxsteps [boolean-2d-array  [start-x start-y] [vector-x vector-y] max-steps]
  (let [erg (RayCaster/castMaxSteps start-x
                                    start-y
                                    vector-x
                                    vector-y
                                    (width boolean-2d-array)
                                    (height boolean-2d-array)
                                    boolean-2d-array
                                    max-steps
                                    max-steps)]
    (if (= -1 erg)
      :not-blocked
      erg)))

; STEPLIST TEST

#_(def current-steplist (atom nil))

#_(defn steplist-contains? [tilex tiley] ; use vector equality
  (some
    (fn [[x y]]
      (and (= x tilex) (= y tiley)))
    @current-steplist))

#_(defn render-line-middle-to-mouse [color]
  (let [[x y] (input/get-mouse-pos)]
    (g/draw-line (/ (g/viewport-width) 2)
                 (/ (g/viewport-height) 2)
                 x y color)))

#_(defn update-test-raycast-steplist []
    (reset! current-steplist
            (map
             (fn [step]
               [(.x step) (.y step)])
             (raycaster/ray-steplist (get-cell-blocked-boolean-array)
                                     (:position @player-entity)
                                     (g/map-coords)))))

;; MAXSTEPS TEST

#_(def current-steps (atom nil))

#_(defn update-test-raycast-maxsteps []
    (let [maxsteps 10]
      (reset! current-steps
              (raycaster/ray-maxsteps (get-cell-blocked-boolean-array)
                                      (v-direction (g/map-coords) start)
                                      maxsteps))))

#_(defn draw-test-raycast []
  (let [start (:position @player-entity)
        target (g/map-coords)
        color (if (fast-ray-blocked? start target) g/red g/green)]
    (render-line-middle-to-mouse color)))

; PATH BLOCKED TEST

#_(defn draw-test-path-blocked [] ; TODO draw in map no need for screenpos-of-tilepos
  (let [[start-x start-y] (:position @player-entity)
        [target-x target-y] (g/map-coords)
        [start1 target1 start2 target2] (create-double-ray-endpositions start-x start-y target-x target-y 0.4)
        [start1screenx,start1screeny]   (screenpos-of-tilepos start1)
        [target1screenx,target1screeny] (screenpos-of-tilepos target1)
        [start2screenx,start2screeny]   (screenpos-of-tilepos start2)
        [target2screenx,target2screeny] (screenpos-of-tilepos target2)
        color (if (is-path-blocked? start1 target1 start2 target2)
                g/red
                g/green)]
    (g/draw-line start1screenx start1screeny target1screenx target1screeny color)
    (g/draw-line start2screenx start2screeny target2screenx target2screeny color)))
