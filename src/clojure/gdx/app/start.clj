(in-ns 'clojure.gdx)

(defn- clear-screen! [] (ScreenUtils/clear Color/BLACK))

(defn- lwjgl3-app-config
  [{:keys [title width height full-screen? fps]}]
  {:pre [title width height (boolean? full-screen?) (or (nil? fps) (int? fps))]}
  ; https://github.com/libgdx/libgdx/pull/7361
  ; Maybe can delete this when using that new libgdx version
  ; which includes this PR.
  (when SharedLibraryLoader/isMac
    (.set org.lwjgl.system.Configuration/GLFW_LIBRARY_NAME "glfw_async")
    (.set org.lwjgl.system.Configuration/GLFW_CHECK_THREAD0 false))
  (let [config (doto (Lwjgl3ApplicationConfiguration.)
                 (.setTitle title)
                 (.setForegroundFPS (or fps 60)))]
    (if full-screen?
      (.setFullscreenMode config (Lwjgl3ApplicationConfiguration/getDisplayMode))
      (.setWindowedMode config width height))
    config))

(defprotocol AppListener
  (^:private on-create [_])
  (^:private on-dispose [_])
  (^:private on-render [_])
  (^:private on-resize [_ dim]))

(defn- lwjgl3-app [app-listener config]
  (Lwjgl3Application. (proxy [ApplicationAdapter] []
                        (create  []    (on-create  app-listener))
                        (dispose []    (on-dispose app-listener))
                        (render  []    (on-render  app-listener))
                        (resize  [w h] (on-resize  app-listener [w h])))
                      (lwjgl3-app-config config)))

(defn- ctx-mk-order [[k _]]
  (if (= k :context/screens) 1 0))

(defn- ctx-app-listener [ctx]
  (reify AppListener
    (on-create [_]
      (run! ->mk (sort-by ctx-mk-order ctx)))

    (on-dispose [_]
      (run! destroy! ctx)); only keys ... FIXME ?

    (on-render [_]
      (clear-screen!)
      (screen-render! (current-screen)))

    (on-resize [_ dim]
      (update-viewports! dim))))

(defn start-app!
  "Validates all properties, then starts a libgdx application with the desktop (lwjgl3) backend and context/settings as of property :app/core."
  [properties-edn-file]
  (->ctx-properties properties-edn-file)
  (let [app (build-property :app/core)]
    (lwjgl3-app (ctx-app-listener (:app/context app))
                (:app/lwjgl3 app))))

(def-attributes
  :fps          :nat-int
  :full-screen? :boolean
  :width        :nat-int
  :height       :nat-int
  :title        :string
  :app/lwjgl3 [:map [:fps
                     :full-screen?
                     :width
                     :height
                     :title]]
  :app/context [:map [:context/assets
                      :context/graphics
                      :context/screens
                      :context/vis-ui]])

(def-type :properties/app
  {:schema [:app/lwjgl3
            :app/context]
   :overview {:title "Apps"
              :columns 10}})

(defn exit-app!
  "Schedule an exit from the application. On android, this will cause a call to pause() and dispose() some time in the future, it will not immediately finish your application. On iOS this should be avoided in production as it breaks Apples guidelines

  [javadoc](https://javadoc.io/static/com.badlogicgames.gdx/gdx/1.12.1/com/badlogic/gdx/Application.html#exit())"
  []
  (.exit Gdx/app))

(defmacro post-runnable!
  "Posts a Runnable on the main loop thread. In a multi-window application, the Gdx.graphics and Gdx.input values may be unpredictable at the time the Runnable is executed. If graphics or input are needed, they can be copied to a variable to be used in the Runnable. For example:

  final Graphics graphics = Gdx.graphics;

  [javadoc](https://javadoc.io/static/com.badlogicgames.gdx/gdx/1.12.1/com/badlogic/gdx/Application.html#postRunnable(java.lang.Runnable))"
  [& exprs]
  `(.postRunnable Gdx/app (fn [] ~@exprs)))
