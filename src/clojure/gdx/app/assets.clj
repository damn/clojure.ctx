(in-ns 'clojure.gdx)

(defn dispose! [obj] (Disposable/.dispose obj))

(defn- asset-manager ^AssetManager []
  (proxy [AssetManager clojure.lang.ILookup] []
    (valAt [file]
      (.get ^AssetManager this ^String file))))

(defn- internal-file
  "Path relative to the asset directory on Android and to the application's root directory on the desktop. On the desktop, if the file is not found, then the classpath is checked. This enables files to be found when using JWS or applets. Internal files are always readonly."
  ^FileHandle [path]
  (.internal Gdx/files path))

(defn- recursively-search [folder extensions]
  (loop [[^FileHandle file & remaining] (.list (internal-file folder))
         result []]
    (cond (nil? file)
          result

          (.isDirectory file)
          (recur (concat remaining (.list file)) result)

          (extensions (.extension file))
          (recur remaining (conj result (.path file)))

          :else
          (recur remaining result))))

(defn- search-files [folder file-extensions]
  (map #(str/replace-first % folder "")
       (recursively-search folder file-extensions)))

(defn- load-assets! [^AssetManager manager files class-k]
  (let [^Class klass (case class-k :sound Sound :texture Texture)]
    (doseq [file files]
      (.load manager ^String file klass))))

(declare ^:private assets)

(def ^:private image-file-extensions #{"png" "bmp"})
(def ^:private sound-file-extensions #{"wav"})

(defc :context/assets
  {:data :string}
  (->mk [[_ folder]]
    (let [manager (asset-manager)
          sound-files   (search-files folder sound-file-extensions)
          texture-files (search-files folder image-file-extensions)]
      (load-assets! manager sound-files   :sound)
      (load-assets! manager texture-files :texture)
      (.finishLoading manager)
      (bind-root #'assets {:manager manager
                           :sound-files sound-files
                           :texture-files texture-files})))

  (destroy! [_]
    (dispose! (:manager assets))))

(defn- asset
  "Returns the sound or texture at file-path."
  [path]
  (get (:manager assets) path))

(defn play-sound!
  "Sound is already loaded from file, this will perform only a lookup for the sound and play it."
  [path]
  (Sound/.play (asset path)))

(defc :tx/sound
  {:data :sound}
  (do! [[_ file]]
    (play-sound! file)
    nil))
