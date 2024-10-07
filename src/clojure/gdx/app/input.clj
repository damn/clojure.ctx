(in-ns 'clojure.gdx)

(defn- ->gdx-field [klass-str k]
  (eval (symbol (str "com.badlogic.gdx." klass-str "/" (str/replace (str/upper-case (name k)) "-" "_")))))

(def ^:private ->gdx-input-button (partial ->gdx-field "Input$Buttons"))
(def ^:private ->gdx-input-key    (partial ->gdx-field "Input$Keys"))

(comment
 (and (= (->gdx-input-button :left) 0)
      (= (->gdx-input-button :forward) 4)
      (= (->gdx-input-key :shift-left) 59))
 )

; missing button-pressed?
; also not explaining just-pressed or pressed docs ...
; always link the java class (for all stuff?)
; https://javadoc.io/static/com.badlogicgames.gdx/gdx/1.12.1/com/badlogic/gdx/Input.html#isButtonPressed(int)

(defn button-just-pressed?
  ":left, :right, :middle, :back or :forward."
  [b]
  (.isButtonJustPressed Gdx/input (->gdx-input-button b)))

(defn key-just-pressed?
  "See [[key-pressed?]]."
  [k]
  (.isKeyJustPressed Gdx/input (->gdx-input-key k)))

(defn key-pressed?
  "For options see [libgdx Input$Keys docs](https://javadoc.io/doc/com.badlogicgames.gdx/gdx/latest/com/badlogic/gdx/Input.Keys.html).
  Keys are upper-cased and dashes replaced by underscores.
  For example Input$Keys/ALT_LEFT can be used with :alt-left.
  Numbers via :num-3, etc."
  [k]
  (.isKeyPressed Gdx/input (->gdx-input-key k)))

(defn- set-input-processor! [processor]
  (.setInputProcessor Gdx/input processor))
