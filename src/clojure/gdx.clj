(ns clojure.gdx
  {:metadoc/categories {:app "🖥️ Application"
                        :app.graphics "🎨 Graphics"
                        :app.graphics.camera "🎥 Camera"
                        :app.graphics.images "🖼️ Image"
                        :app.screen "📺 Screens"
                        :app.view.gui "🖼️ Gui View"
                        :app.view.world "🗺️ World View"
                        :app.input "🎮 Input"
                        :component "⚙️ Component"
                        :ctx "📜 Context"
                        :effects "💥 Effects"
                        :entity "👾 Entity"
                        :geometry "📐 Geometry"
                        :properties "📦️ Properties"
                        :ui "🎛️ UI"
                        :ui-actor "🕴️ UI Actor"
                        :ui-stage "UI Stage"
                        :ui-table "UI Table"
                        :utils "🔧 Utils"
                        :utils.random "🎲 Random"
                        :world "🌎 World"
                        :world.timer "⏳ Timer"}}
  (:require (clojure [set :as set]
                     [string :as str]
                     [edn :as edn]
                     [math :as math]
                     [pprint :refer [pprint]])
            [clojure.gdx.tiled :as t]
            [clj-commons.pretty.repl :refer [pretty-pst]]
            [data.grid2d :as g]
            (malli [core :as m]
                   [error :as me]
                   [generator :as mg]))
  (:import (com.badlogic.gdx Gdx ApplicationAdapter)
           com.badlogic.gdx.assets.AssetManager
           com.badlogic.gdx.audio.Sound
           com.badlogic.gdx.files.FileHandle
           (com.badlogic.gdx.graphics Color Colors Texture Texture$TextureFilter OrthographicCamera Camera Pixmap Pixmap$Format)
           (com.badlogic.gdx.graphics.g2d SpriteBatch Batch BitmapFont TextureRegion)
           (com.badlogic.gdx.graphics.g2d.freetype FreeTypeFontGenerator FreeTypeFontGenerator$FreeTypeFontParameter)
           (com.badlogic.gdx.math MathUtils Vector2 Vector3 Circle Rectangle Intersector)
           (com.badlogic.gdx.utils Align Scaling SharedLibraryLoader ScreenUtils Disposable)
           (com.badlogic.gdx.utils.viewport Viewport FitViewport)
           (com.badlogic.gdx.backends.lwjgl3 Lwjgl3Application Lwjgl3ApplicationConfiguration)
           (com.badlogic.gdx.scenes.scene2d Actor Touchable Group Stage)
           (com.badlogic.gdx.scenes.scene2d.ui Widget Image Label Button Table Cell WidgetGroup Stack ButtonGroup HorizontalGroup VerticalGroup Window Tree$Node)
           (com.badlogic.gdx.scenes.scene2d.utils ClickListener ChangeListener TextureRegionDrawable Drawable)
           (com.kotcrab.vis.ui VisUI VisUI$SkinScale)
           (com.kotcrab.vis.ui.widget Tooltip VisTextButton VisCheckBox VisSelectBox VisImage VisImageButton VisTextField VisWindow VisTable VisLabel VisSplitPane VisScrollPane Separator VisTree)
           space.earlygrey.shapedrawer.ShapeDrawer
           gdl.RayCaster)
  (:load "gdx/math"
         "gdx/utils"
         "gdx/component"
         "gdx/properties"

         "gdx/app/assets"
         "gdx/app/input"
         "gdx/app/graphics"
         "gdx/app/screens"
         "gdx/app/ui"
         "gdx/app/start"

         "gdx/screens/property_editor"
         "gdx/world"

         "gdx/entity/base"
         "gdx/entity/image"
         "gdx/entity/animation"
         "gdx/entity/movement"
         "gdx/entity/delete_after_duration"
         "gdx/entity/destroy_audiovisual"
         "gdx/entity/line"
         "gdx/entity/projectile"
         "gdx/entity/skills"
         "gdx/entity/faction"
         "gdx/entity/clickable"
         "gdx/entity/mouseover"
         "gdx/entity/temp_modifier"
         "gdx/entity/alert"
         "gdx/entity/string_effect"
         "gdx/entity/modifiers"
         "gdx/entity/inventory"
         "gdx/entity/state"

         "gdx/doc"))
