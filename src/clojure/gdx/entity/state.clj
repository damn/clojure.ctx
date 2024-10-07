(in-ns 'clojure.gdx)

(defsystem enter "FIXME" [_])
(defmethod enter :default [_])

(defsystem exit  "FIXME" [_])
(defmethod exit :default  [_])

(defsystem player-enter "FIXME" [_])
(defmethod player-enter :default [_])

(defsystem pause-game? "FIXME" [_])
(defmethod pause-game? :default [_])

(defsystem manual-tick "FIXME" [_])
(defmethod manual-tick :default [_])

(defsystem clicked-inventory-cell "FIXME" [_ cell])
(defmethod clicked-inventory-cell :default [_ cell])

(defsystem clicked-skillmenu-skill "FIXME" [_ skill])
(defmethod clicked-skillmenu-skill :default [_ skill])
