(ns game-state.elapsed-time
  (:require [api.context :as ctx]))

(defn- elapsed-time [ctx]
  (:context.game/elapsed-time ctx))

(defrecord ImmutableCounter [duration stop-time])

(extend-type api.context.Context
  api.context/Counter
  (->counter [ctx duration]
    {:pre [(>= duration 0)]}
    (->ImmutableCounter duration (+ (elapsed-time ctx) duration)))

  (stopped? [ctx {:keys [stop-time]}]
    (>= (elapsed-time ctx) stop-time))

  (reset [ctx {:keys [duration] :as counter}]
    (assoc counter :stop-time (+ (elapsed-time ctx) duration)))

  (finished-ratio [ctx {:keys [duration stop-time] :as counter}]
    {:post [(<= 0 % 1)]}
    (if (ctx/stopped? ctx counter)
      0
      ; min 1 because floating point math inaccuracies
      (min 1 (/ (- stop-time (elapsed-time ctx)) duration)))))

(defn update-time [ctx]
  (update ctx :context.game/elapsed-time + (:context.game/delta-time ctx)))
